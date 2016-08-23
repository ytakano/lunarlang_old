#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include "lunar_common.hpp"
#include "lunar_bytestream.hpp"
#include "lunar_string.hpp"
#include "lunar_ringq.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_hash.hpp"

#include <string>
#include <vector>

namespace lunar {

template <typename T>
class parsec {
public:
    typedef bytestream<T>             bytes_t;
    typedef std::basic_string<T>      string_t;
    typedef std::unique_ptr<string_t> ptr_string_t;

    struct message {
        STRM_RESULT result;
        uint64_t    line;
        uint64_t    col;
    };

    class parser_space {
    public:
        parser_space(const hash_set<T> &spaces) : m_spaces(spaces) { }

        bool operator() (T c)
        {
            auto it = m_spaces.find(c);
            if (it != m_spaces.end())
                return true;

            return false;
        }

    private:
        const hash_set<T> &m_spaces;
    };

    class parser_char {
    public:
        parser_char(T c) : m_char(c) { }

        bool operator() (T c) { return c == m_char; }

    private:
        T m_char;
    };

    class parser_digit {
    public:
        bool operator() (T c) { return (T)'0' <= c && c <= (T)'9'; }
    };

    class parser_hex_digit {
    public:
        bool operator() (T c)
        {
            return ((T)'0' <= c && c <= (T)'9') ||
                   ((T)'a' <= c && c <= (T)'f') ||
                   ((T)'A' <= c && c <= (T)'F');
        }
    };

    class parser_oct_digit {
    public:
        bool operator() (T c)
        {
            return (T)'0' <= c && c <= (T)'7';
        }
    };

    T parser_satisfy(parsec &ps, std::function<bool(T)> func)
    {
        T c;
        for (;;) {
            auto result = ps.m_bytes.front(c);

            if (result == STRM_SUCCESS) {
                break;
            } else if (result == STRM_NO_MORE_DATA) {
                string_t *ptr;
                auto result2 = pop_stream_ptr(ps.m_shared_stream, (void**)&ptr);
                if (result2 == STRM_SUCCESS) {
                    ps.m_bytes.push_back(ptr);
                } else if (result2 == STRM_CLOSED) {
                    ps.m_bytes.push_eof();
                } else {
                    select_green_thread(nullptr, 0, (void**)&ps.m_shared_stream, 1, false, 0);
                }
            } else {
                ps.m_is_eof = true;
                ps.m_is_result = false;
                ps.set_err(result, ps.m_line, ps.m_col);

                return 0;
            }
        }

        if (func(c)) {
            ps.m_is_result = true;
            ps.m_num++;

            if (c == (T)'\n') {
                ps.m_line++;
                ps.m_col = 1;
            } else {
                ps.m_col++;
            }

            if (ps.m_is_look_ahead || ps.m_is_try) {
                ps.m_bytes.move_tmp_pos(1);
            } else {
                ps.m_bytes.consume(1);
            }

            return c;
        }

        ps.m_is_result = false;
        ps.set_err(STRM_SUCCESS, ps.m_line, ps.m_col);

        return c;
    }

    class parser_try {
    public:
        parser_try(parsec &p) : m_parsec(p) {
            m_col    = m_parsec.m_col;
            m_line   = m_parsec.m_line;
            m_num    = m_parsec.m_num;
            m_pos    = m_parsec.m_bytes.get_tmp_pos();
            m_is_try = m_parsec.m_is_try;
            m_is_eof = m_parsec.m_is_eof;

            m_parsec.m_is_try = true;
        }

        virtual ~parser_try() {
            if (m_parsec.m_is_result) {
                if (! m_is_try && ! m_parsec.m_is_look_ahead) {
                   	auto n = m_parsec.m_num - m_num;
                    m_parsec.m_bytes.restore_tmp_pos(m_pos);
                    m_parsec.m_bytes.consume(n);
                }

                m_parsec.m_is_try = m_is_try;
            } else {
                m_parsec.m_col    = m_col;
                m_parsec.m_line   = m_line;
                m_parsec.m_num    = m_num;
                m_parsec.m_is_try = m_is_try;
                m_parsec.m_is_eof = m_is_eof;
                m_parsec.m_bytes.restore_tmp_pos(m_pos);
            }
        }

    private:
        parsec   &m_parsec;
        uint64_t  m_col, m_line, m_num;
        point2u64 m_pos;
        bool      m_is_try;
        bool      m_is_eof;
    };

    class parser_look_ahead {
    public:
        parser_look_ahead(parsec &p) : m_parsec(p) {
            m_col  = m_parsec.m_col;
            m_line = m_parsec.m_line;
            m_num  = m_parsec.m_num;
            m_pos  = m_parsec.m_bytes.get_tmp_pos();
            m_is_look_ahead = m_parsec.m_is_look_ahead;
            m_is_eof = m_parsec.m_is_eof;

            m_parsec.m_is_look_ahead = true;
        }

        virtual ~parser_look_ahead() {
            m_parsec.m_col  = m_col;
            m_parsec.m_line = m_line;
            m_parsec.m_num  = m_num;
            m_parsec.m_is_look_ahead = m_is_look_ahead;
            m_parsec.m_is_eof = m_is_eof;
            m_parsec.m_bytes.restore_tmp_pos(m_pos);
         }

    private:
        parsec   &m_parsec;
        uint64_t  m_col, m_line, m_num;
        point2u64 m_pos;
        bool      m_is_look_ahead;
        bool      m_is_eof;
    };

    bool parser_string(parsec &ps, const T *str)
    {
        while (*str != 0) {
            ps.character(*str);
            if (ps.is_success()) {
                str++;
            } else {
                return false;
            }
        }

        return true;
    }

    template<typename RT>
    class parser_many {
    public:
        parser_many(parsec &p, std::function<RT()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many() { }

        std::unique_ptr<std::vector<RT>> operator() () {
            auto ret = llvm::make_unique(std::vector<RT>());

            for (;;) {
                parser_try ptry(m_parsec);

                auto val = m_func();

                if (m_parsec.m_is_result) {
                    ret->push_back(val);
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<RT()> m_func;
    };

    template<typename RT>
    class parser_many1 {
    public:
        parser_many1(parsec &p, std::function<RT()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many1() { }

        std::unique_ptr<std::vector<RT>> operator() () {
            auto ret = llvm::make_unique(std::vector<RT>());

            auto val = m_func();
            if (! m_parsec.m_is_result)
                return ret;

            ret.push_back(val);

            for (;;) {
                parser_try ptry(m_parsec);

                auto val = m_func();

                if (m_parsec.m_is_result) {
                    ret->push_back(val);
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<RT()> m_func;
    };

    parsec(shared_stream *s)
        : m_shared_stream(s),
          m_is_result(true),
          m_is_eof(false),
          m_col(1),
          m_line(1),
          m_num(0),
          m_is_look_ahead(false),
          m_is_try(false)
    {
        m_spaces.insert((T)U'\u0009');
        m_spaces.insert((T)U'\u000A');
        m_spaces.insert((T)U'\u000B');
        m_spaces.insert((T)U'\u000C');
        m_spaces.insert((T)U'\u000D');
        m_spaces.insert((T)U'\u001C');
        m_spaces.insert((T)U'\u001D');
        m_spaces.insert((T)U'\u001E');
        m_spaces.insert((T)U'\u001F');
        m_spaces.insert((T)U'\u0020');
        m_spaces.insert((T)U'\u00A0');

        if (sizeof(T) == 1)
            return;

        m_spaces.insert((T)U'\u11A3');
        m_spaces.insert((T)U'\u11A4');
        m_spaces.insert((T)U'\u11A5');
        m_spaces.insert((T)U'\u11A6');
        m_spaces.insert((T)U'\u11A7');
        m_spaces.insert((T)U'\u1689');
        m_spaces.insert((T)U'\u2000');
        m_spaces.insert((T)U'\u2001');
        m_spaces.insert((T)U'\u2002');
        m_spaces.insert((T)U'\u2003');
        m_spaces.insert((T)U'\u2004');
        m_spaces.insert((T)U'\u2005');
        m_spaces.insert((T)U'\u2006');
        m_spaces.insert((T)U'\u2007');
        m_spaces.insert((T)U'\u2008');
        m_spaces.insert((T)U'\u2009');
        m_spaces.insert((T)U'\u200A');
        m_spaces.insert((T)U'\u200B');
        m_spaces.insert((T)U'\u202F');
        m_spaces.insert((T)U'\u205F');
        m_spaces.insert((T)U'\u2060');
        m_spaces.insert((T)U'\u3000');
        m_spaces.insert((T)U'\u3164');
        m_spaces.insert((T)U'\uFEFF');
    }

    virtual ~parsec() { }

    void set_err(STRM_RESULT result, int line, int col)
    {
        m_err.result = result;
        m_err.line   = line;
        m_err.col    = col;
    }

    bool parse_eof()
    {
        parser_satisfy(*this, [](char32_t c) { return true; });
        return m_is_eof;
    }

    T satisfy(std::function<bool(T)> f)
    {
        return parser_satisfy(*this, f);
    }

    T character(T c) {
        return parser_satisfy(*this, parser_char(c));
    }

    bool parse_string(const T *str) {
        return parser_string(*this, str);
    }

    T parse_space() {
        return parser_satisfy(*this, parser_space(m_spaces));
    }

    T parse_digit() {
        return parser_satisfy(*this, parser_digit());
    }

    T parse_hex_digit() {
        return parser_satisfy(*this, parser_hex_digit());
    }

    T parse_oct_digit() {
        return parser_satisfy(*this, parser_oct_digit());
    }

    template<typename RT>
    std::unique_ptr<std::vector<RT>>parse_many(std::function<RT()> func) {
        return parser_many<RT>(*this, func)();
    }

    string_t parse_many_char(std::function<T()> func) {
        return parser_many<T>(*this, func)();
    }

    template<typename RT>
    std::unique_ptr<std::vector<RT>> parse_many1(std::function<RT()> func) {
        return parser_many1<RT>(*this, func)();
    }

    string_t parse_many1_char(std::function<T()> func) {
        return parser_many1<T>(*this, func)();
    }

    bool is_success() { return m_is_result; }
    bool is_eof() { return m_is_eof; }
    void set_is_success(bool val) { m_is_result = val; }
    message get_errmsg() { return m_err; }
    uint64_t get_line() { return m_line; }
    uint64_t get_col() { return m_col; }

    void set_nodel()
    {
        m_bytes.set_del_func([](string_t *ptr) { });
    }

private:
    shared_stream *m_shared_stream;
    hash_set<T> m_spaces;
    bytes_t  m_bytes;
    message  m_err;
    bool     m_is_result;
    bool     m_is_eof;
    uint64_t m_col;
    uint64_t m_line;
    uint64_t m_num;
    bool     m_is_look_ahead;
    bool     m_is_try;
};

template<>
template<>
class parsec<char>::parser_many<char> {
    public:
        parser_many(parsec &p, std::function<char()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many() { }

        std::string operator() () {
            std::string ret;

            for (;;) {
                parser_try ptry(m_parsec);

                auto c = m_func();

                if (m_parsec.m_is_result) {
                    ret += c;
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<char()> m_func;
};

template<>
template<>
class parsec<char32_t>::parser_many<char32_t> {
    public:
        parser_many(parsec &p, std::function<char32_t()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many() { }

        parsec<char32_t>::string_t operator() () {
            parsec<char32_t>::string_t ret;

            for (;;) {
                parser_try ptry(m_parsec);

                auto c = m_func();

                if (m_parsec.m_is_result) {
                    ret += c;
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<char32_t()> m_func;
};

template<>
template<>
class parsec<char>::parser_many1<char> {
    public:
        parser_many1(parsec &p, std::function<char()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many1() { }

        std::string operator() () {
            std::string ret;

            auto c = m_func();
            if (! m_parsec.m_is_result)
                return ret;

            ret += c;

            for (;;) {
                parser_try ptry(m_parsec);

                auto c = m_func();

                if (m_parsec.m_is_result) {
                    ret += c;
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<char()> m_func;
};

template<>
template<>
class parsec<char32_t>::parser_many1<char32_t> {
    public:
        parser_many1(parsec &p, std::function<char32_t()> func) : m_parsec(p), m_func(func) { }

        virtual ~parser_many1() { }

        parsec<char32_t>::string_t operator() () {
            parsec<char32_t>::string_t ret;

            auto c = m_func();
            if (! m_parsec.m_is_result)
                return ret;

            ret += c;

            for (;;) {
                parser_try ptry(m_parsec);

                auto c = m_func();

                if (m_parsec.m_is_result) {
                    ret += c;
                } else {
                    break;
                }
            }

            m_parsec.m_is_result = true;

            return ret;
        }

    private:
        parsec &m_parsec;
        std::function<char32_t()> m_func;
};

}

#endif // LUNAR_PARSEC_HPP
#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include "lunar_common.hpp"
#include "lunar_bytestream.hpp"
#include "lunar_string.hpp"
#include "lunar_ringq.hpp"
#include "lunar_green_thread.hpp"

#include <string>
#include <unordered_set>
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
        int         line;
        int         col;
    };

    struct char_t {
        T    m_char;
        bool m_is_result;
        
        operator bool() const { return m_is_result; }
    };
    
    class parser_space {
    public:
        parser_space(const std::unordered_set<T> &spaces) : m_spaces(spaces) { }

        char_t operator() (T c)
        {
            char_t ret;
            auto it = m_spaces.find(c);
            if (c != m_spaces.end()) {
                ret.m_is_result = true;
                ret.m_char = c;
            } else {
                ret.m_is_result = false;
            }
            
            return ret;
        }
    
    private:
        const std::unordered_set<T> &m_spaces;
    };
    
    class parser_char {
    public:
        parser_char(T c) : m_char(c) { }
        
        char_t operator() (T c)
        {
            char_t ret;
            if (c == m_char) {
                ret.m_is_result = true;
                ret.m_char = c;
            } else {
                ret.m_is_result = false;
            }
            
            return ret;
        }
    
    private:
        T m_char;
    };

    class parser_digit {
    public:
        char_t operator() (T c)
        {
            return {c, (T)'0' <= c && c <= (T)'9'};
        }
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
    
    class parser_satisfy {
    public:
        parser_satisfy(parsec &p, std::function<char_t(T)> func) : m_parsec(p), m_func(func) { }
        virtual ~parser_satisfy() { }

        char_t operator() () {
            T c;
            for (;;) {
                auto result = m_parsec.m_bytes.front(c);

                if (result == STRM_SUCCESS) {
                    break;
                } else if (result == STRM_NO_MORE_DATA) {
                    string_t *ptr;
                    auto result2 = pop_ptr(m_parsec.m_shared_stream, (void**)&ptr);
                    if (result2 == STRM_SUCCESS) {
                        m_parsec.m_bytes.push_back(ptr);
                    } else if (result2 == STRM_CLOSED) {
                        m_parsec.m_bytes.push_eof();
                    } else {
                        select_green_thread(nullptr, 0, (void**)&m_parsec.m_shared_stream, 1, false, 0);
                    }
                } else {
                    m_parsec.m_is_result = false;
                    m_parsec.set_err(result, m_parsec.m_line, m_parsec.m_col);

                    char_t ret;
                    ret.m_is_result = false;
                    return ret;
                }
            }
            
            auto ret = m_func(c);
            if (ret) {
                m_parsec.m_is_result = true;
                m_parsec.m_num++;
                
                if (c == (T)'\n') {
                    m_parsec.m_line++;
                    m_parsec.m_col = 1;
                } else {
                    m_parsec.m_col++;
                }
                
                if (m_parsec.m_is_look_ahead || m_parsec.m_is_try) {
                    m_parsec.m_bytes.move_tmp_pos(1);
                } else {
                    m_parsec.m_bytes.consume(1);
                }
                
                return ret;
            }
            
            m_parsec.m_is_result = false;
            m_parsec.set_err(STRM_SUCCESS, m_parsec.m_line, m_parsec.m_col);
            
            return ret;
        }
    
    private:
        std::function<char_t(T)> m_func;
        parsec &m_parsec;
    };

    class parser_try {
    public:
        parser_try(parsec &p) : m_parsec(p) {
            m_col    = m_parsec.m_col;
            m_line   = m_parsec.m_line;
            m_num    = m_parsec.m_num;
            m_pos    = m_parsec.m_bytes.get_tmp_pos();
            m_is_try = m_parsec.m_is_try;
            
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
                m_parsec.m_bytes.restore_tmp_pos(m_pos);
            }
        }
    
    private:
        parsec   &m_parsec;
        uint64_t  m_col, m_line, m_num;
        point2u64 m_pos;
        bool      m_is_try;
    };
    
    class parser_look_ahead {
    public:
        parser_look_ahead(parsec &p) : m_parsec(p) {
            m_col  = m_parsec.m_col;
            m_line = m_parsec.m_line;
            m_num  = m_parsec.m_num;
            m_pos  = m_parsec.m_bytes.get_tmp_pos();
            m_is_look_ahead = m_parsec.m_is_look_ahead;
            
            m_parsec.m_is_look_ahead = true;
        }

        virtual ~parser_look_ahead() {
            m_parsec.m_col  = m_col;
            m_parsec.m_line = m_line;
            m_parsec.m_num  = m_num;
            m_parsec.m_is_look_ahead = m_is_look_ahead;
            m_parsec.m_bytes.restore_tmp_pos(m_pos);
         }
    
    private:
        parsec   &m_parsec;
        uint64_t  m_col, m_line, m_num;
        point2u64 m_pos;
        bool      m_is_look_ahead;
    };
    
    class parser_string {
    public:
        parser_string(parsec &p, const T *str) : m_parsec(p), m_str(str) { }

        virtual ~parser_string() { }
        
        string_t* operator() () {
            string_t *ret = new string_t;
            
            while (*m_str != 0) {
                auto c = m_parsec.character(*m_str)();
                if (c) {
                    ret->push_back(c.m_char);
                } else {
                    delete ret;
                    return nullptr;
                }
            }
            
            return ret;
        }
        
    private:
        parsec  &m_parsec;
        const T *m_str;
    };
    
    template<typename RT>
    class parser_many {
    public:
        parser_many(parsec &p, std::function<RT()> func) : m_parsec(p), m_func(func) { }
        
        virtual ~parser_many() { }
        
        std::vector<RT>* operator() () {
            std::vector<RT> *ret = new std::vector<RT>;
            
            for (;;) {
                parser_try ptry(m_parsec);
                if (m_parsec.get_) {
                    
                }
            }
        }

    private:
        parsec &m_parsec;
        std::function<RT()> m_func;
    };
    
    parsec(shared_stream *s)
        : m_shared_stream(s),
          m_is_result(true),
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
    
    parser_satisfy satisfy(std::function<char_t(T)> f)
    {
        return parser_satisfy(*this, f);
    }
    
    parser_satisfy character(T c) {
        return parser_satisfy(*this, parser_char(c));
    }
    
    parser_string parse_string(const T *str) {
        return parser_string(str);
    }
    
    parser_satisfy parse_space() {
        return parser_satisfy(*this, parser_space(m_spaces));
    }
    
    parser_satisfy parse_digit() {
        return parser_satisfy(*this, parser_digit());
    }
    
    parser_satisfy parse_hex_digit() {
        return parser_satisfy(*this, parser_hex_digit());
    }
    
    parser_satisfy parse_oct_digit() {
        return parser_satisfy(*this, parser_oct_digit());
    }
    
    bool get_is_result() { return m_is_result; }
    void set_is_result(bool is_result) { m_is_result = is_result; }

private:
    shared_stream *m_shared_stream;
    std::unordered_set<T> m_spaces;
    bytes_t  m_bytes;
    message  m_err;
    bool     m_is_result;
    uint64_t m_col;
    uint64_t m_line;
    uint64_t m_num;
    bool     m_is_look_ahead;
    bool     m_is_try;
};

}

#endif // LUNAR_PARSEC_HPP
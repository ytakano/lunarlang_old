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
    typedef bytestream<T>         stream_t;
    typedef std::unordered_set<T> chars_t;
    typedef std::basic_string<T>  string_t;

    parsec(shared_stream s)
        : m_shared_stream(s),
          m_col(1),
          m_line(1),
          m_num(0),
          m_result(true),
          m_is_look_ahead(false),
          m_is_try(false)
    {
        m_err.result = SUCCESS;
        m_err.line   = 1;
        m_err.col    = 1;
        
        if (p_space == nullptr) {
            if (__sync_lock_test_and_set(&lock, 1)) {
                while (lock);
            } else {
                init_space();
                __sync_lock_release(&lock);
            }
        }
    }

    virtual ~parsec() { }
    
    static void make_chars(chars_t &chars, const string_t &str)
    {
        for (auto &c: str)
            chars.insert(c);
    }
    
    static void init_space()
    {
        p_space = new chars_t;
        
        p_space->insert((T)U'\u0009');
        p_space->insert((T)U'\u000A');
        p_space->insert((T)U'\u000B');
        p_space->insert((T)U'\u000C');
        p_space->insert((T)U'\u000D');
        p_space->insert((T)U'\u001C');
        p_space->insert((T)U'\u001D');
        p_space->insert((T)U'\u001E');
        p_space->insert((T)U'\u001F');
        p_space->insert((T)U'\u0020');
        p_space->insert((T)U'\u00A0');
        
        if (sizeof(T) == 1)
            return;
        
        p_space->insert((T)U'\u11A3');
        p_space->insert((T)U'\u11A4');
        p_space->insert((T)U'\u11A5');
        p_space->insert((T)U'\u11A6');
        p_space->insert((T)U'\u11A7');
        p_space->insert((T)U'\u1689');
        p_space->insert((T)U'\u2000');
        p_space->insert((T)U'\u2001');
        p_space->insert((T)U'\u2002');
        p_space->insert((T)U'\u2003');
        p_space->insert((T)U'\u2004');
        p_space->insert((T)U'\u2005');
        p_space->insert((T)U'\u2006');
        p_space->insert((T)U'\u2007');
        p_space->insert((T)U'\u2008');
        p_space->insert((T)U'\u2009');
        p_space->insert((T)U'\u200A');
        p_space->insert((T)U'\u200B');
        p_space->insert((T)U'\u202F');
        p_space->insert((T)U'\u205F');
        p_space->insert((T)U'\u2060');
        p_space->insert((T)U'\u3000');
        p_space->insert((T)U'\u3164');
        p_space->insert((T)U'\uFEFF');
    }

    struct message {
        read_result result;
        int         line;
        int         col;
    };

    class parser_chain;
    class parser_or;
    
    class parser {
    public:
        parser(parsec &p) : m_parsec(p) { }
        virtual ~parser() { }

        virtual bool operator() () { return m_parsec.m_result; };

        parsec::parser_chain operator>> (parsec::parser &&p)
        {
            return parsec::parser_chain(this, &p);
        }

        parsec::parser_chain operator>> (parsec::parser &p)
        {
            return parsec::parser_chain(this, &p);
        }

        parsec::parser_or operator|| (parsec::parser &&p)
        {
            return parsec::parser_or(this, &p);
        }

        parsec::parser_or operator|| (parsec::parser &p)
        {
            return parsec::parser_or(this, &p);
        }
    
    public:
        parsec &m_parsec;
    };
    
    class parser_func : public parser {
    public:
        parser_func(parsec &p, std::function<bool(parsec&)> func)
            : parser(p), m_func(func) { }
        virtual ~parser_func() { }
        
        virtual bool operator() () { return m_func(parser::m_parsec); }
    
    private:
        std::function<bool(parsec&)> m_func;
    };
    
    class parser_chain : public parser {
    public:
        parser_chain(parser *lhs, parser *rhs)
            : parser(lhs->m_parsec), m_lhs(lhs), m_rhs(rhs)
        {
            assert(&lhs->m_parsec == &rhs->m_parsec);
        }
        virtual ~parser_chain() { }
        
        virtual bool operator() ()
        {
            (*m_lhs)();
            if (parser::m_parsec.m_result) {
                (*m_rhs)();
            }
            
            return parser::m_parsec.m_result;
        }
        
    private:
        parser *m_lhs, *m_rhs;
    };
    
    class parser_or : public parser {
    public:
        parser_or(parser *lhs, parser *rhs)
            : parser(lhs->m_parsec), m_lhs(lhs), m_rhs(rhs)
        {
            assert(&lhs->m_parsec == &rhs->m_parsec);
        }
        virtual ~parser_or() { }
        
        virtual bool operator() ()
        {
            (*m_lhs)();
            if (! parser::m_parsec.m_result) {
                (*m_rhs)();
            }
            
            return parser::m_parsec.m_result;
        }
        
    private:
        parser *m_lhs, *m_rhs;
    };
    
    class parser_satisfy : public parser {
    public:
        parser_satisfy(parsec &p, std::function<bool(T)> func) : parser(p), m_func(func) { }
        virtual ~parser_satisfy() { }
        
        virtual bool operator() ()
        {
            T c;
            printf("here\n");
            for (;;) {
                auto result = parser::m_parsec.m_stream.front(c);

                if (result == SUCCESS) {
                    printf("success: c = %c\n", (char)c);
                    break;
                } else if (result == NO_MORE_DATA) {
                    printf("no more data\n");
                    std::u32string *ptr;
                    auto result2 = pop_string(&parser::m_parsec.m_shared_stream, &ptr);
                    if (result2 == SUCCESS) {
                        parser::m_parsec.m_stream.push_back(ptr);
                    } else if (result2 == END_OF_STREAM) {
                        parser::m_parsec.m_stream.push_eof();
                    }
                    
                    assert(result2 != NO_MORE_DATA);
                } else {
                    parser::m_parsec.m_result = false;
                    parser::m_parsec.set_err(result, parser::m_parsec.m_line, parser::m_parsec.m_col);
                    return false;
                }
            }

            if (m_func(c)) {
                parser::m_parsec.m_result = true;
                parser::m_parsec.m_num++;
                parser::m_parsec.m_str.push_back(c);
                
                if (c == (T)'\n') {
                    parser::m_parsec.m_line++;
                    parser::m_parsec.m_col = 1;
                } else {
                    parser::m_parsec.m_col++;
                }
                
                if (parser::m_parsec.m_is_look_ahead || parser::m_parsec.m_is_try) {
                    parser::m_parsec.m_stream.move_tmp_pos(1);
                } else {
                    parser::m_parsec.m_stream.consume(1);
                }
                
                return true;
            }
            
            parser::m_parsec.m_result = false;
            parser::m_parsec.set_err(SUCCESS, parser::m_parsec.m_line, parser::m_parsec.m_col);
            
            return false;
        }
    
    private:
        std::function<bool(T)> m_func;
    };
    
    class parser_one_of {
    public:
        parser_one_of(const chars_t &chars) : m_chars(chars) { }
    
        bool operator() (T c)
        {
            return m_chars.find(c) != m_chars.end();
        }
    
    private:
        chars_t m_chars;
    };
    
    class parser_char {
    public:
        parser_char(T c) : m_char(c) { }
        
        bool operator() (T c)
        {
            return c == m_char;
        }
    
    private:
        T m_char;
    };
    
    class parser_none_of {
    public:
        parser_none_of(const chars_t &chars) : m_chars(chars) { }
    
        bool operator() (T c)
        {
            return m_chars.find(c) == m_chars.end();
        }
    
    private:
        chars_t m_chars;
    };
    
    class parser_digit {
    public:
        bool operator() (T c)
        {
            return (T)'0' <= c && c <= (T)'9';
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

    class parser_many : public parser {
    public:
        parser_many(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_many() { }
        
        virtual bool operator() ()
        {
            for (;;) {
                m_parser();
                if (! parser::m_parsec.m_result)
                    break;
            }
            
            parser::m_parsec.m_result = true;
            return true;
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_many1 : public parser {
    public:
        parser_many1(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_many1() { }
        
        virtual bool operator() ()
        {
            m_parser();
            if (! parser::m_parsec.m_result)
                return false;

            for (;;) {
                m_parser();
                if (! parser::m_parsec.m_result)
                    break;
            }
            
            parser::m_parsec.m_result = true;
            return true;
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_string : public parser {
    public:
        parser_string(parsec &p, const string_t &str) : parsec(p), m_str(str) { }
        virtual ~parser_string() { }
        
        virtual bool operator() ()
        {
            for (auto &s: m_str) {
                parser::m_parsec.character(s)();
                if (! parser::m_parsec.m_result) {
                    return false;
                }
            }
            
            parser::m_parsec.m_result = true;
            return true;
        }
    
    private:
        const string_t &m_str;
    };
    
    class parser_try : public parser {
    public:
        parser_try(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_try() { }
        
        virtual bool operator() ()
        {
            auto col    = parser::m_parsec.m_col;
            auto line   = parser::m_parsec.m_line;
            auto num    = parser::m_parsec.m_num;
            auto pos    = parser::m_parsec.m_stream.get_tmp_pos();
            auto is_try = parser::m_parsec.m_is_try;
            auto str    = parser::m_parsec.m_str;
            
            parser::m_parsec.m_is_try = true;
            
            m_parser();
            
            if (parser::m_parsec.m_result) {
                if (! is_try && ! parser::m_parsec.m_is_look_ahead) {
                   	int n = parser::m_parsec.m_num - num;
                    parser::m_parsec.m_stream.restore_tmp_pos(pos);
                    parser::m_parsec.m_stream.consume(n);
                }
                
                parser::m_parsec.m_is_try = is_try;
            } else {
                parser::m_parsec.m_col    = col;
                parser::m_parsec.m_line   = line;
                parser::m_parsec.m_num    = num;
                parser::m_parsec.m_is_try = is_try;
                parser::m_parsec.m_str    = str;
                parser::m_parsec.m_stream.restore_tmp_pos(pos);
            }
            
            return parser::m_parsec.m_result;
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_look_ahead : public parser {
    public:
        parser_look_ahead(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_look_ahead() { }
        
        virtual bool operator() ()
        {
            auto col    = parser::m_parsec.m_col;
            auto line   = parser::m_parsec.m_line;
            auto num    = parser::m_parsec.m_num;
            auto pos    = parser::m_parsec.m_stream.get_tmp_pos();
            auto is_lah = parser::m_parsec.m_is_look_ahead;
            auto str    = parser::m_parsec.m_str;
            
            parser::m_parsec.m_is_look_ahead = true;
            
            m_parser();
            
            parser::m_parsec.m_col  = col;
            parser::m_parsec.m_line = line;
            parser::m_parsec.m_num  = num;
            parser::m_parsec.m_is_look_ahead = is_lah;
            parser::m_parsec.m_stream.restore_tmp_pos(pos);
            
            if (! parser::m_parsec.m_result)
                parser::m_parsec.m_str = str;
            
            return parser::m_parsec.m_result;
        }
    
    private:
        parser &m_parser;
    };
    
    parser_satisfy satisfy(std::function<bool(T)> f)
    {
        return parser_satisfy(*this, f);
    }
    
    parser_satisfy one_of(const chars_t &chars)
    {
        return parser_satisfy(*this, parser_one_of(chars));
    }

    parser_satisfy character(T c)
    {
        return parser_satisfy(*this, parser_char(c));
    }

    parser_satisfy none_of(const chars_t &chars)
    {
        return parser_satisfy(*this, chars);
    }
    
    parser_satisfy digit()
    {
        return parser_satisfy(*this, parser_digit());
    }
    
    parser_satisfy hex_digit()
    {
        return parser_satisfy(*this, parser_hex_digit());
    }
    
    parser_satisfy oct_digit()
    {
        return parser_satisfy(*this, parser_oct_digit());
    }

    parser_satisfy space()
    {
        return one_of(*p_space);
    }
    
    parser_many many(parser &&p)
    {
        return parser_many(*this, p);
    }
    
    parser_many many(parser &p)
    {
        return parser_many(*this, p);
    }
    
    parser_try try_parse(parser &&p)
    {
        return parser_try(*this, p);
    }
    
    parser_try try_parse(parser &p)
    {
        return parser_try(*this, p);
    }
    
    parser_look_ahead look_ahead(parser &&p)
    {
        return parser_look_ahead(*this, p);
    }
    
    parser_look_ahead look_ahead(parser &p)
    {
        return parser_look_ahead(*this, p);
    }
    
    parser_func func(std::function<void(parsec&)> f)
    {
        return parser_func(*this, f);
    }
    
    const message & get_err() { return m_err; }

    const string_t & get_string() { return m_str; }
    void set_string(string_t &str) { m_str = str; }
    void clear_string() { m_str.clear(); }

    bool get_result() { return m_result;}
    int  get_line() { return m_line; }
    int  get_col() {return m_col; }
    
    void set_err(read_result result, int line, int col)
    {
        m_err.result = result;
        m_err.line   = line;
        m_err.col    = col;
    }

private:
    static chars_t *p_space;
    volatile static int lock;

    shared_stream m_shared_stream;
    stream_t m_stream;
    string_t m_str;
    message  m_err;
    int      m_col;
    int      m_line;
    int      m_num;
    bool     m_result;
    bool     m_is_look_ahead;
    bool     m_is_try;
};

template<typename T> typename parsec<T>::chars_t *parsec<T>::p_space = nullptr;
template<typename T> volatile int parsec<T>::lock = 0;

}

#endif // LUNAR_PARSEC_HPP
#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include "lunar_common.hpp"
#include "lunar_stream.hpp"
#include "lunar_string.hpp"

#include <string>
#include <set>
#include <vector>

namespace lunar {

template <typename T>
class parsec {
public:
    typedef stream<T>            stream_t;
    typedef std::set<T>          chars_t;
    typedef std::basic_string<T> string_t;

    parsec(stream_t &stream)
        : m_stream(stream),
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
    }

    virtual ~parsec() { }
    
    static void make_chars(chars_t &chars, const string_t &str)
    {
        for (auto &c: str)
            chars.insert(c);
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

        virtual void operator() () { };

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
        parser_func(parsec &p, std::function<void(parsec&)> func)
            : parser(p), m_func(func) { }
        virtual ~parser_func() { }
        
        virtual void operator() () { m_func(parser::m_parsec); }
    
    private:
        std::function<void(parsec&)> m_func;
    };
    
    class parser_chain : public parser {
    public:
        parser_chain(parser *lhs, parser *rhs)
            : parser(lhs->m_parsec), m_lhs(lhs), m_rhs(rhs)
        {
            assert(&lhs->m_parsec == &rhs->m_parsec);
        }
        virtual ~parser_chain() { }
        
        virtual void operator() ()
        {
            (*m_lhs)();
            if (parser::m_parsec.m_result) {
                (*m_rhs)();
            }
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
        
        virtual void operator() ()
        {
            (*m_lhs)();
            if (! parser::m_parsec.m_result) {
                (*m_rhs)();
            }
        }
        
    private:
        parser *m_lhs, *m_rhs;
    };
    
    class parser_satisfy : public parser {
    public:
        parser_satisfy(parsec &p, std::function<bool(T)> func) : parser(p), m_func(func) { }
        virtual ~parser_satisfy() { }
        
        virtual void operator() ()
        {
            T c;
            auto result = parser::m_parsec.m_stream.front(c); 

            if (result == NO_MORE_DATA) {
                parser::m_parsec.m_result = false;
                parser::m_parsec.set_err(result, parser::m_parsec.m_line, parser::m_parsec.m_col);
                return;
            } else if (result == END_OF_STREAM) {
                parser::m_parsec.m_result = false;
                parser::m_parsec.set_err(result, parser::m_parsec.m_line, parser::m_parsec.m_col);
                return;
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
            } else {
                parser::m_parsec.m_result = false;
                parser::m_parsec.set_err(result, parser::m_parsec.m_line, parser::m_parsec.m_col);
            }
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

    class parser_many : public parser {
    public:
        parser_many(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_many() { }
        
        virtual void operator() ()
        {
            for (;;) {
                m_parser();
                if (! parser::m_parsec.m_result)
                    break;
            }
            
            parser::m_parsec.m_result = true;
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_many1 : public parser {
    public:
        parser_many1(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_many1() { }
        
        virtual void operator() ()
        {
            for (;;) {
                m_parser();
                if (! parser::m_parsec.m_result)
                    break;
            }
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_string : public parser {
    public:
        parser_string(parsec &p, const string_t &str) : parsec(p), m_str(str) { }
        virtual ~parser_string() { }
        
        virtual void operator() ()
        {
            for (auto &s: m_str) {
                parser::m_parsec.character(s)();
                
                if (! parser::m_parsec.m_result) {
                    return;
                }
            }
            
            parser::m_parsec.m_result = true;
        }
    
    private:
        const string_t &m_str;
    };
    
    class parser_try : public parser {
    public:
        parser_try(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_try() { }
        
        virtual void operator() ()
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
        }
    
    private:
        parser &m_parser;
    };
    
    class parser_look_ahead : public parser {
    public:
        parser_look_ahead(parsec &pc, parser &pr) : parser(pc), m_parser(pr) { }
        virtual ~parser_look_ahead() { }
        
        virtual void operator() ()
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
    stream_t &m_stream;
    string_t  m_str;
    message   m_err;
    int       m_col;
    int       m_line;
    int       m_num;
    bool      m_result;
    bool      m_is_look_ahead;
    bool      m_is_try;
};

}

#endif // LUNAR_PARSEC_HPP
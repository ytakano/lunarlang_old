#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include "lunar_common.hpp"
#include "lunar_stream.hpp"

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
          m_result(false),
          m_is_look_ahead(false),
          m_is_try(false) { }
    virtual ~parsec() { }
    
    static void make_chars(chars_t &chars, const string_t &str)
    {
        for (auto &c: str)
            chars.insert(c);
    }
    
    class parser_chain;
    class parser_or;
    
    class parser {
    public:
        parser(parsec &p) : m_parsec(p) { }
        virtual ~parser() { }

        virtual void operator() () { };

        parsec::parser_chain operator>> (parsec::parser &p)
        {
            return parsec::parser_chain(this, &p);
        }

        parsec::parser_or operator|| (parsec::parser &p)
        {
            return parsec::parser_or(this, &p);
        }
    
    public:
        parsec &m_parsec;
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
    
    class parser_one_of : public parser {
    public:
        parser_one_of(parsec &p, const chars_t &chars) : parser(p), m_chars(chars) { }
        virtual ~parser_one_of() { }
        
        virtual void operator() ()
        {
            if (parser::m_parsec.m_stream.empty()) {
                parser::m_parsec.m_result = false;
                // TODO: print error
            } else {
                auto c = parser::m_parsec.m_stream.front();
                if (m_chars.find(c) == m_chars.end()) {
                    parser::m_parsec.m_result = false;
                    // TODO: print error
                } else {
                    parser::m_parsec.m_result = true;
                    parser::m_parsec.m_num++;
                    parser::m_parsec.m_str.push_back(c);

                    if ((char)c == '\n') {
                        parser::m_parsec.m_line++;
                        parser::m_parsec.m_col = 0;
                    } else {
                        parser::m_parsec.m_col++;
                    }
                    
                    if (parser::m_parsec.m_is_look_ahead || parser::m_parsec.m_is_try) {
                        parser::m_parsec.m_stream.move(1);
                    } else {
                        parser::m_parsec.m_stream.consume(1);
                    }
                }
            }
        }
    
    private:
        const chars_t &m_chars;
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
    
    parser_one_of one_of(const chars_t &chars)
    {
        return parser_one_of(*this, chars);
    }
    
    parser_many many(parser &p)
    {
        return parser_many(*this, p);
    }
    
    string_t get_string()
    {
        return m_str;
    }
    
    void clear_string()
    {
        m_str.clear();
    }
    
    static void parse(parser &p)
    {
        
    }

private:
    stream_t &m_stream;
    string_t  m_str;
    int       m_col;
    int       m_line;
    int       m_num;
    bool      m_result;
    bool      m_is_look_ahead;
    bool      m_is_try;
};

}

#endif // LUNAR_PARSEC_HPP
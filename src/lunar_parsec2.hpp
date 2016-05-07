#ifndef LUNAR_PARSEC2_HPP
#define LUNAR_PARSEC2_HPP

#include "lunar_common.hpp"
#include "lunar_bytestream.hpp"
#include "lunar_string.hpp"
#include "lunar_ringq.hpp"
#include "lunar_fiber.hpp"

#include <string>
#include <unordered_set>
#include <vector>

namespace lunar {

template <typename T>
class parsec2 {
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
        
        bool operator bool() const { return m_is_result; }
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
                    std::u32string *ptr;
                    auto result2 = pop_string(&m_parsec.m_shared_stream, &ptr);
                    if (result2 == STRM_SUCCESS) {
                        m_parsec.m_bytes.push_back(ptr);
                    } else if (result2 == STRM_CLOSED) {
                        m_parsec.m_bytes.push_eof();
                    } else {
                        yield_fiber();
                    }
                } else {
                    m_parsec.m_result = false;
                    m_parsec.set_err(result, m_parsec.m_line, m_parsec.m_col);

                    char_t ret;
                    ret.m_is_result = false;
                    return ret;
                }
            }
            
            auto ret = m_func(c);
            if (ret) {
                m_parsec.m_result = true;
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
            
            m_parsec.m_result = false;
            m_parsec.set_err(STRM_SUCCESS, m_parsec.m_line, m_parsec.m_col);
            
            return ret;
        }
    
    private:
        std::function<char_t(T)> m_func;
        parsec2 &m_parsec;
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
            if (m_parsec.m_result) {
                if (! m_is_try && ! m_parsec.m_is_look_ahead) {
                   	auto n = m_parsec.m_num - num;
                    m_parsec.m_bytes.restore_tmp_pos(pos);
                    m_parsec.m_bytes.consume(n);
                }
                
                parser::m_parsec.m_is_try = is_try;
            } else {
                m_parsec.m_col    = m_col;
                m_parsec.m_line   = m_line;
                m_parsec.m_num    = m_num;
                m_parsec.m_is_try = m_is_try;
                m_parsec.m_bytes.restore_tmp_pos(pos);
            }
        }
    
    private:
        parsec2  &m_parsec;
        uint64_t  m_col, m_line, m_num;
        point2u64 m_pos;
        bool      m_is_try;
    };
    
    template <typename RET>
    class parser_many {
    public:
        parser_many(parsec &p, std::function<char_t(T)> func) : m_parsec(p), m_func(func) { }
        virtual ~parser_many() { }
        
        std::unique_ptr<std::vector<RET>> operator() () {
            auto vals = llvm::make_unique<std::vector<RET>>();
            
            return vals;
        }
    
    private:
        parsec2 &m_parsec;
    };
    
    parsec2(shared_stream s)
        : m_shared_stream(s),
          m_is_result(true),
          m_col(1),
          m_line(1),
          m_num(0),
          m_is_skip(false),
          m_is_look_ahead(false),
          m_is_try(false) { }
    virtual ~parsec2() { }
    
    void set_err(STRM_RESULT result, int line, int col)
    {
        m_err.result = result;
        m_err.line   = line;
        m_err.col    = col;
    }

private:
    shared_stream m_shared_stream;
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

#endif // LUNAR_PARSEC2_HPP
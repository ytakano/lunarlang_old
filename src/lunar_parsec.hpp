#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include "lunar_common.hpp"
#include "lunar_stream.hpp"

#include <string>
#include <set>

namespace lunar {

template <typename T>
class parsec {
    typedef stream<T> stream_t;
    typedef std::set<T> char_set_t;
    typedef std::basic_string<T> string_t;

public:
    parsec(std::unique_ptr<stream_t> stream)
        : m_stream(std::move(stream)), m_col(1), m_line(1) { }
    virtual ~parsec() { }

    static std::shared_ptr<char_set_t> make_char_set(const string_t &str)
    {
        std::shared_ptr<char_set_t> chars(new std::set<T>);
        for (auto &c: str) {
            chars->insert(c);
        }
        
        return chars;
    }

    T one_of(std::shared_ptr<char_set_t> chars, bool is_look_ahead = false, bool is_try = false)
    {
        T c = m_stream.front();
    }

private:
    std::unique_ptr<stream_t> m_stream;
    int m_col;
    int m_line;
};

}

#endif // LUNAR_PARSEC_HPP
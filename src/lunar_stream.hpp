#ifndef LUNAR_STREAM_HPP
#define LUNAR_STREAM_HPP

#include "lunar_common.hpp"

#include <deque>

namespace lunar {

template <typename T>
class stream {
public:
    stream() { }
    
    T front() { return m_deque.front().front(); }

    void consume(int num)
    {

    }

private:
    typedef std::basic_string<T> string_t;

    class data_t {
    public:
        data_t(std::unique_ptr<string_t> data) : m_data(data), m_pos(0) { }
        
        T front()
        {
            return m_data[m_pos];
        }

    private:
        std::unique_ptr<string_t> m_data;
        int m_pos;
    };

    std::deque<data_t> m_deque;
};

};

#endif // LUNAR_STREAM_HPP
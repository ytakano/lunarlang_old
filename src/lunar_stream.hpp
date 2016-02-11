#ifndef LUNAR_STREAM_HPP
#define LUNAR_STREAM_HPP

#include "lunar_common.hpp"

#include <deque>

namespace lunar {

template <typename T>
class stream {
    typedef std::basic_string<T> string_t;

public:
    stream() { }
    
    T front() { return m_deque.front()->front(); }
    
    void move(int num)
    {
        // TODO
    }

    void consume(int num)
    {
        while (! m_deque.empty() && num > 0) {
            auto &data = m_deque.front();
            int   len  = data->size();
            
            if (num >= len) {
                m_deque.pop_front();
                num -= len;
            } else {
                data->consume(num);
                break;
            }
        }
    }
    
    bool empty() { return m_deque.empty(); }
    
    void push_back(std::unique_ptr<string_t> data)
    {
        auto d = llvm::make_unique<data_t>(std::move(data));
        m_deque.push_back(std::move(d));
    }

private:
    class data_t {
    public:
        data_t(std::unique_ptr<string_t> data) : m_data(std::move(data)), m_pos(0) { }
        
        T front()
        {
            return (*m_data)[m_pos];
        }
        
        int size()
        {
            return m_data->size() - m_pos;
        }
        
        void consume(int num)
        {
            m_pos += num;
            assert(m_pos <= m_data->size());
        }

    private:
        std::unique_ptr<string_t> m_data;
        size_t m_pos;
    };

    std::deque<std::unique_ptr<data_t>> m_deque;
};

};

#endif // LUNAR_STREAM_HPP
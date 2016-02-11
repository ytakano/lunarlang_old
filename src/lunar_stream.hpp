#ifndef LUNAR_STREAM_HPP
#define LUNAR_STREAM_HPP

#include "lunar_common.hpp"

#include <deque>

namespace lunar {

enum read_result {
    SUCCESS,
    NO_MORE_DATA,
    END_OF_STREAM,
};

template <typename T>
class stream {
    typedef std::basic_string<T> string_t;

public:
    stream() : m_is_eof(false) { }
    
    read_result front(T &c)
    {
        assert(m_tmp_pos.x < (int)m_deque.size());
        if (m_deque.front()->front(c, m_tmp_pos.y) == NO_MORE_DATA) {
            if (m_is_eof) {
                return END_OF_STREAM;
            } else {
                return NO_MORE_DATA;
            }
        } else {
            return SUCCESS;
        }
    }
    
    const point2i & get_tmp_pos() const { return m_tmp_pos; }
    
    void tmp_pos_move(int num)
    {
        // TODO
    }
    
    void tmp_pos_restore(const point2i &pos)
    {
        assert(pos.x >= 0 && pos.y >= 0);
        m_tmp_pos = pos;
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
    
    void push_back(std::unique_ptr<string_t> data)
    {
        auto d = llvm::make_unique<data_t>(std::move(data));
        m_deque.push_back(std::move(d));
    }

private:
    class data_t {
    public:
        data_t(std::unique_ptr<string_t> data) : m_data(std::move(data)), m_pos(0) { }
        
        read_result front(T &c, int offset)
        {
            int pos = m_pos + offset;
            if (pos >= (int)m_data->size()) {
                return NO_MORE_DATA;
            } else {
                c = (*m_data)[m_pos];
                return SUCCESS; 
            }
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
    point2i m_tmp_pos;
    bool    m_is_eof;
};

};

#endif // LUNAR_STREAM_HPP
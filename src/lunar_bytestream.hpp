#ifndef LUNAR_BYTESTREAM_HPP
#define LUNAR_BYTESTREAM_HPP

#include "lunar_common.hpp"

#include <deque>

namespace lunar {

enum read_result {
    SUCCESS,
    NO_MORE_DATA,
    END_OF_STREAM,
};

template <typename T>
class bytestream {
    typedef std::basic_string<T> string_t;

public:
    bytestream() : m_is_eof(false) { }
    
    read_result front(T &c)
    {
        assert(m_tmp_pos.x <= (int)m_deque.size());
        if (m_tmp_pos.x >= (int)m_deque.size() ||
            m_deque[m_tmp_pos.x]->front(c, m_tmp_pos.y) == NO_MORE_DATA) {
            return m_is_eof ? END_OF_STREAM : NO_MORE_DATA;
        } else {
            return SUCCESS;
        }
    }
    
    const point2i & get_tmp_pos() const { return m_tmp_pos; }
    
    void move_tmp_pos(int num)
    {
        assert(num > 0);
        assert((size_t)m_tmp_pos.x < m_deque.size());
        
        while ((size_t)m_tmp_pos.x < m_deque.size()) {
            int size = m_deque[m_tmp_pos.x]->size() - m_tmp_pos.y;
            if (size > num) {
                m_tmp_pos.y += num;
                break;
            } else {
                m_tmp_pos.x++;
                m_tmp_pos.y = 0;

                num -= size;
                if (num == 0) {
                    break;
                }
            }
        }
    }
    
    void restore_tmp_pos(const point2i &pos)
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
        
        read_result front(T &c, size_t offset)
        {
            auto pos = m_pos + offset;
            if (pos >= m_data->size()) {
                return NO_MORE_DATA;
            } else {
                c = (*m_data)[pos];
                return SUCCESS;
            }
        }
        
        size_t size()
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

#endif // LUNAR_BYTESTREAM_HPP
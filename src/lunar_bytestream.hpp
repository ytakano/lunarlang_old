#ifndef LUNAR_BYTESTREAM_HPP
#define LUNAR_BYTESTREAM_HPP

#include "lunar_common.hpp"

#include <deque>

namespace lunar {

template <typename T>
class bytestream {
    typedef std::basic_string<T> string_t;

public:
    bytestream() : m_is_eof(false) { }

    ~bytestream()
    {
        for (auto &data: m_deque) {
            data.remove();
        }
    }
    
    STRM_RESULT front(T &c)
    {
        assert(m_tmp_pos.x <= (int)m_deque.size());
        if (m_tmp_pos.x >= (int)m_deque.size() ||
            m_deque[m_tmp_pos.x].front(c, m_tmp_pos.y) == STRM_NO_MORE_DATA) {
            return m_is_eof ? STRM_CLOSED : STRM_NO_MORE_DATA;
        } else {
            return STRM_SUCCESS;
        }
    }
    
    const point2i & get_tmp_pos() const { return m_tmp_pos; }
    
    void move_tmp_pos(int num)
    {
        assert(num > 0);
        assert((size_t)m_tmp_pos.x < m_deque.size());
        
        while ((size_t)m_tmp_pos.x < m_deque.size()) {
            int size = m_deque[m_tmp_pos.x].size() - m_tmp_pos.y;
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
            data_t &data = m_deque.front();
            int     len  = data.size();
            
            if (num >= len) {
                data.remove();
                m_deque.pop_front();
                num -= len;
            } else {
                data.consume(num);
                break;
            }
        }
    }
    
    void push_back(string_t *data)
    {
        m_deque.push_back(data_t(data));
    }
    
    void push_eof() { m_is_eof = true; }

private:
    class data_t {
    public:
        data_t(string_t *data) : m_data(data), m_pos(0) { }
        ~data_t() { }
        
        STRM_RESULT front(T &c, size_t offset)
        {
            auto pos = m_pos + offset;
            if (pos >= m_data->size()) {
                return STRM_NO_MORE_DATA;
            } else {
                c = (*m_data)[pos];
                return STRM_SUCCESS;
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
        
        void remove()
        {
            delete m_data;
        }

    private:
        string_t *m_data;
        size_t    m_pos;
    };

    std::deque<data_t> m_deque;
    point2i m_tmp_pos;
    bool    m_is_eof;
};

};

#endif // LUNAR_BYTESTREAM_HPP
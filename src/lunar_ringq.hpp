#ifndef LUNAR_RINGQ_HPP
#define LUNAR_RINGQ_HPP

namespace lunar {

template <typename T>
class ringq {
public:
    ringq(int len) : m_max_len(len),
                  m_len(0),
                  m_buf(new T[len]),
                  m_buf_end(m_buf + len),
                  m_head(m_buf),
                  m_tail(m_buf),
                  m_is_eof(false) { }
    virtual ~ringq() { delete[] m_buf; }

    STRM_RESULT pop(T &retval);
    STRM_RESULT push(const T &val);
    void push_eof() { m_is_eof = true; }
    bool is_eof() { return m_is_eof; }
    int  get_len() { return m_len; }

private:
    int m_max_len;
    volatile int m_len;
    T *m_buf;
    T *m_buf_end;

    T *m_head;
    T *m_tail;

    bool m_is_eof;
};

template <typename T>
inline STRM_RESULT
ringq<T>::pop(T &retval)
{
    if (m_len == 0) {
        if (m_is_eof)
            return STRM_CLOSED;
        else
            return STRM_NO_MORE_DATA;
    }

    retval = *m_head;
    m_len--;

    m_head++;

    if (m_head == m_buf_end) {
        m_head = m_buf;
    }

    return STRM_SUCCESS;
}

template <typename T>
STRM_RESULT
ringq<T>::push(const T &val)
{
    if (m_is_eof)
        return STRM_CLOSED;

    if (m_len == m_max_len)
        return STRM_NO_VACANCY;

    *m_tail = val;
    m_len++;

    m_tail++;

    if (m_tail == m_buf_end) {
        m_tail = m_buf;
    }

    return STRM_SUCCESS;
}

}

#endif // LUNAR_RINGQ_HPP
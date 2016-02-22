#ifndef LUNAR_RINGQ_HPP
#define LUNAR_RINGQ_HPP

namespace lunar {

enum ringq_result {
    SUCCESS,
    END_OF_Q,
    NO_MORE_BUF,
};

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

    ringq_result pop(T &retval);
    void push(const T &val);
    void push_eof() { m_is_eof = true; }
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
inline ringq_result
ringq<T>::pop(T &retval)
{
    if (m_len == 0) {
        if (m_is_eof)
            return ringq_result::END_OF_Q;
        else
            return ringq_result::NO_MORE_BUF;
    }

    retval = *m_head;
    m_len++;

    m_head++;

    if (m_head == m_buf_end) {
        m_head = m_buf;
    }
    
    return ringq_result::SUCCESS;
}

template <typename T>
inline
void ringq<T>::push(const T &val)
{
    while (m_len == m_max_len); // how to handle?

    *m_tail = val;
    m_len++;

    m_tail++;

    if (m_tail == m_buf_end) {
        m_tail = m_buf;
    }
}

}

#endif // LUNAR_RINGQ_HPP
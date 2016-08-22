#ifndef LUNAR_HASH_HPP
#define LUNAR_HASH_HPP

#include <inttypes.h>

#include <iterator>
#include <list>
#include <functional>

#include <x86intrin.h>

namespace lunar {

template<typename T, typename F = std::hash<T>>
class hash_set
{
public:
    hash_set() : m_min_bucket(32),
                 m_max_bucket(1 << 63),
                 m_num_bucket(m_min_bucket),
                 m_bucket(new std::list<T>[m_num_bucket + 1]),
                 m_mask(m_num_bucket - 1),
                 m_mask_bits(63 - _lzcnt_u64(m_num_bucket)),
                 m_size(0) { }
    virtual ~hash_set()
    {
        delete[] m_bucket;
    }

    class iterator : public std::iterator<std::forward_iterator_tag, T>
    {
    public:
        iterator() : m_ptr(nullptr) { }

        const iterator& operator++ ()
        {
            ++m_it;
            if (m_it == m_ptr->end()) {
                while (++m_ptr < m_ptr_end) {
                    if (! m_ptr->empty()) {
                        m_it = m_ptr->begin();
                        break;
                    }
                }

                if (m_ptr == m_ptr_end) {
                    m_ptr--;
                    m_it = m_ptr->end();
                }
            }

            return *this;
        }

        const T& operator* () const
        {
            return *m_it;
        }

        bool operator== (const iterator &lhs) const
        {
            return m_ptr == lhs.m_ptr && m_it == lhs.m_it;
        }

        bool operator!= (const iterator &lhs) const
        {
            return ! (*this == lhs);
        }

    private:
        iterator(std::list<T> *ptr, std::list<T> *ptr_end, typename std::list<T>::iterator it)
            : m_ptr(ptr), m_it(it), m_is_end(false) { }

        std::list<T> *m_ptr;
        typename std::list<T>::iterator m_it;
        bool m_is_end;

        friend class hash_set;
    };

    iterator find(const T &val)
    {
        auto idx = get_idx(val);

        for (auto it = m_bucket[idx].begin(); it != m_bucket[idx].end(); ++it) {
            if (*it == val) {
                return iterator(&m_bucket[idx], &m_bucket[m_num_bucket], it);
            }
        }

        return iterator();
    }

    iterator begin()
    {
        for (uint64_t i = 0; i < m_num_bucket; i++) {
            if (! m_bucket[i].empty())
                return iterator(&m_bucket[i], &m_bucket[m_num_bucket], m_bucket[i].begin());
        }

        return iterator();
    }

    iterator end()
    {
        return iterator(&m_bucket[m_num_bucket], &m_bucket[m_num_bucket], m_bucket[m_num_bucket].begin());
    }

    std::pair<iterator, bool> insert(const T &val)
    {
        if (m_size > (m_num_bucket >> 1)) {
            if (! increase_bucket())
                return std::pair<iterator, bool>(end(), false);
        }

        typename std::list<T>::iterator it;
        auto idx = get_idx(val);
        for (it = m_bucket[idx].begin(); it != m_bucket[idx].end(); ++it) {
            if (*it == val) {
                *it = val;
                break;
            }
        }

        if (it == m_bucket[idx].end()) {
            m_bucket[idx].push_back(val);
            it = --(m_bucket[idx].end());
        }

        return std::pair<iterator, bool>(iterator(&m_bucket[idx], &m_bucket[m_num_bucket], it), false);
    }

    uint64_t erase(const T &val)
    {
        auto idx = get_idx(val);

        for (auto it = m_bucket[idx].begin(); it != m_bucket[idx].end(); ++it) {
            if (*it == val) {
                m_bucket[idx].erase(it);
                return 1;
            }
        }

        return 0;
    }

private:
    const uint64_t m_min_bucket;
    const uint64_t m_max_bucket;
    uint64_t       m_num_bucket;
    std::list<T>  *m_bucket;
    uint64_t       m_mask;
    uint64_t       m_mask_bits;
    uint64_t       m_size;
    F              m_hash_func;

    uint64_t get_idx(const T& val)
    {
        uint64_t h   = m_hash_func(val);
        uint64_t idx = 0;

        do {
            idx ^= h & m_mask;
            h >>= m_mask_bits;
        } while (h != 0);

        printf("val = %d, idx = %d, num_bucket = %d\n", val, idx, m_num_bucket);

        return idx;
    }

    bool increase_bucket()
    {
        if (m_num_bucket == m_max_bucket)
            return false;

        auto old_num_bucket = m_num_bucket;
        auto old_bucket     = m_bucket;

        m_num_bucket = m_num_bucket << 1;
        m_bucket     = new std::list<T>[m_num_bucket + 1];
        m_mask       = m_num_bucket - 1;
        m_mask_bits  = 63 - _lzcnt_u64(m_num_bucket);

        for (uint64_t i = 0; i < old_num_bucket; i++) {
            for (auto ptr: old_bucket[i]) {
                auto idx = get_idx(ptr);
                m_bucket[idx].push_back(ptr);
            }
        }

        return true;
    }

    bool decrease_bucket()
    {
        if (m_num_bucket == m_min_bucket)
            return false;

        auto old_num_bucket = m_num_bucket;
        auto old_bucket     = m_bucket;

        m_num_bucket = m_num_bucket >> 1;
        m_bucket     = new std::list<T>[m_num_bucket + 1];
        m_mask       = m_num_bucket - 1;
        m_mask_bits  = 63 - _lzcnt_u64(m_num_bucket);

        for (uint64_t i = 0; i < old_num_bucket; i++) {
            for (auto ptr: old_bucket[i]) {
                auto idx = get_idx(ptr);
                m_bucket[idx].push_back(ptr);
            }
        }

        return true;
    }
};

}

#endif // LUNAR_HASH_HPP
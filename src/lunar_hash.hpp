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
    hash_set() : m_min_bucket(64),
                 m_max_bucket((uint64_t)1 << 56),
                 m_num_bucket(m_min_bucket),
                 m_bucket(new std::list<T>[m_num_bucket + 1]),
                 m_mask(m_num_bucket - 1),
                 m_mask_bits(_tzcnt_u64(m_num_bucket)),
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
                    m_it = m_ptr->begin();
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
            : m_ptr(ptr), m_ptr_end(ptr_end), m_it(it), m_is_end(false) { }

        std::list<T> *m_ptr;
        std::list<T> *m_ptr_end;
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

        return end();
    }

    iterator begin()
    {
        for (uint64_t i = 0; i < m_num_bucket; i++) {
            if (! m_bucket[i].empty())
                return iterator(&m_bucket[i], &m_bucket[m_num_bucket], m_bucket[i].begin());
        }

        return end();
    }

    iterator end()
    {
        return iterator(&m_bucket[m_num_bucket], &m_bucket[m_num_bucket], m_bucket[m_num_bucket].begin());
    }

    std::pair<iterator, bool> insert(const T &val)
    {
        if (m_size > (m_num_bucket >> 1))
            increase_bucket();

        typename std::list<T>::iterator it;
        auto idx = get_idx(val);
        auto end = m_bucket[idx].end();
        for (it = m_bucket[idx].begin(); it != end; ++it) {
            if (*it == val) {
                *it = val;
                return std::pair<iterator, bool>(iterator(&m_bucket[idx], &m_bucket[m_num_bucket], it), true);
            }
        }

        m_bucket[idx].push_back(val);
        it = --end;
        m_size++;

        return std::pair<iterator, bool>(iterator(&m_bucket[idx], &m_bucket[m_num_bucket], it), true);
    }

    uint64_t erase(const T &val)
    {
        if (m_size < (m_num_bucket >> 3))
            decrease_bucket();

        auto idx = get_idx(val);

        for (auto it = m_bucket[idx].begin(); it != m_bucket[idx].end(); ++it) {
            if (*it == val) {
                m_bucket[idx].erase(it);
                m_size--;
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

        return idx;
    }

    bool increase_bucket()
    {
        if (m_num_bucket >= m_max_bucket) {
            return false;
        }

        auto old_num_bucket = m_num_bucket;
        auto old_bucket     = m_bucket;

        m_num_bucket = m_num_bucket << 2;
        m_bucket     = new std::list<T>[m_num_bucket + 1];
        m_mask       = m_num_bucket - 1;
        m_mask_bits  = _tzcnt_u64(m_num_bucket);

        for (uint64_t i = 0; i < old_num_bucket; i++) {
            for (auto &val: old_bucket[i]) {
                auto idx = get_idx(val);
                m_bucket[idx].push_back(val);
            }
        }

        delete[] old_bucket;

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
        m_mask_bits  = _tzcnt_u64(m_num_bucket);

        for (uint64_t i = 0; i < old_num_bucket; i++) {
            for (auto &val: old_bucket[i]) {
                auto idx = get_idx(val);
                m_bucket[idx].push_back(val);
            }
        }

        delete[] old_bucket;

        return true;
    }
};

template<typename K, typename V, typename F = std::hash<K>>
class hash_map
{
    class hpair {
    public:
        bool operator== (const hpair &rhs)
        {
            return first == rhs.first;
        }

        K first;
        V second;
    };

    class hash_func {
    public:
        uint32_t operator() (const hpair &lhs)
        {
            return F()(lhs.first);
        }
    };

public:
    class iterator : public std::iterator<std::forward_iterator_tag, std::pair<K, V>>
    {
    public:

    private:
        typename hash_set<hpair, hash_func>::iterator m_it;
    };

private:

    hash_set<hpair, hash_func> m_set;

};

}

#endif // LUNAR_HASH_HPP
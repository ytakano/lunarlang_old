#ifndef LUNAR_SKIPLIST_HPP
#define LUNAR_SKIPLIST_HPP

#include "lunar_xorshift.hpp"

#include <stdlib.h>

namespace lunar {

template <typename K, typename V, int MAX_LEVEL> class skiplist;

template <typename K, typename V, int MAX_LEVEL>
class skiplist_node {
public:
    skiplist_node() { }
    virtual ~skiplist_node() { delete[] m_forward; }

private:
    uint8_t m_level;
    K m_key;
    V m_val;
    skiplist_node **m_forward;
    skiplist_node  *m_backward;

    friend class skiplist<K, V, MAX_LEVEL>;
};

template <typename K, typename V, int MAX_LEVEL = 32>
class skiplist {
public:
    skiplist();
    virtual ~skiplist();

    void insert(const K &key, const V &val);
    void erase(const K &key);
    const V* find(const K &key);

private:
    skiplist_node<K, V, MAX_LEVEL> *m_header;
    uint64_t m_size;
    uint8_t  m_level;

    xorshift32 m_xs;

    uint8_t random_level();
};

template <typename K, typename V, int MAX_LEVEL>
inline skiplist<K, V, MAX_LEVEL>::skiplist() : m_size(0), m_level(1)
{
    m_header = new skiplist_node<K, V, MAX_LEVEL>;

    m_header->m_level = MAX_LEVEL;
    m_header->m_forward = new skiplist_node<K, V, MAX_LEVEL>*[MAX_LEVEL];

    for (int i = 0; i < MAX_LEVEL; i++) {
        m_header->m_forward[i] = nullptr;
    }
}

template <typename K, typename V, int MAX_LEVEL>
inline skiplist<K, V, MAX_LEVEL>::~skiplist()
{
    auto p = m_header;
    while (p != nullptr) {
        auto p1 = p->m_forward[0];
        delete p;
        p = p1;
    }
}

template <typename K, typename V, int MAX_LEVEL>
inline uint8_t skiplist<K, V, MAX_LEVEL>::random_level()
{
    uint64_t max_level;
    uint64_t lvl = 1;

    asm (
        "lzcntq %0, %1;"
        : "=r" (max_level)
        : "r" (m_size)
        );

    max_level = 64 - max_level + 1;
    max_level = max_level > MAX_LEVEL ? MAX_LEVEL : max_level;

    while (lvl < max_level && m_xs.xor32() < (UINT32_MAX >> 1))
        lvl++;

    return lvl;
}

template <typename K, typename V, int MAX_LEVEL>
inline void skiplist<K, V, MAX_LEVEL>::insert(const K &key, const V &val)
{
    skiplist_node<K, V, MAX_LEVEL> *update[MAX_LEVEL];
    skiplist_node<K, V, MAX_LEVEL> *x = m_header;

    for (int i = m_level - 1; i >= 0; i--) {
        while (x->m_forward[i] != nullptr && x->m_forward[i]->m_key < key)
            x = x->m_forward[i];

        update[i] = x;
    }

    x = x->m_forward[0];

    if (x != nullptr && x->m_key == key) {
        x->m_val = val;
    } else {
        auto new_node = new skiplist_node<K, V, MAX_LEVEL>();

        new_node->m_level   = random_level();
        new_node->m_forward = new skiplist_node<K, V, MAX_LEVEL>*[new_node->m_level];

        if (new_node->m_level > m_level) {
            for (int i = m_level; i < new_node->m_level; i++) {
                update[i] = m_header;
            }

            m_level = new_node->m_level;
        }

        new_node->m_key = key;
        new_node->m_val = val;

        for (int i = 0; i < new_node->m_level; i++) {
            new_node->m_forward[i]  = update[i]->m_forward[i];
            update[i]->m_forward[i] = new_node;
        }

        new_node->m_backward = update[0];
        if (update[0]->m_forward[0] != nullptr)
            update[0]->m_forward[0]->m_backward = new_node;

        m_size++;
    }
}

template <typename K, typename V, int MAX_LEVEL>
inline void skiplist<K, V, MAX_LEVEL>::erase(const K &key)
{
    skiplist_node<K, V, MAX_LEVEL> *update[MAX_LEVEL];
    skiplist_node<K, V, MAX_LEVEL> *x = m_header, *p = nullptr;

    for (int i = m_level - 1; i >= 0; i--) {
        while (x->m_forward[i] != nullptr && x->m_forward[i]->m_key < key)
            x = x->m_forward[i];

        update[i] = x;
    }

    x = x->m_forward[0];

    if (x != nullptr && x->m_key == key) {
        for (int i = 0; i < m_level; i++) {
            if (update[i]->m_forward[i] != x)
                break;

            update[i]->m_forward[i] = x->m_forward[i];
        }

        if (x->m_forward[0] != nullptr)
            x->m_forward[0]->m_backward = update[0];

        p = x;

        int i = m_level - 1;
        while(i >= 0 && m_header->m_forward[i] == nullptr) {
            i--;
        }

        m_level = i + 1;
    }

    delete p;
}

template <typename K, typename V, int MAX_LEVEL>
inline const V* skiplist<K, V, MAX_LEVEL>::find(const K &key)
{
    auto x = m_header;

    for (int i = m_level - 1; i >= 0; i--) {
        while (x->m_forward[i] != nullptr && x->m_forward[i]->m_key < key)
            x = x->m_forward[i];
    }

    x = x->m_forward[0];

    if (x != nullptr && x->m_key == key) {
        return &x->m_val;
    }

    return nullptr;
}

}

#endif // LUNAR_SKIPLIST_HPP

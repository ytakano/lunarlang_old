#ifndef LUNAR_SLUB_STACK_HPP
#define LUNAR_SLUB_STACK_HPP

#include "lunar_common.hpp"
#include "lunar_asm.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <string.h>

namespace lunar {

class slub_stack {
    struct slab;

    struct slab_head {
        uint64_t  m_idx;
        slab     *m_slab;
        void     *m_protect;
    };

    struct slab {
        void     *m_buf;
        uint64_t  m_mask;
        size_t    m_size;
        slab     *m_prev;
        slab     *m_next;

    public:
        slab(int alignment, size_t size) : m_mask(0), m_size(size), m_prev(nullptr), m_next(nullptr) {
            if (posix_memalign(&m_buf, alignment, size * 64)) {
                PRINTERR("failed allocate stack\n");
                exit(-1);
            }
        }

        void* allocate(slub_stack *slub)
        {
            uint64_t idx = tzcntq(~m_mask);

            void *protect = &((char*)m_buf)[m_size * idx];
            if (mprotect(protect, slub->m_pagesize, PROT_NONE) < 0) {
                PRINTERR("failed mprotect!: %s", strerror(errno));
                exit(-1);
            }

            void *ptr =  &((char*)protect)[m_size - 32];

            slab_head *head = (slab_head*)ptr;

            head->m_idx        = idx;
            head->m_slab       = this;
            head->m_protect    = protect;

            m_mask |= ((uint64_t)1 << idx);

            if (m_mask == (uint64_t)~0) {
                // remove from free list
                if (m_prev)
                    m_prev->m_next = m_next;

                if (m_next)
                    m_next->m_prev = m_prev;

                if (slub->m_freelist_head == this)
                    slub->m_freelist_head = m_next;

                if (slub->m_freelist_tail == this)
                    slub->m_freelist_tail = m_prev;

                // add to full list
                if (slub->m_full)
                    slub->m_full->m_prev = this;

                m_prev = nullptr;
                m_next = slub->m_full;
                slub->m_full = this;
            }

            return ptr;
        }

        void deallocate(void *ptr, slub_stack *slub)
        {
            slab_head *head = (slab_head*)ptr;

            if (mprotect(head->m_protect, slub->m_pagesize, PROT_READ | PROT_WRITE) < 0) {
                PRINTERR("failed mprotect!: %s", strerror(errno));
                exit(-1);
            }

            if (m_mask == (uint64_t)~0) {
                // remove from full list
                if (m_prev)
                    m_prev->m_next = m_next;

                if (m_next)
                    m_next->m_prev = m_prev;

                if (slub->m_full == this)
                    slub->m_full = m_next;

                // add to free list
                if (slub->m_freelist_tail) {
                    m_prev = slub->m_freelist_tail;
                    m_next = nullptr;
                    slub->m_freelist_tail->m_next = this;
                    slub->m_freelist_tail = this;
                } else {
                    slub->m_freelist_head = this;
                    slub->m_freelist_tail = this;
                }
            }

            m_mask &= ~((uint64_t)1 << head->m_idx);

            if (m_mask == 0 && slub->m_vacancy > 128 && slub->m_freelist_head != slub->m_freelist_tail) {
                if (m_prev)
                    m_prev->m_next = m_next;

                if (m_next)
                    m_next->m_prev = m_prev;

                if (slub->m_freelist_tail == this)
                    slub->m_freelist_tail = m_prev;

                slub->m_vacancy -= 64;
                delete this;
            }
        }

        void print_state()
        {
            for (int i = 63; i >= 0; i--)
            {
                if (m_mask & ((uint64_t)1 << i)) {
                    printf("1");
                } else {
                    printf("0");
                }
            }
            printf("\n");
        }

        ~slab() {
            free(m_buf);
        }
    };

public:
    slub_stack() : m_vacancy(64), m_full(nullptr)
    {
        m_pagesize = sysconf(_SC_PAGE_SIZE);
        m_freelist_head = m_freelist_tail = new slab(m_pagesize, m_pagesize * 1024);
    }

    ~slub_stack()
    {
        for (slab *ptr = m_freelist_head; ptr != nullptr;) {
            ptr = ptr->m_next;
            delete ptr;
        }

        for (slab *ptr = m_full; ptr != nullptr;) {
            ptr = ptr->m_next;
            delete ptr;
        }
    }

    void* allocate()
    {
        if (m_freelist_head) {
            m_vacancy--;
            return m_freelist_head->allocate(this);
        } else {
            m_freelist_head = m_freelist_tail = new slab(m_pagesize, m_pagesize * 1024);
            m_vacancy += 63;
            return m_freelist_head->allocate(this);
        }
    }

    void deallocate(void *ptr)
    {
        m_vacancy++;
        slab_head *head = (slab_head*)ptr;
        head->m_slab->deallocate(ptr, this);
    }

    void print_state()
    {
        printf("vacancy: %llu\n", m_vacancy);
        printf("free list:\n");
        int i;
        slab *ptr;
        for (i = 0, ptr = m_freelist_head; ptr != nullptr; i++, ptr = ptr->m_next) {
            printf("%d(%p): ", i, (void*)ptr);
            ptr->print_state();
        }

        printf("full list:\n");
        for (i = 0, ptr = m_full; ptr != nullptr; i++, ptr = ptr->m_next) {
            printf("%d(%p): ", i, (void*)ptr);
            ptr->print_state();
        }
        printf("\n");
    }

private:
    int       m_pagesize;
    uint64_t  m_vacancy;
    slab     *m_freelist_head;
    slab     *m_freelist_tail;
    slab     *m_full;
};

}

#endif // LUNAR_SLUB_STACK_HPP
#include "lunar_shared_type.hpp"

#include <stdint.h>

namespace lunar {

extern "C" {

/*
 * memory layout of shared type:
 * +--------------------------------------+ <- malloc and free here
 * |            reference count           |
 * |               (64 bits)              |
 * +--------------------------------------+ <- make_shared_type() returns a pointer pointing here
 * |                                      |
 * |                 data                 |
 * //          (variable length)          //
 * |                                      |
 */

void* make_shared_type(size_t size)
{
    uint64_t *ptr = (uint64_t*)malloc(size + sizeof(uint64_t));
    *ptr = 1;
    return ptr + 1;
}

void
incref_shared_type(void *p)
{
    uint64_t *ptr = (uint64_t*)p;
    __sync_fetch_and_add(&ptr[-1], 1);
}

void
deref_shared_type(void *p)
{
    uint64_t *ptr = (uint64_t*)p;
    uint64_t cnt = __sync_fetch_and_add(&ptr[-1], -1);
    if (cnt == 0)
        free(ptr - 1);
}

}

}
#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory>
#include <llvm/ADT/STLExtras.h>

#define TZCNTQ(DST, SRC)        \
    do {                        \
        asm (                   \
            "tzcntq %1, %0;"    \
            : "=r" (DST)        \
            : "r" (SRC)         \
            );                  \
    } while (0)

#define POPCNTQ(DST, SRC)       \
    do {                        \
        asm (                   \
            "popcntq %1, %0;"   \
            : "=r" (DST)        \
            : "r" (SRC)         \
            );                  \
    } while (0)

namespace lunar {

struct point2u64 {
    uint64_t x, y;

    point2u64() : x(0), y(0) { }
    point2u64(uint64_t x, uint64_t y) : x(x), y(y) { }
};

struct point3i {
    int x, y, z;

    point3i() : x(0), y(0), z(0) { }
    point3i(int x, int y, int z) : x(x), y(y), z(z) { }
};

enum STRM_RESULT {
    STRM_SUCCESS      =  1,
    STRM_NO_MORE_DATA = -1,
    STRM_CLOSED       = -2,
    STRM_NO_VACANCY   = -3,
};

inline uint64_t
tzcntq(uint64_t num)
{
    uint64_t ret;
    TZCNTQ(ret, num);
    return ret;
}

inline uint64_t
popcntq(uint64_t num)
{
    uint64_t ret;
    POPCNTQ(ret, num);
    return ret;
}

}

#define PRINTERR(M, ...) fprintf(stderr, "ERROR (%s:%d): " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#endif // LUNAR_COMMON_HPP
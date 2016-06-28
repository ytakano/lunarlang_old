#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory>
#include <llvm/ADT/STLExtras.h>

namespace lunar{

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

union alltype {
    void*    ptr;
    uint64_t u64;
     int64_t i64;
    uint32_t u32;
     int32_t i32;
    uint16_t u16;
     int16_t i16;
    uint8_t   u8;
     int8_t   i8;
     bool     bin;
};

}

#define PRINTERR(M, ...) fprintf(stderr, "ERROR (%s:%d): " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#endif // LUNAR_COMMON_HPP
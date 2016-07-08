#ifndef LUNAR_XORSHIFT_HPP
#define LUNAR_XORSHIFT_HPP

#include <stdint.h>

namespace lunar {

class xorshift32 {
public:
    xorshift32() : y(2463534242) { }
    xorshift32(uint32_t seed)
    {
        if (seed == 0)
            y = 2463534242;
        else
            y = seed;
    }

    uint32_t xor32()
    {
        y = y ^ (y << 13); y = y ^ (y >> 17);
        return y = y ^ (y << 5);
    }

private:
    uint32_t y;
};

class xorshift128 {
public:
    xorshift128() : x(123456789), y(362436069), z(521288629), w(88675123) { }
    xorshift128(uint32_t s)
    {
        if (s == 0) {
            x = 123456789;
            y = 362436069;
            z = 521288629;
            w = 88675123;
        } else {
            x = s = 1812433253U * (s ^ (s >> 30));
            y = s = 1812433253U * (s ^ (s >> 30)) + 1;
            z = s = 1812433253U * (s ^ (s >> 30)) + 2;
            w = s = 1812433253U * (s ^ (s >> 30)) + 3;
        }
    }

    uint32_t xor128()
    {
        uint32_t t = x ^ (x << 11);
        x = y; y = z; z = w;
        return (w = (w ^ (w >> 19)) ^ (t ^ (t >> 8)));
    }

private:
    uint32_t x, y, z, w;
};

}

#endif // LUNAR_XORSHIFT_HPP
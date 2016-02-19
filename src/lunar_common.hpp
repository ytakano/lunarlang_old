#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory>
#include <llvm/ADT/STLExtras.h>

namespace lunar{

struct point2i {
    int x, y;
    
    point2i() : x(0), y(0) { }
    point2i(int x, int y) : x(x), y(y) { }
};

struct point3i {
    int x, y, z;

    point3i() : x(0), y(0), z(0) { }
    point3i(int x, int y, int z) : x(x), y(y), z(z) { }
};

}

#define PRINTERR(M, ...) fprintf(stderr, "ERROR (%s:%d): " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#endif // LUNAR_COMMON_HPP
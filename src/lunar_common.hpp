#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory>

#define PRINTERR(M, ...) fprintf(stderr, "DEBUG %s:%d: " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#endif // LUNAR_COMMON_HPP
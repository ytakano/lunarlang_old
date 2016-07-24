#ifndef LUNAR_SHARED_TYPE_HPP
#define LUNAR_SHARED_TYPE_HPP

#include <stdlib.h>

namespace lunar {

extern "C" {

void *make_shared_type(size_t size);
void incref_shared_type(void *p);
void deref_shared_type(void *p);

}

}

#endif // LUNAR_SHARED_TYPE_HPP
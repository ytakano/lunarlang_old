#ifndef LUNAR_SHARED_STREAM_HPP
#define LUNAR_SHARED_STREAM_HPP

#include "lunar_common.hpp"

namespace lunar {

// every stream is multiple-writers and single-reader

struct shared_stream {
    static const uint32_t READ         = 0x0001;
    static const uint32_t WRITE        = 0x0002;
    static const uint32_t CLOSED_WRITE = 0x0004;
    static const uint32_t MT           = 0x0008;

    union {
        int   fd;
        void *ptr;
    } m_data;

    uint16_t  m_flag; // READ, WRITE
    uint16_t *m_flag_shared; // CLOSED_WRITE, MT
    uint32_t *m_refcnt;
};

}

#endif // LUNAR_SHARED_STREAM_HPP
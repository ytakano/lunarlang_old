#ifndef LUNAR_SHARED_STREAM_HPP
#define LUNAR_SHARED_STREAM_HPP

#include "lunar_common.hpp"
#include "lunar_spin_lock.hpp"

namespace lunar {

// container for stream
// every stream is multiple-writers and single-reader

union stream_t {
    int   fd;
    void *ptr;
};

struct shared_stream {
    static const uint32_t READ         = 0x0001;
    static const uint32_t WRITE        = 0x0002;
    static const uint32_t CLOSED_READ  = 0x0004;
    static const uint32_t CLOSED_WRITE = 0x0008;
    static const uint32_t MT           = 0x00F0;

    stream_t  stream;
    uint32_t  flag;  // READ, WRITE
    
    struct shared_data_t {
        uint32_t  flag_shared; // CLOSED_READ, CLOSED_WRITE, MT
        uint32_t  refcnt;
        spin_lock lock;
    } *shared_data;
};

extern "C" {
    void make_shared_stream(shared_stream *p, stream_t srm);
    void make_shared_write_only_stream(shared_stream *dst,
                                       shared_stream *src, bool is_mt);
    bool deref_shared_stream(shared_stream *ptr);
}

}

#endif // LUNAR_SHARED_STREAM_HPP
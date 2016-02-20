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
    static const uint32_t ENABLE_MT    = 0x0010; // stream can be shared among multipe threads
    static const uint32_t SHARED_MT    = 0x0020; // stream is beeing shared among multiple threads

    stream_t stream;
    uint32_t flag;  // READ, WRITE
    
    struct shared_data_t {
        uint32_t  flag_shared; // CLOSED_READ, CLOSED_WRITE, ENABLE_MT, SHARED_MT
        uint32_t  refcnt;
        spin_lock lock;
    } *shared_data;
};

extern "C" {
    void make_shared_stream(shared_stream *p, stream_t srm, bool is_enable_mt);
    void make_shared_write_only_stream(shared_stream *dst,
                                       shared_stream *src, bool is_shared_mt);
    bool deref_shared_stream(shared_stream *ptr);
}

}

#endif // LUNAR_SHARED_STREAM_HPP
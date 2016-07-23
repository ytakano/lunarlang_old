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
    static const uint32_t SOCKET       = 0x0040; // stream socket?

    uint32_t flag;  // READ or WRITE

    struct shared_data_t {
        uint32_t  flag_shared; // CLOSED_READ, CLOSED_WRITE, ENABLE_MT, SHARED_MT, SOCKET
        uint32_t  refcnt;  // for read and write stream
        uint32_t  wrefcnt; // for write strean
        spin_lock lock;
        stream_t  stream;
        shared_stream *readstrm;
    } *shared_data;
};

extern "C" {
    void make_bytes_stream(shared_stream *ronly, shared_stream *wonly, int qlen, int vecsize);
    void make_ptr_stream(shared_stream *ronly, shared_stream *wonly, int qlen);
    void make_fd_stream(shared_stream *ronly, shared_stream *wonly, int fd, bool is_socket);
    void incref_stream(shared_stream *wonly);
    void deref_ptr_stream(shared_stream *ptr);
    void deref_fd_stream(shared_stream *ptr);
}

}

#endif // LUNAR_SHARED_STREAM_HPP
#include "lunar_shared_stream.hpp"
#include "lunar_ringq.hpp"

#include <unistd.h>

#include <sys/socket.h>

namespace lunar {

template <typename T>
void
make_stream(shared_stream *ronly, shared_stream *wonly, int bufsize)
{
    auto p = new shared_stream::shared_data_t;
    
    p->flag_shared = 0;
    p->refcnt      = 2;
    p->wrefcnt     = 1;
    p->stream.ptr  = new ringq<T>(bufsize);
    p->readstrm    = ronly;
    
    ronly->flag        = shared_stream::READ;
    ronly->shared_data = p;

    wonly->flag        = shared_stream::WRITE;
    wonly->shared_data = p;
}

template <typename T>
void
deref_stream(shared_stream *ptr)
{
    ptr->shared_data--;
    if (ptr->shared_data == 0) {
        auto p = (ringq<T>*)ptr->shared_data->stream.ptr;
        delete p;
    } else {
        if (ptr->flag & shared_stream::WRITE) {
            ptr->shared_data->wrefcnt--;
            if (ptr->shared_data->wrefcnt == 0)
                ptr->shared_data->flag_shared |= shared_stream::CLOSED_WRITE;
        } else {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
        }
    }
}

extern "C" {

void
make_ptr_stream(shared_stream *ronly, shared_stream *wonly, int bufsize)
{
    make_stream<void*>(ronly, wonly, bufsize);
}

// before shared_stream is transfered to another thread,
// SHARED_MT flag must be set true
void
make_fd_stream(shared_stream *ronly, shared_stream *wonly, int fd,
               bool is_socket, int bufsize)
{
    auto p = new shared_stream::shared_data_t;
    
    p->flag_shared = shared_stream::ENABLE_MT;
    p->stream.fd   = fd;
    
    if (is_socket)
        p->flag_shared |= shared_stream::SOCKET;

    assert(ronly || wonly);
    
    if (ronly == nullptr) {
        p->refcnt  = 1;
        p->wrefcnt = 1;
        p->flag_shared |= shared_stream::CLOSED_READ;
        
        wonly->flag        = shared_stream::WRITE;
        wonly->shared_data = p;
    } else if (wonly == nullptr) {
        p->refcnt  = 1;
        p->wrefcnt = 0;
        p->flag_shared |= shared_stream::CLOSED_WRITE;
        
        ronly->flag        = shared_stream::READ;
        ronly->shared_data = p;
    } else {
        p->refcnt  = 2;
        p->wrefcnt = 1;
        
        ronly->flag        = shared_stream::READ;
        ronly->shared_data = p;
        
        wonly->flag        = shared_stream::WRITE;
        wonly->shared_data = p;
    }
}

void
deref_ptr_stream(shared_stream *ptr)
{
    deref_stream<void*>(ptr);
}

void
deref_fd_stream(shared_stream *ptr)
{
    if (ptr->shared_data->flag_shared & shared_stream::SHARED_MT) {
        spin_lock_acquire_unsafe lock(ptr->shared_data->lock);
        
        ptr->shared_data->refcnt--;
        if (ptr->shared_data->refcnt == 0) {
            lock.unlock();
            close(ptr->shared_data->stream.fd);
            return;
        }
        
        if (ptr->flag & shared_stream::WRITE) {
            ptr->shared_data->wrefcnt--;
            if (ptr->shared_data->wrefcnt > 0)
                ptr->shared_data->flag_shared |= shared_stream::CLOSED_WRITE;
        } else {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
        }

        lock.unlock();
    } else {
        ptr->shared_data->refcnt--;
        if (ptr->shared_data->refcnt == 0) {
            close(ptr->shared_data->stream.fd);
            return;
        }

        if (ptr->flag & shared_stream::WRITE) {
            ptr->shared_data->wrefcnt--;
            if (ptr->shared_data->wrefcnt > 0) {
                ptr->shared_data->flag_shared |= shared_stream::CLOSED_WRITE;
                if (ptr->shared_data->flag_shared & shared_stream::SOCKET)
                    shutdown(ptr->shared_data->stream.fd, SHUT_WR);
            }
        } else {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
            if (ptr->shared_data->flag_shared & shared_stream::SOCKET)
                shutdown(ptr->shared_data->stream.fd, SHUT_RD);
        }
    }
}

} // extern "C"

} // namespace lunar
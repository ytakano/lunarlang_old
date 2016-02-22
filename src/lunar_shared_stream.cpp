#include "lunar_shared_stream.hpp"
#include "lunar_ringq.hpp"

namespace lunar {

typedef ringq<void*> voidq_t;
typedef ringq<int>   intq_t;

extern "C" {

void
make_ptr_stream(shared_stream *rw, shared_stream *wonly, int bufsize)
{
    auto p = new shared_stream::shared_data_t;
    
    p->flag_shared = 0;
    p->refcnt      = 2;
    p->stream.ptr  = new voidq_t(bufsize);
    
    rw->flag        = shared_stream::READ | shared_stream::WRITE;
    rw->shared_data = p;

    wonly->flag        = shared_stream::WRITE;
    wonly->shared_data = p;
}

// before shared_stream is transfered to another thread,
// SHARED_MT flag must be set true
void
make_fd_stream(shared_stream *rw, shared_stream *wonly, int fd, int bufsize)
{
    auto p = new shared_stream::shared_data_t;
    
    p->flag_shared = shared_stream::ENABLE_MT;
    p->stream.fd   = fd;

    assert(rw || wonly);
    
    if (rw == nullptr) {
        p->refcnt = 1;
        p->flag_shared |= shared_stream::CLOSED_READ;
        
        wonly->flag        = shared_stream::WRITE;
        wonly->shared_data = p;
    } else if (wonly == nullptr) {
        p->refcnt = 1;
        p->flag_shared |= shared_stream::CLOSED_WRITE;
        
        rw->flag        = shared_stream::READ;
        rw->shared_data = p;
    } else {
        p->refcnt = 2;
        
        rw->flag        = shared_stream::READ | shared_stream::WRITE;
        rw->shared_data = p;
        
        wonly->flag        = shared_stream::WRITE;
        wonly->shared_data = p;
    }
}

void
deref_ptr_stream(shared_stream *ptr)
{
    ptr->shared_data--;
    if (ptr->shared_data == 0) {
        auto p = (voidq_t*)ptr->shared_data->stream.ptr;
        delete p;
    } else {
        if (ptr->flag & shared_stream::READ) {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
        }
    }
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
        
        if (ptr->flag & shared_stream::READ) {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
        }

        lock.unlock();
    } else {
        ptr->shared_data->refcnt--;
        if (ptr->shared_data->refcnt == 0) {
            close(ptr->shared_data->stream.fd);
            return;
        }

        if (ptr->flag & shared_stream::READ) {
            ptr->shared_data->flag_shared |= shared_stream::CLOSED_READ;
        }
    }
}

} // extern "C"

} // namespace lunar
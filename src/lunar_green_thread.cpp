#include "lunar_green_thread.hpp"
#include "lunar_rtm_lock.hpp"

#include <sys/ioctl.h>

// currentry, this code can run on X86_64 System V ABI

#ifdef __linux__
#define _setjmp setjmp
#define _longjmp longjmp
#endif // __linux__

namespace lunar {

__thread green_thread *lunar_gt = nullptr;
__thread uint64_t thread_id;

rtm_lock lock_thread2gt;
std::unordered_map<uint64_t, green_thread*> thread2gt;

// stack layout:
//    [empty]
//    context
//    argument
//    func     <- %rsp
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "movq 8(%rsp), %rdi;" // set the argument
    "callq *(%rsp);"      // call func()
    "movq 16(%rsp), %rax;"
    "movl $6, (%rax);"    // context.m_state = STOP
#ifdef __APPLE__
    "call _schedule_green_thread;"  // call _schedule_green_thread
#else  // *BSD, Linux
    "call schedule_green_thread;"   // call schedule_green_thread
#endif // __APPLE__
);

void update_clock();

static volatile uint64_t lunar_clock; // milliseconds
static std::thread thread_clock(update_clock);

void
update_clock()
{
    timespec t0;
    GETTIME(&t0);
    thread_clock.detach();

    for (;;) {
        timespec t1;
        GETTIME(&t1);
        TIMESPECSUB(&t1, &t0);
        lunar_clock = t1.tv_sec * 1000 + t1.tv_nsec * 1e-6;
        usleep(1000);
    }
}

extern "C" {

void
get_streams_ready_green_thread(void ***streams, ssize_t *len)
{
    return lunar_gt->get_streams_ready(streams, len);
}
    
bool
is_timeout_green_thread()
{
    return lunar_gt->is_timeout();
}

bool
is_ready_threadq_green_thread()
{
    return lunar_gt->is_ready_threadq();
}

uint64_t
get_clock()
{
    return lunar_clock;
}

uint64_t
get_thread_id()
{
    return thread_id;
}

void*
get_green_thread(uint64_t thid)
{
    {
        rtm_transaction tr(lock_thread2gt);
        auto it = thread2gt.find(thid);
        if (it == thread2gt.end())
            return nullptr;
        else
            return it->second;
    }
}

bool
init_green_thread(uint64_t thid)
{
    bool result;
    if (lunar_gt == nullptr) {
        lunar_gt = new green_thread;
        rtm_transaction tr(lock_thread2gt);
        if (thread2gt.find(thid) != thread2gt.end()) {
            result = false;
        } else {
            thread2gt[thid] = lunar_gt;
            result = true;
        }
    } else {
        return false;
    }
    
    if (! result) {
        delete lunar_gt;
        return false;
    }
    
    return true;
}

void
schedule_green_thread()
{
    lunar_gt->schedule();
}

void
spawn_green_thread(void (*func)(void*), void *arg)
{
    lunar_gt->spawn(func, arg);
}

void
run_green_thread()
{
    lunar_gt->run();

    {
        rtm_transaction tr(lock_thread2gt);
        thread2gt.erase(get_thread_id());
    }

    delete lunar_gt;
}

#ifdef KQUEUE
void
select_green_thread(struct kevent *kev, int num_kev,
             void * const *stream, int num_stream,
             bool is_threadq, int64_t timeout)
{
    lunar_gt->select_stream(kev, num_kev, stream, num_stream, is_threadq, timeout);
}
#elif (defined EPOLL)
void
select_green_thread(epoll_event *eev, int num_eev,
             void * const *stream, int num_stream,
             bool is_threadq, int64_t timeout)
{
    lunar_gt->select_stream(eev, num_eev, stream, num_stream, is_threadq, timeout);
}
#endif // KQUEUE

STRM_RESULT
push_threadq_green_thread(uint64_t id, void *p)
{
    green_thread *fb;

    {
        rtm_transaction tr(lock_thread2gt);
        auto it = thread2gt.find(id);
        if (it == thread2gt.end()) {
            return STRM_CLOSED;
        } else {
            fb = it->second;
            fb->inc_refcnt_threadq();
        }
    }
    
    auto ret = fb->push_threadq(p);
    fb->dec_refcnt_threadq();
    
    return ret;
}

STRM_RESULT
push_threadq_fast_unsafe_green_thread(void *fb, void *p)
{
    return ((green_thread*)fb)->push_threadq(p);
}

STRM_RESULT
pop_threadq_green_thread(void **p)
{
    return lunar_gt->pop_threadq(p);
}

STRM_RESULT
pop_ptr(void *p, void **ret)
{
    return lunar_gt->pop_stream<void*>((shared_stream*)p, *ret);
}

STRM_RESULT
push_ptr(void *p, void *ret)
{
    return lunar_gt->push_stream<void*>((shared_stream*)p, ret);
}

STRM_RESULT
pop_string(void *p, void **ret)
{
    return lunar_gt->pop_stream<std::u32string*>((shared_stream*)p, *(std::u32string**)ret);
}

STRM_RESULT
push_string(void *p, void *ret)
{
    return lunar_gt->push_stream<std::u32string*>((shared_stream*)p, (std::u32string*)ret);
}

void
push_eof_string(shared_stream *p)
{
    lunar_gt->push_eof_stream<std::u32string*>(p);
}

void
get_fds_ready_green_thread(fdevent_green_thread **events, ssize_t *len)
{
    lunar_gt->get_fds_ready(events, len);
}

} // extern "C"

template<typename T>
STRM_RESULT
green_thread::pop_stream(shared_stream *p, T &ret)
{
    assert(p->flag & shared_stream::READ);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;
    
    auto result = q->pop(ret);
    
    assert(result != STRM_NO_VACANCY);
    
    return result;
}

#define NOTIFY_STREAM(STREAM, QUEUE)                                           \
    do {                                                                       \
        auto it = m_wait_stream.find(QUEUE);                                   \
        if (it != m_wait_stream.end()) {                                       \
            it->second->m_state |= context::SUSPENDING;                        \
            it->second->m_ev_stream.push_back(STREAM->shared_data->readstrm);  \
            m_suspend.push_back(it->second);                                   \
            m_wait_stream.erase(it);                                           \
        }                                                                      \
    } while (0)

template<typename T>
STRM_RESULT
green_thread::push_stream(shared_stream *p, T data)
{
    assert(p->flag & shared_stream::WRITE);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;
    
    if (p->shared_data->flag_shared & shared_stream::CLOSED_READ || q->is_eof()) {
        NOTIFY_STREAM(p, q);
        return STRM_CLOSED;
    }
    
    auto result = q->push(data);
    if (result == STRM_SUCCESS) {
        NOTIFY_STREAM(p, q);
    }

    return result;
}

template<typename T>
void
green_thread::push_eof_stream(shared_stream *p)
{
    assert(p->flag & shared_stream::WRITE);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;
    
    q->push_eof();
    
    if (p->flag & shared_stream::READ) {
        p->shared_data->flag_shared |= shared_stream::CLOSED_READ;
    }

    if (p->flag & shared_stream::WRITE) {
        p->shared_data->flag_shared |= shared_stream::CLOSED_WRITE;
        NOTIFY_STREAM(p, q);
    }
}

green_thread::green_thread(int qsize)
    : m_count(0),
      m_running(nullptr),
      m_wait_thq(nullptr),
      m_threadq(qsize)
{
#ifdef KQUEUE
    for (;;) {
        m_kq = kqueue();
        if (m_kq == -1) {
            if (errno == EINTR) continue;
            PRINTERR("could not create kqueue!");
            exit(-1);
        } else {
            break;
        }
    }
#elif (defined EPOLL)
    for (;;) {
        m_epoll = epoll_create(32);
        if (m_epoll == -1) {
            if (errno == EINTR) continue;
            PRINTERR("could not create epoll!");
            exit(-1);
        } else {
            break;
        }
    }
#endif // KQUEUE
}

green_thread::~green_thread()
{
#ifdef KQUEUE
    for (;;) {
        if (close(m_kq) == -1) {
            if (errno == EINTR) continue;
            PRINTERR("failed close!");
            exit(-1);
        } else {
            break;
        }
    } 
#elif (defined EPOLL)
    for (;;) {
        if (close(m_epoll) == -1) {
            if (errno == EINTR) continue;
            PRINTERR("failed clsoe!");
            exit(-1);
        } else {
            break;
        }
    }
#endif // KQUEUE
}

void
green_thread::select_fd(bool is_block)
{
#ifdef KQUEUE
    auto size = m_wait_fd.size();
    struct kevent *kev = new struct kevent[size + 1];
    
    int ret;
    
    if (is_block) {
        if (m_timeout.empty()) {
            ret = kevent(m_kq, nullptr, 0, kev, size, nullptr);
        } else {
            auto &tout = m_timeout.get<0>();
            auto it = tout.begin();
            
            uint64_t clock = lunar_clock;
            
            if (clock >= it->m_clock) {
                timespec tm;
                tm.tv_sec  = 0;
                tm.tv_nsec = 0;
                for (;;) {
                    ret = kevent(m_kq, nullptr, 0, kev, size, &tm);
                    if (ret == -1) {
                        if (errno == EINTR) continue;
                        PRINTERR("failed kevent!");
                        exit(-1);
                    } else {
                        break;
                    }
                }
            } else {
                intptr_t msec = it->m_clock - clock;
                
                assert(msec > 0);
                
                if (size > 0) {
                    timespec tm;
                    tm.tv_sec = msec * 1e-3;
                    tm.tv_nsec = (msec - tm.tv_sec * 1e3) * 1e-6;
                    for (;;) {
                        ret = kevent(m_kq, nullptr, 0, kev, size, &tm);
                        if (ret == -1) {
                            if (errno == EINTR) continue;
                            PRINTERR("failed kevent!");
                            exit(-1);
                        } else {
                            break;
                        }
                    }
                } else {
                    ret = 0;
                    usleep(msec * 1000);
                }
            }
        }
    } else {
        timespec tm;
        tm.tv_sec  = 0;
        tm.tv_nsec = 0;
        for (;;) {
            ret = kevent(m_kq, nullptr, 0, kev, size, &tm);
            if (ret == -1) {
                if (errno == EINTR) continue;
                PRINTERR("failed kevent!");
                exit(-1);
            } else {
                break;
            }
        }
    }

    for (int i = 0; i < ret; i++) {
        if (kev[i].flags & EV_ERROR) { // report any error
            fprintf(stderr, "error on kevent: %s\n", strerror(kev[i].data));
            continue;
        }
        
        // invoke the green_thread waiting the thread queue
        if (m_wait_thq && m_threadq.get_wait_type() == threadq::QWAIT_PIPE &&
            kev[i].ident == (uintptr_t)m_threadq.get_read_fd() && kev[i].filter == EVFILT_READ) {
            
            if (! (m_wait_thq->m_state & context::SUSPENDING)) {
                m_wait_thq->m_state |= context::SUSPENDING;
                m_suspend.push_back(m_wait_thq);
            }

            m_threadq.set_wait_type(threadq::QWAIT_NONE);
            m_wait_thq = nullptr;
            
            assert(! (kev[i].flags & EV_EOF));
            m_threadq.pop_pipe(kev[i].data);
            
            continue;
        }
        
        auto it = m_wait_fd.find({kev[i].ident, kev[i].filter});
        
        assert (it != m_wait_fd.end());
        
        // invoke the green_threads waiting the file descriptors
        for (auto it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
            if (! ((*it2)->m_state & context::SUSPENDING)) {
                (*it2)->m_state |= context::SUSPENDING;
                m_suspend.push_back(*it2);
            }
            (*it2)->m_events.push_back({it->first.m_fd, it->first.m_event, kev[i].flags, kev[i].fflags, kev[i].data});
        }
        
        m_wait_fd.erase(it);
    }

    delete[] kev;
#elif (defined EPOLL)
    auto size = m_wait_fd.size();
    epoll_event *eev = new epoll_event[size];
    
    int ret;
    
    if (is_block) {
        if (m_timeout.empty()) {
            ret = epoll_wait(m_epoll, eev, size, -1);
        } else {
            auto &tout = m_timeout.get<0>();
            auto it = tout.begin();
            
            uint64_t clock = lunar_clock;
            
            if (clock >= it->m_clock) {
                for (;;) {
                    ret = epoll_wait(m_epoll, eev, size, 0);
                    if (ret == -1) {
                        if (errno == EINTR) continue;
                        PRINTERR("failed kevent!");
                        exit(-1);
                    } else {
                        break;
                    }
                }
            } else {
                intptr_t msec = it->m_clock - clock;
                
                assert(msec > 0);
                
                if (size > 0) {
                    for (;;) {
                        ret = epoll_wait(m_epoll, eev, size, msec);
                        if (ret == -1) {
                            if (errno == EINTR) continue;
                            PRINTERR("failed kevent!");
                            exit(-1);
                        } else {
                            break;
                        }
                    }
                } else {
                    ret = 0;
                    usleep(msec * 1000);
                }
            }
        }
    } else {
        for (;;) {
            ret = epoll_wait(m_epoll, eev, size, 0);
            if (ret == -1) {
                if (errno == EINTR) continue;
                PRINTERR("failed kevent!");
                exit(-1);
            } else {
                break;
            }
        }
    }
    
    auto func = [&](int fd, uint32_t event) {
        auto it = m_wait_fd.find({fd, event});
        assert(it != m_wait_fd.end());

        for (auto it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
            if (! ((*it2)->m_state & context::SUSPENDING)) {
                (*it2)->m_state |= context::SUSPENDING;
                m_suspend.push_back(*it2);
            }
            (*it2)->m_events.push_back({it->first.m_fd, it->first.m_event, 0, 0, 0});
        }
        
        m_wait_fd.erase(it);
    };

    for (int i = 0; i < ret; i++) {
        // invoke the green_thread waiting the thread queue
        if (m_wait_thq && m_threadq.get_wait_type() == threadq::QWAIT_PIPE &&
            eev[i].data.fd == m_threadq.get_read_fd() && (eev[i].events & EPOLLIN)) {
            
            if (! (m_wait_thq->m_state & context::SUSPENDING)) {
                m_wait_thq->m_state |= context::SUSPENDING;
                m_suspend.push_back(m_wait_thq);
            }

            m_threadq.set_wait_type(threadq::QWAIT_NONE);
            m_wait_thq = nullptr;
            
            m_threadq.pop_pipe(1);
            
            continue;
        }
        
        if (eev[i].events & EPOLLIN) {
            func(eev[i].data.fd, EPOLLIN);
        }
        
        if (eev[i].events & EPOLLOUT) {
            func(eev[i].data.fd, EPOLLOUT);
        }

        auto it_in  = m_wait_fd.find({eev[i].data.fd, EPOLLIN});
        auto it_out = m_wait_fd.find({eev[i].data.fd, EPOLLOUT});
        
        epoll_event eev2;
        if (it_in == m_wait_fd.end() && it_out == m_wait_fd.end()) {
            if (epoll_ctl(m_epoll, EPOLL_CTL_DEL, eev[i].data.fd, nullptr) < -1) {
                PRINTERR("failed epoll_ctl!");
                exit(-1);
            }
        } else if (it_in != m_wait_fd.end()) {
            eev2.events  = EPOLLIN;
            eev2.data.fd = eev[i].data.fd;
            if (epoll_ctl(m_epoll, EPOLL_CTL_MOD, eev[i].data.fd, &eev2) < -1) {
                PRINTERR("failed epoll_ctl!");
                exit(-1);
            }
        } else {
            eev2.events  = EPOLLOUT;
            eev2.data.fd = eev[i].data.fd;
            if (epoll_ctl(m_epoll, EPOLL_CTL_MOD, eev[i].data.fd, &eev2) < -1) {
                PRINTERR("failed epoll_ctl!");
                exit(-1);
            }
        }
    }
    
    delete[] eev;
#endif // KQUEUE
}

int
green_thread::spawn(void (*func)(void*), void *arg, int stack_size)
{
    auto ctx = llvm::make_unique<context>();

    for (;;) {
        ++m_count;
        if (m_count > 0) {
            if (m_id2context.find(m_count) == m_id2context.end())
                break;
            else
                continue;
        } else {
            m_count = 1;
        }
    }
    
    ctx->m_id    = m_count;
    ctx->m_state = context::READY;
    ctx->m_stack.resize(stack_size);
    
    auto s = ctx->m_stack.size();
    ctx->m_stack[s - 2] = (uint64_t)ctx.get(); // push context
    ctx->m_stack[s - 3] = (uint64_t)arg;       // push argument
    ctx->m_stack[s - 4] = (uint64_t)func;      // push func
    
    m_suspend.push_back(ctx.get());
    m_id2context[m_count] = std::move(ctx);
    
    return 0;
}

void
green_thread::run()
{
    if (_setjmp(m_jmp_buf) == 0) {
        schedule();
    }
}

void
green_thread::resume_timeout()
{
    uint64_t now = lunar_clock;

    auto &tout = m_timeout.get<0>();
    for (auto it = tout.begin(); it != tout.end(); ) {
        if (it->m_clock > now)
            break;

        it->m_ctx->m_state |= context::SUSPENDING;
        it->m_ctx->m_is_ev_timeout = true;
        m_suspend.push_back(it->m_ctx);
        
        tout.erase(it++);
    }
}

void
green_thread::schedule()
{
    if (! m_wait_fd.empty())
        select_fd(false);

    for (;;) {
        context *ctx = nullptr;
        
        if (m_running) {
            ctx = m_running;
            if (m_running->m_state == context::RUNNING) {
                m_running->m_state = context::SUSPENDING;
                m_suspend.push_back(m_running);
            }
        }
        
        if (! m_timeout.empty())
            resume_timeout();

        if (m_wait_thq && m_threadq.m_qwait_type == threadq::QWAIT_NONE && m_threadq.get_len() > 0) {
            if (! (m_wait_thq->m_state & context::SUSPENDING)) {
                m_wait_thq->m_state |= context::SUSPENDING;
                m_suspend.push_back(m_wait_thq);
            }

            m_wait_thq->m_is_ev_thq = true;
            m_wait_thq = nullptr;
        }
        
        // invoke READY state thread
        if (! m_suspend.empty()) {
            m_running = m_suspend.front();
            auto state = m_running->m_state;
            m_running->m_state = context::RUNNING;
            m_suspend.pop_front();
            
            if (state & context::READY) {
                if (ctx) {
                    if (_setjmp(ctx->m_jmp_buf) == 0) {
                        auto p = &m_running->m_stack[m_running->m_stack.size() - 4];
                        asm (
                            "movq %0, %%rsp;" // set stack pointer
                            "movq %0, %%rbp;" // set frame pointer
                            "jmp ___INVOKE;"
                            :
                            : "r" (p)
                        );
                    } else {
                        return;
                    }
                } else {
                    auto p = &m_running->m_stack[m_running->m_stack.size() - 4];
                    asm (
                        "movq %0, %%rsp;" // set stack pointer
                        "movq %0, %%rbp;" // set frame pointer
                        "jmp ___INVOKE;"
                        :
                        : "r" (p)
                    );
                }
            } else {
                // remove the context from wait queues
                if (! m_running->m_fd.empty()) {
#ifdef KQUEUE
                    struct kevent *kev = new struct kevent[m_running->m_fd.size()];

                    int i = 0;
                    for (auto &ev: m_running->m_fd) {
                        auto it = m_wait_fd.find(ev);
                        if (it == m_wait_fd.end())
                            continue;

                        it->second.erase(m_running);
                        
                        if (it->second.empty()) {
                            m_wait_fd.erase(it);
                            EV_SET(&kev[i++], ev.m_fd, ev.m_event, EV_DELETE, 0, 0, nullptr);
                        }
                    }
                    
                    if (i > 0) {
                        if(kevent(m_kq, kev, i, nullptr, 0, nullptr) == -1) {
                            PRINTERR("failed kevent!");
                            exit(-1);
                        }
                    }

                    delete[] kev;
#elif (defined EPOLL)
                    std::vector<int> fds;
                    
                    for (auto &ev: m_running->m_fd) {
                        auto it = m_wait_fd.find(ev);
                        if (it == m_wait_fd.end())
                            continue;
                        
                        it->second.erase(m_running);
                        
                        if (it->second.empty())
                            m_wait_fd.erase(it);
                        
                        fds.push_back(ev.m_fd);
                    }
                    
                    for (int fd: fds) {
                        auto it_in  = m_wait_fd.find({fd, EPOLLIN});
                        auto it_out = m_wait_fd.find({fd, EPOLLOUT});
                        
                        epoll_event eev;
                        if (it_in == m_wait_fd.end() && it_out == m_wait_fd.end()) {
                            for (;;) {
                                if (epoll_ctl(m_epoll, EPOLL_CTL_DEL, fd, nullptr) < -1) {
                                    if (errno == EINTR) continue;
                                    PRINTERR("failed epoll_ctl!");
                                    exit(-1);
                                } else {
                                    break; 
                                }
                            }
                        } else if (it_in != m_wait_fd.end()) {
                            eev.events  = EPOLLIN;
                            eev.data.fd = fd;
                            for (;;) {
                                if (epoll_ctl(m_epoll, EPOLL_CTL_MOD, fd, &eev) < -1) {
                                    if (errno == EINTR) continue;
                                    PRINTERR("failed epoll_ctl!");
                                    exit(-1);
                                } else {
                                    break;
                                }
                            }
                        } else {
                            eev.events  = EPOLLOUT;
                            eev.data.fd = fd;
                            for (;;) {
                                if (epoll_ctl(m_epoll, EPOLL_CTL_MOD, fd, &eev) < -1) {
                                    if (errno == EINTR) continue;
                                    PRINTERR("failed epoll_ctl!");
                                    exit(-1);
                                } else {
                                    break;
                                }
                            }
                        }
                    }
#endif // KQUEUE

                    m_running->m_fd.clear();
                }
                
                for (auto strm: m_running->m_stream) {
                    m_wait_stream.erase(strm);
                }
                
                m_running->m_stream.clear();
                
                if (state & context::WAITING_TIMEOUT)
                    m_timeout.get<1>().erase(m_running);

                if (state & context::WAITING_THQ) {
                    
                    spin_lock_acquire_unsafe lock(m_threadq.m_qlock);
                    if (m_threadq.m_qwait_type == threadq::QWAIT_PIPE) {
                        m_threadq.m_qwait_type = threadq::QWAIT_NONE;
                        lock.unlock();
                        
                        if (m_threadq.m_qlen > 0) {
                            m_running->m_is_ev_thq = true;
                            uint8_t buf[32];
                            while (read(m_threadq.m_qpipe[0], buf, sizeof(buf)) > 0);
                        }
#ifdef KQUEUE
                        struct kevent kev;
                        EV_SET(&kev, m_threadq.m_qpipe[0], EVFILT_READ, EV_DELETE, 0, 0, nullptr);
                        if (kevent(m_kq, &kev, 1, nullptr, 0, nullptr) == -1) {
                            PRINTERR("failed kevent!");
                            exit(-1);
                        }
#elif (defined EPOLL)
                        if (epoll_ctl(m_epoll, EPOLL_CTL_DEL, m_threadq.m_qpipe[0], nullptr) < -1) {
                            PRINTERR("failed epoll_ctl!");
                            exit(-1);
                        }
#endif // KQUEUE
                    } else {
                        lock.unlock();
                    }
                    
                    m_wait_thq = nullptr;
                }
                
                if (ctx == m_running)
                    return;
                
                if (_setjmp(ctx->m_jmp_buf) == 0) {
                    _longjmp(m_running->m_jmp_buf, 1);
                } else {
                    return;
                }
            }
        }
        
        if (m_wait_thq) {
            spin_lock_acquire_unsafe lock(m_threadq.m_qlock);
            if (m_threadq.m_qlen > 0) {
                lock.unlock();

                if (! (m_wait_thq->m_state & context::SUSPENDING)) {
                    m_wait_thq->m_state |= context::SUSPENDING;
                    m_suspend.push_back(m_wait_thq);
                }

                m_wait_thq->m_is_ev_thq = true;
                m_wait_thq = nullptr;
                continue;
            } else {
                m_threadq.m_is_qnotified = false;
                if (m_wait_fd.empty() && m_timeout.empty()) {
                    m_threadq.m_qwait_type = threadq::QWAIT_COND;
                    lock.unlock();
                    // wait the notification via condition wait
                    {
                        std::unique_lock<std::mutex> mlock(m_threadq.m_qmutex);
                        if (m_threadq.m_qlen == 0)
                            m_threadq.m_qcond.wait(mlock);
                        
                        m_threadq.m_qwait_type = threadq::QWAIT_NONE;
                    }

                    if (! (m_wait_thq->m_state & context::SUSPENDING)) {
                        m_wait_thq->m_state |= context::SUSPENDING;
                        m_suspend.push_back(m_wait_thq);
                    }

                    m_wait_thq->m_is_ev_thq = true;
                    m_wait_thq = nullptr;
                    continue;
                } else {
                    // wait the notificication via pipe
                    m_threadq.m_qwait_type = threadq::QWAIT_PIPE;
                    lock.unlock();
                    
#ifdef KQUEUE
                    struct kevent kev;
                    EV_SET(&kev, m_threadq.m_qpipe[0], EVFILT_READ, EV_ADD | EV_ENABLE | EV_ONESHOT, 0, 0, NULL);
                    for (;;) {
                        if (kevent(m_kq, &kev, 1, nullptr, 0, nullptr) == -1) {
                            if (errno == EINTR) continue;
                            PRINTERR("failed kevent!");
                            exit(-1);
                        } else {
                            break;
                        }
                    }
#elif (defined EPOLL)
                    epoll_event eev;
                    eev.data.fd = m_threadq.m_qpipe[0];
                    eev.events  = EPOLLIN;
                    for (;;) {
                        if (epoll_ctl(m_epoll, EPOLL_CTL_ADD, m_threadq.m_qpipe[0], &eev) < -1) {
                            if (errno == EINTR) continue;
                            PRINTERR("failed epoll_ctl!");
                            exit(-1);
                        } else {
                            break;
                        }
                    }
#endif // KQUEUE
                }
            }
        } else if (m_wait_fd.empty() && m_timeout.empty()) {
            break;
        }

        for (;;) {
            select_fd(true);
            if (! m_timeout.empty())
                resume_timeout();
            if (! m_suspend.empty())
                break;
        }
    }

    _longjmp(m_jmp_buf, 1);
}

#if (defined KQUEUE)
void
green_thread::select_stream(struct kevent *kev, int num_kev,
                     void * const *stream, int num_stream,
                     bool is_threadq, int64_t timeout)
#elif (defined EPOLL) // #if (defined KQUEUE)
void
green_thread::select_stream(epoll_event *eev, int num_eev,
                     void * const *stream, int num_stream,
                     bool is_threadq, int64_t timeout)
#endif // #if (defined KQUEUE)
{
    m_running->m_state = 0;
    m_running->m_events.clear();
    m_running->m_ev_stream.clear();
    m_running->m_is_ev_thq = false;
    m_running->m_is_ev_timeout = false;
    
    if (timeout) {
        m_running->m_state |= context::WAITING_TIMEOUT;
        m_timeout.insert(ctx_time(lunar_clock + (uint64_t)timeout, m_running));
    }

#ifdef KQUEUE
    if (num_kev > 0) {
        m_running->m_state |= context::WAITING_FD;
        for (;;) {
            if (kevent(m_kq, kev, num_kev, NULL, 0, NULL) == -1) {
                if (errno == EINTR) continue;
                PRINTERR("could not set events to kqueue!");
                exit(-1);
            } else {
                break;
            }
        }

        for (int i = 0; i < num_kev; i++) {
            m_wait_fd[{kev[i].ident, kev[i].filter}].insert(m_running);
            m_running->m_fd.push_back({kev[i].ident, kev[i].filter});
        }
    }
#elif (defined EPOLL)
    if (num_eev > 0) {
        m_running->m_state |= context::WAITING_FD;
        for (int i = 0; i < num_eev; i++) {
            auto it_in  = m_wait_fd.find({eev[i].data.fd, EPOLLIN});
            auto it_out = m_wait_fd.find({eev[i].data.fd, EPOLLOUT});

            if (it_in == m_wait_fd.end() && it_out == m_wait_fd.end()) {
                for (;;) {
                    if (epoll_ctl(m_epoll, EPOLL_CTL_ADD, eev[i].data.fd, &eev[i]) < 0) {
                        if (errno == EINTR) continue;
                        PRINTERR("failed epoll_ctl!");
                        exit(-1);
                    } else {
                        break;
                    }
                }
            } else if ((it_out != m_wait_fd.end() && eev[i].events == EPOLLIN) ||
                       (it_in != m_wait_fd.end() && eev[i].events == EPOLLOUT)) {
                epoll_event e = eev[i];
                e.events = EPOLLIN | EPOLLOUT;
                for (;;) {
                    if (epoll_ctl(m_epoll, EPOLL_CTL_MOD, e.data.fd, &e) < -1) {
                        if (errno == EINTR) continue;
                        PRINTERR("failed epoll_ctl");
                        exit(-1);
                    }
                }
            }

            m_wait_fd[{eev[i].data.fd, eev[i].events}].insert(m_running);
            m_running->m_fd.push_back({eev[i].data.fd, eev[i].events});
        }
    }
#endif // KQUEUE

    if (num_stream) {
        m_running->m_state |= context::WAITING_STREAM;
        for (int i = 0; i < num_stream; i++) {
            void *s = ((shared_stream*)stream[i])->shared_data->stream.ptr;
            assert(((shared_stream*)stream[i])->flag & shared_stream::READ);
            m_wait_stream[s] = m_running;
            m_running->m_stream.push_back(s);
        }
    }
    
    if (is_threadq) {
        assert(m_wait_thq == nullptr);
        m_wait_thq = m_running;
        m_wait_thq->m_state |= context::WAITING_THQ;
    }
    
    if (m_running->m_state == 0) {
        m_running->m_state = context::SUSPENDING;
        m_suspend.push_back(m_running);
    }
    
    schedule();
}

green_thread::threadq::threadq(int qsize)
    : m_qlen(0),
      m_refcnt(0),
      m_is_qnotified(true),
      m_qwait_type(threadq::QWAIT_NONE),
      m_max_qlen(qsize),
      m_q(new void*[qsize]),
      m_qend(m_q + qsize),
      m_qhead(m_q),
      m_qtail(m_q)
{
    if (pipe(m_qpipe) == -1) {
        PRINTERR("could not create pipe!");
        exit(-1);
    }

    int val = 1;
    ioctl(m_qpipe[0], FIONBIO, &val);
}

green_thread::threadq::~threadq()
{
    while (m_refcnt);
    
    delete[] m_q;

    for (;;) {
        if (close(m_qpipe[0]) < 0) {
            if (errno == EINTR) continue;
            PRINTERR("failed close!\n");
            exit(-1);
        } else {
            break;
        }
    }

    for (;;) {
        if (close(m_qpipe[1]) < 0) {
            if (errno == EINTR) continue;
            PRINTERR("failed colse!\n");
            exit(-1);
        } else {
            break;
        }
    }
}

}
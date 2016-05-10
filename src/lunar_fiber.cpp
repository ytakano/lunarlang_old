#include "lunar_fiber.hpp"
#include "lunar_rtm_lock.hpp"

// currentry, this code can run on X86_64 System V ABI

#ifdef KQUEUE
    #define FD_EV_READ             EVFILT_READ
    #define FD_EV_WRITE            EVFILT_WRITE
    #define FD_EV_VNODE            EVFILT_VNODE
    #define FD_EV_PROC             EVFILT_PROC
    #define FD_EV_SIGNAL           EVFILT_SIGNAL
    #define FD_EV_USER             EVFILT_USER
#ifdef __APPLE__
    #define FD_EV_MACHPORT         EVFILT_MACHPORT
#endif // __APPLE__
    
    // for read or write events
    #define FD_EV_FLAG_EOF         EV_EOF

    // for files
    #define FD_EV_FFLAG_DELETE     NOTE_DELETE
    #define FD_EV_FFLAG_WRITE      NOTE_WRITE
    #define FD_EV_FFLAG_EXTEND     NOTE_EXTEND
    #define FD_EV_FFLAG_ATTRIB     NOTE_ATTRIB
    #define FD_EV_FFLAG_LINK       NOTE_LINK
    #define FD_EV_FFLAG_RENAME     NOTE_RENAME
    #define FD_EV_FFLAG_REVOKE     NOTE_REVOKE

    // for processes
    #define FD_EV_FFLAG_EXIT       NOTE_EXIT
    #define FD_EV_FFLAG_FORK       NOTE_FORK
    #define FD_EV_FFLAG_EXEC       NOTE_EXEC
#ifdef __APPLE__
    #define FD_EV_FFLAG_EXITSTATUS NOTE_EXITSTATUS
    #define FD_EV_FFLAG_SIGNAL     NOTE_SIGNAL
    #define FD_EV_FFLAG_REAP       NOTE_REAP
#else
    #define FD_EV_FFLAG_TRAC       NOTE_TRACK
#endif // __APPLE__

#endif // KQUEUE

#ifdef EPOLL
    #define FD_EV_READ             1
    #define FD_EV_WRITE            2
    #define FD_EV_VNODE            3
    #define FD_EV_PROC             4
    #define FD_EV_SIGNAL           5
    #define FD_EV_USER             6
    
    // for read or write events
    #define FD_EV_FLAG_EOF         1

    // for files
    #define FD_EV_FFLAG_DELETE     0x0001
    #define FD_EV_FFLAG_WRITE      0x0002
    #define FD_EV_FFLAG_EXTEND     0x0004
    #define FD_EV_FFLAG_ATTRIB     0x0008
    #define FD_EV_FFLAG_LINK       0x0010
    #define FD_EV_FFLAG_RENAME     0x0020
    #define FD_EV_FFLAG_REVOKE     0x0040

    // for processes
    #define FD_EV_FFLAG_EXIT       0x0080
    #define FD_EV_FFLAG_FORK       0x0100
    #define FD_EV_FFLAG_EXEC       0x0200
    #define FD_EV_FFLAG_EXITSTATUS 0x0400
    #define FD_EV_FFLAG_SIGNAL     0x0800
    #define FD_EV_FFLAG_REAP       0x1000
    #define FD_EV_FFLAG_TRAC       0x2000
#endif // EPOLL

namespace lunar {

__thread fiber *lunar_gt = nullptr;

rtm_lock lock_thread2gt;
std::unordered_map<std::thread::id, fiber*> thread2gt;

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
    "call _yield_fiber;"  // call _yeild_fiber
#else  // *BSD, Linux
    "call yield_fiber;"   // call yeild_fiber
#endif // __APPLE__
);

extern "C" {

void
init_fiber()
{
    if (lunar_gt == nullptr) {
        lunar_gt = new fiber;
        rtm_transaction tr(lock_thread2gt);
        thread2gt[std::this_thread::get_id()] = lunar_gt;
    }
}

void
yield_fiber()
{
    lunar_gt->yield();
}

void
spawn_fiber(void (*func)(void*), void *arg)
{
    lunar_gt->spawn(func, arg);
}

void
run_fiber()
{
    lunar_gt->run();

    {
        rtm_transaction tr(lock_thread2gt);
        thread2gt.erase(std::this_thread::get_id());
    }

    delete lunar_gt;
}

STRM_RESULT
push_threadq_fiber(std::thread::id id, void *p)
{
    fiber *fb;

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
push_threadq_fast_unsafe_fiber(fiber *fb, void *p)
{
    return fb->push_threadq(p);
}

/*
void
wait_fd_read_fiber(int fd)
{
    lunar_gt->wait_fd_read(fd);
}

void
wait_fd_write_fiber(int fd)
{
    lunar_gt->wait_fd_write(fd);
}

STRM_RESULT pop_string(shared_stream *p, std::u32string **ret, bool is_yield)
{
    return lunar_gt->pop_stream<std::u32string*>(p, *ret, is_yield);
}

STRM_RESULT push_string(shared_stream *p, std::u32string *ret)
{
    return lunar_gt->push_stream<std::u32string*>(p, ret);
}

void push_eof_string(shared_stream *p)
{
    return lunar_gt->push_eof_stream<std::u32string*>(p);
}
*/

} // extern "C"

/*
void
fiber::wait_fd_read(int fd)
{
    m_running->m_state = context::WAITING_FD;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

void
fiber::wait_fd_write(int fd)
{
    m_running->m_state = context::WAITING_FD;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

template<typename T>
STRM_RESULT
fiber::push_stream(shared_stream *p, T data)
{
    assert(p->flag & shared_stream::WRITE);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;
    
    if (p->shared_data->flag_shared & shared_stream::CLOSED_READ || q->is_eof())
        return STRM_CLOSED;
    
    for (;;) {
        auto result = q->push(data);
        if (result == STRM_SUCCESS) {
            auto it = m_wait_stream.find(q);
            if (it != m_wait_stream.end()) {
                // notify
                it->second->m_state = context::SUSPENDING;
                m_suspend.push_back(it->second);
                m_wait_stream.erase(it);
            }
            return STRM_SUCCESS;
        } else if (result == STRM_CLOSED) {
            return STRM_CLOSED;
        } else {
            yield();
        }
    }
}

template<typename T>
void
fiber::push_eof_stream(shared_stream *p)
{
    assert(p->flag & shared_stream::WRITE);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;
    
    q->push_eof();
    
    if (p->flag & shared_stream::READ) {
        p->shared_data->flag_shared |= shared_stream::CLOSED_READ;
    }

    if (p->flag & shared_stream::WRITE) {
        p->shared_data->flag_shared |= shared_stream::CLOSED_WRITE;
    }
}
*/

fiber::fiber(int qsize)
    : m_count(0),
      m_running(nullptr),
      m_wait_thq(nullptr),
      m_threadq(qsize)
{
#ifdef KQUEUE
    m_kq = kqueue();
    if (m_kq == -1) {
        PRINTERR("could not create kqueue!");
        exit(-1);
    }
#endif // KQUEUE
}

fiber::~fiber()
{
#ifdef KQUEUE
    close(m_kq);
#endif // KQUEUE
}

void
fiber::select_fd(bool is_block)
{
#ifdef KQUEUE
    auto size = m_wait_fd.size();
    if (size == 0)
        return;
    
    struct kevent *kev = new struct kevent[size];
    
    int ret;
    
    if (is_block) {
        // TODO: check timeout
        ret = kevent(m_kq, nullptr, 0, kev, size, NULL);
    } else {
        timespec tm;
        tm.tv_sec  = 0;
        tm.tv_nsec = 0;
        ret = kevent(m_kq, nullptr, 0, kev, size, &tm);
    }

    if (ret == -1) {
        PRINTERR("failed kevent");
        exit(-1);
    }
    
    for (int i = 0; i < ret; i++) {
        if (kev[i].flags & EV_ERROR) { // report any error
            fprintf(stderr, "error on kevent: %s\n", strerror(kev[i].data));
            continue;
        }

        auto it = m_wait_fd.find({kev[i].ident, kev[i].filter});
        
        assert (it != m_wait_fd.end());
        
        for (auto it2 = it->second.begin(); it2 != it->second.end(); ++it2) {
            if (! ((*it2)->m_state & context::SUSPENDING)) {
                (*it2)->m_state |= context::SUSPENDING;
                m_suspend.push_back(*it2);
            }
            (*it2)->m_events.insert({it->first, {kev[i].flags, kev[i].fflags, kev[i].data}});
            (*it2)->m_fd.erase(it->first);
        }
        
        m_wait_fd.erase(it);
    }

    delete[] kev;
#endif // KQUEUE

#ifdef EPOLL
#endif // EPOLL

}

int
fiber::spawn(void (*func)(void*), void *arg, int stack_size)
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
    
    ctx->m_id      = m_count;
    ctx->m_state   = context::READY;
    ctx->m_stack.resize(stack_size);
    
    auto s = ctx->m_stack.size();
    ctx->m_stack[s - 2] = (uint64_t)ctx.get(); // push context
    ctx->m_stack[s - 3] = (uint64_t)arg;       // push argument
    ctx->m_stack[s - 4] = (uint64_t)func;      // push func
    
    m_ready.push_back(ctx.get());
    m_id2context[m_count] = std::move(ctx);
    
    return 0;
}

void
fiber::run()
{
    if (setjmp(m_jmp_buf) == 0) {
        yield();
    }
}

void
fiber::yield()
{
    for (;;) {
        context *ctx = nullptr;
        
        if (m_running && m_running->m_state == context::RUNNING) {
            m_running->m_state = context::SUSPENDING;
            m_suspend.push_back(m_running);
            ctx = m_running;
        }

        // invoke READY state thread
        if (! m_ready.empty()) {
            m_running = m_ready.front();
            m_running->m_state = context::RUNNING;
            m_ready.pop_front();
            if (ctx != nullptr) {
                if (setjmp(ctx->m_jmp_buf) == 0) {
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
        }
    }

/*
    for (;;) {
        context *ctx = nullptr;
        bool flag = true;
        
        if (m_running) {
            if (m_running->m_state == context::STOP) {
                m_id2context.erase(m_running->m_id);
                m_running = nullptr;
            } else if (m_running->m_state == context::WAITING_FD) {
                ctx = m_running;
                m_wait_fd[m_running->m_fd] = m_running;
            } else if (m_running->m_state == context::WAITING_STREAM) {
                ctx = m_running;
            } else if (m_running->m_state == context::WAITING_THQ) {
                ctx = m_running;
                m_threadq = m_running;
            } else if (m_running->m_state == context::RUNNING) {
                ctx = m_running;
                m_running->m_state = context::SUSPENDING;
                m_suspend.push_back(m_running);
                
                flag = false;
                if (m_qlen > 0 && m_threadq) {
                    m_threadq->m_state = context::SUSPENDING;
                    m_suspend.push_back(m_threadq);
                    m_threadq = nullptr;
                }
        
                select_fd(false);
            }
        }
    
        // invoke READY state thread
        if (! m_ready.empty()) {
            m_running = m_ready.front();
            m_running->m_state = context::RUNNING;
            m_ready.pop_front();
            if (ctx != nullptr) {
                if (setjmp(ctx->m_jmp_buf) == 0) {
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
        }
        
        if (flag) {
            if (m_qlen > 0 && m_threadq) {
                m_threadq->m_state = context::SUSPENDING;
                m_suspend.push_back(m_threadq);
                m_threadq = nullptr;
            }
        
            select_fd(false);
        }
    
        // invoke SUSPEND state thread
        if (! m_suspend.empty()) {
            m_running = m_suspend.front();
            m_running->m_state = context::RUNNING;
            m_suspend.pop_front();
            if (ctx != nullptr) {
                if (setjmp(ctx->m_jmp_buf) == 0) {
                    longjmp(m_running->m_jmp_buf, 1);
                } else {
                    return;
                }
            } else {
                longjmp(m_running->m_jmp_buf, 1);
            }
        }
        
        if (m_threadq) {
            spin_lock_acquire_unsafe lock(m_qlock);
            if (m_qlen > 0) {
                // invoke WAITING_THQ state thread
                lock.unlock();
                m_running = m_threadq;
                m_running->m_state = context::RUNNING;
                m_threadq = nullptr;
                if (ctx != nullptr) {
                    if (setjmp(ctx->m_jmp_buf) == 0) {
                        longjmp(m_running->m_jmp_buf, 1);
                    } else {
                        return;
                    }
                } else {
                    longjmp(m_running->m_jmp_buf, 1);
                }
            } else {
                // suspend
                m_is_qnotified = false;
                if (m_wait_fd.empty()) {
                    m_qwait_type = QWAIT_COND;
                    lock.unlock();
                    // condition wait
                    {
                        std::unique_lock<std::mutex> mlock(m_qmutex);
                        if (m_qlen == 0)
                            m_qcond.wait(mlock);
                    }

                    m_running = m_threadq;
                    m_running->m_state = context::RUNNING;
                    m_threadq = nullptr;
                    if (ctx != nullptr) {
                        if (setjmp(ctx->m_jmp_buf) == 0) {
                            longjmp(m_running->m_jmp_buf, 1);
                        } else {
                            return;
                        }
                    } else {
                        longjmp(m_running->m_jmp_buf, 1);
                    }
                } else {
                    m_qwait_type = QWAIT_PIPE;
                    lock.unlock();

                    m_threadq->m_state = context::WAITING_FD;
                    m_threadq->m_fd    = m_qpipe[0];

#ifdef KQUEUE
                    EV_SET(&m_kev, m_qpipe[0], EVFILT_READ, EV_ADD, 0, 0, NULL);
#endif // KQUEUE
                    m_wait_fd[m_qpipe[0]] = m_threadq;
                    m_threadq = nullptr;
                }
            }
        }
        
        if (m_wait_fd.empty())
            break;

        select_fd(true);
    }
    
    longjmp(m_jmp_buf, 1);

*/
}

#if (defined KQUEUE)
void
fiber::select_stream(struct kevent *kev, int num_kev,
                     void * const *stream, int num_stream,
                     bool &is_threadq, int64_t timeout)
#elif (defined EPOLL) // #if (defined KQUEUE)
fiber::select_stream(
                     void * const *stream, int num_stream,
                     bool &is_threadq, int64_t timeout)
#endif // #if (defined KQUEUE)
{
    m_running->m_state = 0;

    if (timeout) {
        timeval tv;
        gettimeofday(&tv, nullptr);
        
        double t = tv.tv_sec + tv.tv_usec * 0.000001 + timeout * 0.001;

        m_running->m_state |= context::WAITING_TIMEOUT;
        m_timeout.insert(ctx_time(t, m_running));
    }

#ifdef KQUEUE
    if (num_kev > 0) {
        if (kevent(m_kq, kev, num_kev, NULL, 0, NULL) == -1) {
            PRINTERR("could not set events to kqueue!");
            exit(-1);
        }
        for (int i = 0; i < num_kev; i++) {
            m_wait_fd[{kev[i].ident, kev[i].filter}].insert(m_running);
            m_running->m_fd.insert({kev[i].ident, kev[i].filter});
        }
    }
#endif // KQUEUE

#ifdef EPOLL
#endif // EPOLL


    if (num_stream) {
        m_running->m_state |= context::WAITING_STREAM;
        for (int i = 0; i < num_stream; i++) {
            void *s = stream[i];
            m_wait_stream[s] = m_running;
            m_running->m_stream.insert(s);
        }
    }
    
    if (is_threadq) {
        assert(m_wait_thq == nullptr);
        m_wait_thq = m_running;
        m_wait_thq->m_is_threadq  = true;
        m_wait_thq->m_state |= context::WAITING_THQ;
    }
    
    if (m_running->m_state == 0) {
        m_running->m_state = context::SUSPENDING;
        m_suspend.push_back(m_running);
    }
    
    yield();
}

fiber::threadq::threadq(int qsize)
    : m_qlen(0),
      m_refcnt(0),
      m_is_qnotified(true),
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
}

fiber::threadq::~threadq()
{
    while (m_refcnt);
    
    delete[] m_q;

    close(m_qpipe[0]);
    close(m_qpipe[1]);
}

STRM_RESULT
fiber::threadq::push(void *p)
{
    if (m_qlen == m_max_qlen) 
        return STRM_NO_VACANCY;

    spin_lock_acquire_unsafe lock(m_qlock);

    *m_qtail = p;
    m_qlen++;
    m_qtail++;

    if (m_qtail == m_qend) {
        m_qtail = m_q;
    }
    
    if (! m_is_qnotified) {
        m_is_qnotified = true;
        lock.unlock();
        if (m_qwait_type == QWAIT_COND) {
            std::unique_lock<std::mutex> mlock(m_qmutex);
            m_qcond.notify_one();
        } else {
            char c = '\0';
            write(m_qpipe[1], &c, sizeof(c));
        }
        
        return STRM_SUCCESS;
    }
    
    lock.unlock();
    
    return STRM_SUCCESS;
}

STRM_RESULT
fiber::threadq::pop(void **p)
{
    if (m_qlen == 0)
        return STRM_NO_MORE_DATA;

    *p = *m_qhead;

    {
        spin_lock_acquire lock(m_qlock);
        m_qlen--;
    }

    m_qhead++;

    if (m_qhead == m_qend) {
        m_qhead = m_q;
    }

    return STRM_SUCCESS;
}

}
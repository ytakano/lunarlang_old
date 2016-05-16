#include "lunar_green_thread.hpp"
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

bool
is_timeout_green_thread()
{
    return lunar_gt->is_timeout();
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

void
select_green_thread(struct kevent *kev, int num_kev,
             void * const *stream, int num_stream,
             bool is_threadq, int64_t timeout)
{
    lunar_gt->select_stream(kev, num_kev, stream, num_stream, is_threadq, timeout);
}

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
pop_ptr(shared_stream *p, void **ret)
{
    return lunar_gt->pop_stream<void*>(p, *ret);
}

STRM_RESULT
push_ptr(shared_stream *p, void *ret)
{
    return lunar_gt->push_stream<void*>(p, ret);
}

STRM_RESULT
pop_string(shared_stream *p, void **ret)
{
    return lunar_gt->pop_stream<std::u32string*>(p, *(std::u32string**)ret);
}

STRM_RESULT
push_string(shared_stream *p, void *ret)
{
    return lunar_gt->push_stream<std::u32string*>(p, (std::u32string*)ret);
}

void
push_eof_string(shared_stream *p)
{
    return lunar_gt->push_eof_stream<std::u32string*>(p);
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

#define NOTIFY_STREAM(STREAM, QUEUE)                       \
    do {                                                   \
        auto it = m_wait_stream.find(QUEUE);               \
        if (it != m_wait_stream.end()) {                   \
            it->second->m_state |= context::SUSPENDING;    \
            it->second->m_ev_stream.push_back(STREAM);     \
            m_suspend.push_back(it->second);               \
            m_wait_stream.erase(it);                       \
        }                                                  \
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
    m_kq = kqueue();
    if (m_kq == -1) {
        PRINTERR("could not create kqueue!");
        exit(-1);
    }
#elif (defined EPOLL)
    epoll_create(32);
    if (m_epoll == -1) {
        PRINTERR("could not create epoll!");
        exit(-1);
    }
#endif // KQUEUE
}

green_thread::~green_thread()
{
#ifdef KQUEUE
    close(m_kq);
#elif (defined EPOLL)
    close(m_epoll);
#endif // KQUEUE
}

void
green_thread::select_fd(bool is_block)
{
    auto size = m_wait_fd.size();
    struct kevent *kev = new struct kevent[size + 1];
    
    int ret;
    int is_timer = false;
    
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
                ret = kevent(m_kq, nullptr, 0, kev, size, &tm);
            } else {
                is_timer = true;
                size++;
                
                struct kevent kevtimer;
                intptr_t msec = it->m_clock - clock;
                
                assert(msec > 0);
                
                EV_SET(&kevtimer, 1, EVFILT_TIMER, EV_ADD | EV_ENABLE | EV_ONESHOT, 0, msec, 0);

                ret = kevent(m_kq, &kevtimer, 1, kev, size, nullptr);
            }
        }
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
        
        if (kev[i].filter == EVFILT_TIMER) {
            is_timer = false;
            assert(kev[i].ident == 1);
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
            (*it2)->m_events.push_back({it->first, {kev[i].flags, kev[i].fflags, kev[i].data}});
        }
        
        m_wait_fd.erase(it);
    }
    
    if (is_timer) {
        struct kevent kevdel;
        EV_SET(&kevdel, 1, EVFILT_TIMER, EV_ADD | EV_DELETE, 0, 0, 0);
        if (kevent(m_kq, &kevdel, 1, nullptr, 0, nullptr) < 0) {
            PRINTERR("failed kevent");
            exit(-1);
        }
    }

    delete[] kev;
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
        if (it->m_clock >= now)
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
    for (;;) {
        context *ctx = nullptr;
        
        if (m_running) {
            ctx = m_running;
            if (m_running->m_state == context::RUNNING) {
                m_running->m_state = context::SUSPENDING;
                m_suspend.push_back(m_running);
            }
        }
        
        if (! m_wait_fd.empty())
            select_fd(false);
        
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
#endif // KQUEUE
                
                    int i = 0;
                    for (auto &ev: m_running->m_fd) {
                        auto it = m_wait_fd.find(ev);
                        if (it == m_wait_fd.end())
                            continue;

                        it->second.erase(m_running);
                        
                        if (it->second.empty()) {
                            m_wait_fd.erase(it);
#ifdef KQUEUE
                            EV_SET(&kev[i++], ev.m_fd, ev.m_event, EV_DELETE, 0, 0, nullptr);
#endif // KQUEUE
                        }
                    }
                
#ifdef KQUEUE
                    if (i > 0) {
                        if(kevent(m_kq, kev, i, nullptr, 0, nullptr) == -1) {
                            PRINTERR("failed kevent");
                            exit(-1);
                        }
                    }

                    delete[] kev;
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
                    if (m_threadq.m_qwait_type == threadq::QWAIT_PIPE) {
#ifdef KQUEUE
                        struct kevent kev;
                        EV_SET(&kev, m_threadq.m_qpipe[0], EVFILT_READ, EV_DELETE, 0, 0, nullptr);
                        if (kevent(m_kq, &kev, 1, nullptr, 0, nullptr) == -1) {
                            PRINTERR("failed kevent");
                            exit(-1);
                        }
#endif // KQUEUE
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
                    // condition wait
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
                    m_threadq.m_qwait_type = threadq::QWAIT_PIPE;
                    lock.unlock();
                    
#ifdef KQUEUE
                    struct kevent kev;
                    EV_SET(&kev, m_threadq.m_qpipe[0], EVFILT_READ, EV_ADD | EV_ENABLE | EV_ONESHOT, 0, 0, NULL);
                    if (kevent(m_kq, &kev, 1, nullptr, 0, nullptr) == -1) {
                        PRINTERR("failed kevent");
                        exit(-1);
                    }
#endif // KQUEUE

#ifdef EPOLL
#endif // EPOLL
                }
            }
        } else if (m_wait_fd.empty() && m_timeout.empty()) {
            break;
        }

        select_fd(true);
    }

    _longjmp(m_jmp_buf, 1);
}

#if (defined KQUEUE)
void
green_thread::select_stream(struct kevent *kev, int num_kev,
                     void * const *stream, int num_stream,
                     bool is_threadq, int64_t timeout)
#elif (defined EPOLL) // #if (defined KQUEUE)
green_thread::select_stream(
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
        if (kevent(m_kq, kev, num_kev, NULL, 0, NULL) == -1) {
            PRINTERR("could not set events to kqueue!");
            exit(-1);
        }

        for (int i = 0; i < num_kev; i++) {
            m_wait_fd[{kev[i].ident, kev[i].filter}].insert(m_running);
            m_running->m_fd.push_back({kev[i].ident, kev[i].filter});
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
}

green_thread::threadq::~threadq()
{
    while (m_refcnt);
    
    delete[] m_q;

    close(m_qpipe[0]);
    close(m_qpipe[1]);
}

STRM_RESULT
green_thread::threadq::push(void *p)
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
        if (m_qwait_type == QWAIT_COND) {
            lock.unlock();
            std::unique_lock<std::mutex> mlock(m_qmutex);
            m_qcond.notify_one();
        } else {
            lock.unlock();
            char c = '\0';
            if (write(m_qpipe[1], &c, sizeof(c)) < 0) {
                PRINTERR("could not write data to pipe");
                exit(-1);
            }
        }
        
        return STRM_SUCCESS;
    }
    
    lock.unlock();
    
    return STRM_SUCCESS;
}

STRM_RESULT
green_thread::threadq::pop(void **p)
{
    int n = 0;
    while (m_qlen == 0) {
        if (n++ > 1000)
            return STRM_NO_MORE_DATA;
    }

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
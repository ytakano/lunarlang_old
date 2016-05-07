#include "lunar_fiber.hpp"
#include "lunar_rtm_lock.hpp"

#include <thread>

// currentry, this code can run on X86_64 System V ABI

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

} // extern "C"

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

void*
fiber::pop_threadq()
{
    int i = 0;
    while (m_qlen == 0) {
        i++;
        if (i > 100) {
            m_running->m_state = context::WAITING_THQ;
            yield();
            i = 0;
        }
    }

    void* retval = *m_qhead;

    {
        spin_lock_acquire lock(m_qlock);
        m_qlen--;
    }

    m_qhead++;

    if (m_qhead == m_qend) {
        m_qhead = m_q;
    }

    return retval;
}

void
fiber::push_threadq(void *ptr)
{
    int i = 0;
    while (m_qlen == m_max_qlen) {
        i++;
        if (i > 100) {
            yield();
            i = 0;
        }
    }

    spin_lock_acquire_unsafe lock(m_qlock);

    *m_qtail = ptr;
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
        
        return;
    }
    
    lock.unlock();
    
    return;
}

template<typename T>
STRM_RESULT
fiber::pop_stream(shared_stream *p, T &ret, bool is_yield)
{
    assert(p->flag & shared_stream::READ);
    
    ringq<T> *q = (ringq<T>*)p->shared_data->stream.ptr;

    for (;;) {
        auto result = q->pop(ret);
        switch (result) {
        case STRM_CLOSED:
            return STRM_CLOSED;
        case STRM_SUCCESS:
            return STRM_SUCCESS;
        case STRM_NO_MORE_DATA:
            if (is_yield) {
                m_running->m_state = context::WAITING_STREAM;
                m_wait_stream[q] = m_running;
                yield();
            } else {
                return STRM_NO_MORE_DATA;
            }
        default:
            assert(result != STRM_NO_VACANCY);
        }
    }

    // not reach here
    return STRM_SUCCESS;
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
        ret = kevent(m_kq, kev, size, kev, size, NULL);
    } else {
        timespec tm;
        tm.tv_sec  = 0;
        tm.tv_nsec = 0;
        ret = kevent(m_kq, kev, size, kev, size, &tm);
    }

    if (ret == -1) {
        PRINTERR("ERROR: failed kevent");
        exit(-1);
    }
    
    for (int i = 0; i < ret; i++) {
        int fd = kev[i].ident;
        auto it = m_wait_fd.find(fd);
        
        it->second->m_state = context::SUSPENDING;
        m_suspend.push_back(std::move(it->second));
        
        m_wait_fd.erase(it);
    }

    delete[] kev;
#endif // KQUEUE
}

int
fiber::spawn(void (*func)(void*), void *arg, int stack_size)
{
    auto ctx = llvm::make_unique<context>();
    
    while (++m_count == 0);
    
    ctx->m_state = context::READY;
    ctx->m_stack.resize(stack_size);
    ctx->m_id    = m_count;
    
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
}

}
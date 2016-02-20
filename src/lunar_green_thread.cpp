#include "lunar_green_thread.hpp"
#include "lunar_rtm_lock.hpp"

#include <thread>

// currentry, this code can run on X86_64 System V ABI

namespace lunar {

__thread green_thread *lunar_gt = nullptr;

rtm_lock lock_thread2gt;
std::unordered_map<std::thread::id, green_thread*> thread2gt;

// stack layout:
//     context
//     func
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "popq %rax;"               // pop func
    "subq $8, %rsp;"           // align 16 bytes
    "callq *%rax;"             // call func()
    "addq $8, %rsp;"           // align 16 bytes
    "popq %rax;"               // pop context
    "movl $5, (%rax);"         // context.m_state = STOP
    "jmp _yield_green_thread;" // jump to _yeild_green_thread
);

extern "C" {

void __INVOKE();

void
init_green_thread()
{
    if (lunar_gt == nullptr) {
        lunar_gt = new green_thread;
        rtm_transaction tr(lock_thread2gt);
        thread2gt[std::this_thread::get_id()] = lunar_gt;
    }
}

void
yield_green_thread()
{
    lunar_gt->yield();
}

void
spawn_green_thread(void (*func)())
{
    lunar_gt->spawn(func);
}

void
run_green_thread()
{
    lunar_gt->run();

    {
        rtm_transaction tr(lock_thread2gt);
        thread2gt.erase(std::this_thread::get_id());
    }

    delete lunar_gt;
}

void
wait_fd_read_green_thread(int fd)
{
    lunar_gt->wait_fd_read(fd);
}

void
wait_fd_write_green_thread(int fd)
{
    lunar_gt->wait_fd_write(fd);
}

void
wait_stream_green_thread()
{
    lunar_gt->wait_stream();
}

void
wake_up_stream_green_thread(void *ptr)
{
    lunar_gt->wake_up_stream(ptr);
}

} // extern "C"

void
green_thread::wait_fd_read(int fd)
{
    m_running->m_state = context::WAITING_FD;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

void
green_thread::wait_fd_write(int fd)
{
    m_running->m_state = context::WAITING_FD;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

void
green_thread::wait_stream()
{
    m_running->m_state = context::WAITING_STREAM;

    yield();
}

void
green_thread::wake_up_stream(void *ptr)
{
    auto it = m_wait_stream.find(ptr); 
    if (it == m_wait_stream.end())
        return;
    
    it->second->m_state = context::SUSPENDING;
    m_suspend.push_back(std::move(it->second));
    m_wait_stream.erase(it);
    
    yield();
}

void
green_thread::schedule()
{
    if (m_wait_fd.empty()) {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_cond.wait(lock);
    } else {
#ifdef KQUEUE
        auto size = m_wait_fd.size();
        struct kevent *kev = new struct kevent[size];
        
        int ret = kevent(m_kq, kev, size, kev, size, NULL);
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

        delete kev;
        
#endif // KQUEUE
    }
}

int
green_thread::spawn(void (*func)(), int stack_size)
{
    auto ctx = llvm::make_unique<context>();
    
    while (++m_count == 0);
    
    ctx->m_state = context::READY;
    ctx->m_stack.resize(stack_size);
    ctx->m_id    = m_count;
    
    auto s = ctx->m_stack.size();
    ctx->m_stack[s - 1] = (uint64_t)ctx.get(); // push context
    ctx->m_stack[s - 2] = (uint64_t)func;      // push func
    
    m_id2context[m_count] = ctx.get();
    m_ready.push_back(std::move(ctx));
    
    return 0;
}

void
green_thread::run()
{
    if (setjmp(m_jmp_buf) == 0) {
        yield();
    }
}

void
green_thread::yield()
{
    for (;;) {
        context *ctx = nullptr;
        
        if (m_running) {
            if (m_running->m_state == context::STOP) {
                m_id2context.erase(m_running->m_id);
                m_running.reset();
            } else if (m_running->m_state == context::WAITING_FD) {
                ctx = m_running.get();
                m_wait_fd[m_running->m_fd] = std::move(m_running);
            } else if (m_running->m_state == context::WAITING_STREAM) {
                ctx = m_running.get();
                m_wait_stream[m_running->m_stream] = std::move(m_running);
            } else if (m_running->m_state == context::RUNNING) {
                ctx = m_running.get();
                m_running->m_state = context::SUSPENDING;
                m_suspend.push_back(std::move(m_running));
            }
        }
    
        // invoke READY state
        if (! m_ready.empty()) {
            m_running = std::move(m_ready.front());
            m_running->m_state = context::RUNNING;
            m_ready.pop_front();
            if (ctx != nullptr) {
                if (setjmp(ctx->m_jmp_buf) == 0) {
                    auto p = &m_running->m_stack[m_running->m_stack.size() - 2];
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
                auto p = &m_running->m_stack[m_running->m_stack.size() - 2];
                asm (
                    "movq %0, %%rsp;" // set stack pointer
                    "movq %0, %%rbp;" // set frame pointer
                    "jmp ___INVOKE;"
                    :
                    : "r" (p)
                );
            }
        }
    
        // invoke SUSPEND state
        if (! m_suspend.empty()) {
            m_running = std::move(m_suspend.front());
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
        
        // TODO:
        if (m_wait_fd.empty())
            break;

        schedule();
    }
    
    longjmp(m_jmp_buf, 1);
}

}
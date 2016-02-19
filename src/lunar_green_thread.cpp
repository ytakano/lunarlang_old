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
    "movl $4, (%rax);"         // context.m_state = STOP
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

} // extern "C"

void
green_thread::wait_fd_read(int fd)
{
    m_running->m_state = context::WAITING;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

void
green_thread::wait_fd_write(int fd)
{
    m_running->m_state = context::WAITING;
    m_running->m_fd    = fd;

#ifdef KQUEUE
    EV_SET(&m_kev, fd, EVFILT_WRITE, EV_ADD, 0, 0, NULL);
#endif // KQUEUE

    yield();
}

void
green_thread::schedule()
{
    if (m_wait.empty()) {
        std::unique_lock<std::mutex> lock(m_mutex);
        m_cond.wait(lock);
    } else {
#ifdef KQUEUE
        // TODO:
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
    context *ctx = nullptr;
    
    if (m_running) {
        if (m_running->m_state == context::STOP) {
            m_running.reset();
        } else if (m_running->m_state == context::WAITING) {
            ctx = m_running.get();
            m_wait[m_running->m_fd] = std::move(m_running);
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
    
    if (! m_wait.empty()) {
        schedule();
        return;
    }
    
    longjmp(m_jmp_buf, 1);
}

}
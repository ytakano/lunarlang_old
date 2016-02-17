#include "lunar_green_thread.hpp"

// currentry, this code can run on X86_64 System V ABI

namespace lunar {

__thread green_thread *lunar_gt = nullptr;

// stack layout:
//     context
//     func
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "popq %rax;"               // pop func
    "callq *%rax;"             // call func()
    "popq %rax;"               // pop context
    "movl $3, (%rax);"         // m_state = STOP
    "jmp _yeild_green_thread;" // jump to _yeild_green_thread
);

extern "C" void __INVOKE();

extern "C" void init_green_thread()
{
    if (lunar_gt == nullptr)
        lunar_gt = new green_thread;
}

extern "C" void yeild_green_thread()
{
    lunar_gt->yield();
}

extern "C" void spawn_green_thread(void (*func)())
{
    lunar_gt->spawn(func);
}

int
green_thread::spawn(void (*func)(), int stack_size)
{
    auto ctx = llvm::make_unique<context>();
    
    while (++m_count != 0);
    
    ctx->m_stack.resize(stack_size);
    ctx->m_id    = m_count;
    ctx->m_state = context::READY; 
    
    auto p = ctx.get();
    
    m_id2context[m_count] = p;
    m_contexts.push_back(std::move(ctx));
    
    auto s = ctx->m_stack.size();
    ctx->m_stack[s - 0] = (uint64_t)ctx.get(); // push context
    ctx->m_stack[s - 1] = (uint64_t)func;      // push func
    
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
    for (auto it = m_contexts.begin(); it != m_contexts.end(); ) {
        if ((*it)->m_state == context::SUSPENDING) {
            if (m_current_ctx != nullptr && m_current_ctx->m_state == context::RUNNING) {
                // suspend current context
                m_current_ctx->m_state = context::SUSPENDING;

                if (setjmp(m_current_ctx->m_jmp_buf) == 0) {
                    // wake up new context
                    (*it)->m_state = context::RUNNING;
                    m_current_ctx = it->get();
                    m_contexts.splice(m_contexts.end(), m_contexts, it);
                    longjmp((*it)->m_jmp_buf, 1);
                } else {
                    return;
                }
            } else {
                // wake up new context
                (*it)->m_state = context::RUNNING;
                m_current_ctx = it->get();
                m_contexts.splice(m_contexts.end(), m_contexts, it);
                longjmp((*it)->m_jmp_buf, 1);
            }
        } else if ((*it)->m_state == context::READY) {
            if (m_current_ctx != nullptr && m_current_ctx->m_state == context::RUNNING) {
                // suspend current context
                m_current_ctx->m_state = context::SUSPENDING;

                if (setjmp(m_current_ctx->m_jmp_buf) == 0) {
                    // invoke new context
                    (*it)->m_state = context::RUNNING;
                    m_current_ctx = it->get();
                    m_contexts.splice(m_contexts.end(), m_contexts, it);
                    
                    auto p = &(*it)->m_stack[(*it)->m_stack.size() - 2];
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
                // invoke new context
                (*it)->m_state = context::RUNNING;
                m_current_ctx = it->get();
                m_contexts.splice(m_contexts.end(), m_contexts, it);

                auto p = &(*it)->m_stack[(*it)->m_stack.size() - 2];
                asm (
                    "movq %0, %%rsp;" // set stack pointer
                    "movq %0, %%rbp;" // set frame pointer
                    "jmp ___INVOKE;"
                    :
                    : "r" (p)
                );
            }
        } else if ((*it)->m_state == context::STOP) {
            // remove context
            m_id2context.erase((*it)->m_id);
            
            if (it->get() == m_current_ctx) {
                m_current_ctx = nullptr;
            }
            
            it = m_contexts.erase(it);
        } else {
            break;
        }
    }
    
    longjmp(m_jmp_buf, 1);
}

}
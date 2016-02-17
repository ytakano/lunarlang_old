#include "lunar_green_thread.hpp"

// currentry, this code can run on X86_64 System V ABI

namespace lunar {

// stack layout:
//     pointer of the context
//     this pointer of the green_thread
//     pointer of the call back function
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "popq %rax;"       // pop function
    "popq %rdi;"       // pop this
    "pushq %rdi;"      // push this
    "callq *%rax;"     // call function(this)
    "popq %rdi;"       // pop this
    "popq %rax;"       // pop context
    "movl $3, (%rax);" // m_state = STOP
    "jmp ___YIELD;"    // jump to __YIELD
);

extern "C" void __INVOKE();

extern "C" void __YIELD(void *p)
{
    green_thread *gt = (green_thread*)p;
    gt->yield();
}

int
green_thread::spawn(void (*func)(void *), int stack_size = 0x80000)
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
    ctx->m_stack[s - 1] = (uint64_t)this;      // push this
    ctx->m_stack[s - 2] = (uint64_t)func;      // push func
    
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
                if (setjmp(m_current_ctx->m_jmp_buf) == 0) {
                    (*it)->m_state = context::RUNNING;
                    m_current_ctx = it->get();
                    m_contexts.splice(m_contexts.end(), m_contexts, it);
                    longjmp((*it)->m_jmp_buf, 1);
                } else {
                    return;
                }
            } else {
                (*it)->m_state = context::RUNNING;
                m_current_ctx = it->get();
                m_contexts.splice(m_contexts.end(), m_contexts, it);
                longjmp((*it)->m_jmp_buf, 1);
            }
        } else if ((*it)->m_state == context::READY) {
            if (m_current_ctx != nullptr && m_current_ctx->m_state == context::RUNNING) {
                if (setjmp(m_current_ctx->m_jmp_buf) == 0) {
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
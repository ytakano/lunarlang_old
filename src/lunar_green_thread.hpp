#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include "lunar_common.hpp"

#include <setjmp.h>

#include <vector>
#include <list>
#include <unordered_map>

namespace lunar {

class green_thread {
    struct context {
        enum {
            READY      = 0,
            RUNNING    = 1,
            SUSPENDING = 2,
            STOP       = 3,
        } m_state;

        jmp_buf m_jmp_buf;
        int     m_id; // m_id must not be 0
        std::vector<uint64_t> m_stack;
    };
    
public:
    green_thread() : m_count(0), m_current_ctx(nullptr) { }

    void yield();
    int  spawn(void (*func)(void *), int stack_size);
    void run();
    
    //static void invoke(void (*func)(void *));

private:
    jmp_buf  m_jmp_buf;
    int      m_count;
    context *m_current_ctx;
    std::list<std::unique_ptr<context>> m_contexts;
    std::unordered_map<int, context*>   m_id2context;
};

}

#endif // LUNAR_GREEN_THREAD_HPP
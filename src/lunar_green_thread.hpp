#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include "lunar_common.hpp"

#include <setjmp.h>

#include <vector>
#include <list>
#include <unordered_map>

namespace lunar {

extern "C" {
    void init_green_thread();
    void yield_green_thread(bool is_wait = false);
    void spawn_green_thread(void (*func)());
    void run_green_thread();
}

class green_thread {
    struct context {
        enum {
            READY      = 0,
            RUNNING    = 1,
            SUSPENDING = 2, // runnable, but not running
            WAIT       = 3, // waiting data, thus not runnable
            STOP       = 4,
        } m_state;

        jmp_buf m_jmp_buf;
        int     m_id; // m_id must not be 0
        std::vector<uint64_t> m_stack;
    };
    
public:
    green_thread() : m_count(0), m_current_ctx(nullptr) { }

    void yield();
    int  spawn(void (*func)(), int stack_size = 0x80000);
    void run();
    void wait(int id);

private:
    jmp_buf  m_jmp_buf;
    int      m_count;
    context *m_current_ctx;
    std::list<std::unique_ptr<context>> m_contexts;
    std::unordered_map<int, context*>   m_id2context;
};

}

#endif // LUNAR_GREEN_THREAD_HPP
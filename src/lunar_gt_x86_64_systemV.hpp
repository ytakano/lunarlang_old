#ifndef LUNAR_GT_X86_64_SYSTEMV
#define LUNAR_GT_X86_64_SYSTEMV

#include "lunar_common.hpp"

#include <vector>
#include <unordered_map>

namespace lunar {

class green_thread_x86_64_systemV {
public:
    green_thread_x86_64_systemV(size_t len) : m_ssize(len), m_count(0) { }
    
    int spawn(void (*func)(void*));
    void yield();
    void run();

private:
    struct registers {
        uint32_t rip; // program counter
        uint64_t rsp; // stack pointer
        uint64_t rbx;
        uint64_t rbp;
        uint64_t r12;
        uint64_t r13;
        uint64_t r14;
        uint64_t r15;
    };

    struct context {
        enum {
            RUUNING,
            READY,
        } state;
        
        registers regs;
        std::vector<char> stack;
        void (*func)(void *);
        int id;
    };

    char m_ssize;
    int  m_count;
    registers m_regs;
    std::unordered_map<int, std::unique_ptr<context>> m_contexts;
    
    void ctx_switch();
};

}

#endif // LUNAR_GT_X86_64_SYSTEMV
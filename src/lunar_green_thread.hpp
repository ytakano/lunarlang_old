#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include "lunar_common.hpp"

#include <setjmp.h>

#include <vector>
#include <deque>
#include <unordered_map>
#include <mutex>
#include <condition_variable>

#define KQUEUE

#ifdef KQUEUE
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#endif // KQUEUE

namespace lunar {

extern "C" {
    void init_green_thread();
    void yield_green_thread();
    void spawn_green_thread(void (*func)());
    void run_green_thread();
    void wait_fd_read_green_thread(int fd);
    void wait_fd_write_green_thread(int fd);
}

class green_thread {    
public:
    struct context {
        enum {
            READY      = 0,
            RUNNING    = 1,
            SUSPENDING = 2,
            WAITING    = 3,
            STOP       = 4,
        } m_state;
        jmp_buf m_jmp_buf;
        int     m_id; // m_id must not be 0
        int     m_fd;
        std::vector<uint64_t> m_stack;
    };

    green_thread() : m_count(0), m_current_ctx(nullptr)
    {
#ifdef KQUEUE
        m_kq = kqueue();
        if (m_kq == -1) {
            PRINTERR("could not create kqueue!");
            exit(-1);
        }
#endif // KQUEUE
    }
    
    ~green_thread()
    {
#ifdef KQUEUE
        close(m_kq);
#endif // KQUEUE
    }

    void yield();
    int  spawn(void (*func)(), int stack_size = 0x80000);
    void run();
    void wait(int id);
    void wait_fd_read(int fd);
    void wait_fd_write(int fd);

private:
    jmp_buf     m_jmp_buf;
    int         m_count;
    context    *m_current_ctx;
    std::mutex  m_mutex;
    std::condition_variable m_cond;
    std::deque<std::unique_ptr<context>> m_ready;
    std::unique_ptr<context>             m_running;
    std::deque<std::unique_ptr<context>> m_suspend;
    std::unordered_map<int, std::unique_ptr<context>> m_wait;
    std::unordered_map<int, context*>    m_id2context;

#ifdef KQUEUE
    int           m_kq;
    struct kevent m_kev;
#endif // KQUEUE
    
    void schedule();
};

}

#endif // LUNAR_GREEN_THREAD_HPP
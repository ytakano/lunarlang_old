#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include "lunar_common.hpp"
#include "lunar_spin_lock.hpp"
#include "lunar_shared_stream.hpp"
#include "lunar_ringq.hpp"

#include <setjmp.h>

#include <string>
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
    read_result pop_string(shared_stream *p, std::u32string **ret);
    read_result push_string(shared_stream *p, std::u32string *ret);
    void push_eof(shared_stream *p);
}

class green_thread {
public:
    struct context {
        enum {
            READY          = 0,
            RUNNING        = 1,
            SUSPENDING     = 2,
            WAITING_FD     = 3,
            WAITING_STREAM = 4,
            WAITING_THQ    = 5,
            STOP           = 6,
        } m_state;
        jmp_buf m_jmp_buf;
        int     m_id; // m_id must not be 0
        int     m_fd;
        void   *m_stream;
        std::vector<uint64_t> m_stack;
    };

    green_thread(int qsize = 4096)
        : m_count(0),
          m_running(nullptr),
          m_threadq(nullptr),
          m_max_qlen(qsize),
          m_qlen(0),
          m_q(new void*[qsize]),
          m_qend(m_q + qsize),
          m_qhead(m_q),
          m_qtail(m_q),
          m_is_qnotified(true)
    {
        
        if (pipe(m_qpipe) == -1) {
            PRINTERR("could not create pipe!");
            exit(-1);
        }

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
        delete[] m_q;
#ifdef KQUEUE
        close(m_kq);
#endif // KQUEUE

        close(m_qpipe[0]);
        close(m_qpipe[1]);
    }

    void yield();
    int  spawn(void (*func)(), int stack_size = 0x80000);
    void run();
    void wait(int id);
    void wait_fd_read(int fd);
    void wait_fd_write(int fd);
    void* pop_threadq();
    void  push_threadq(void *ptr);
    void  push_eof_stream(shared_stream *p);
    
    template<typename T> read_result pop_stream(shared_stream *p, T &ret);
    template<typename T> read_result push_stream(shared_stream *p, T ptr);

private:
    jmp_buf     m_jmp_buf;
    int         m_count;
    std::unordered_map<int, std::unique_ptr<context>> m_id2context;
    std::deque<context*> m_ready;
    context*             m_running;
    std::deque<context*> m_suspend;
    context*             m_threadq;
    std::unordered_map<int, context*>   m_wait_fd;
    std::unordered_map<void*, context*> m_wait_stream;

    // for circular buffer
    int m_max_qlen;
    int m_qlen;
    void **m_q;
    void **m_qend;
    void **m_qhead;
    void **m_qtail;
    bool   m_is_qnotified;
    spin_lock   m_qlock;
    std::mutex  m_qmutex;
    std::condition_variable m_qcond;
    int m_qpipe[2];

    enum {
        QWAIT_COND,
        QWAIT_PIPE,
    } m_qwait_type;
    
    // for file descripter
#ifdef KQUEUE
    int           m_kq;
    struct kevent m_kev;
#endif // KQUEUE
    
    void select_fd(bool is_block);
};

}

#endif // LUNAR_GREEN_THREAD_HPP
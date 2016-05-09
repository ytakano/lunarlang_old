#ifndef LUNAR_FIBER_HPP
#define LUNAR_FIBER_HPP

#include "lunar_common.hpp"
#include "lunar_spin_lock.hpp"
#include "lunar_shared_stream.hpp"
#include "lunar_ringq.hpp"

#include <unistd.h>
#include <setjmp.h>

#include <string>
#include <vector>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <thread>
#include <mutex>
#include <condition_variable>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>

#define KQUEUE

#ifdef KQUEUE
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#endif // KQUEUE

namespace lunar {

class fiber;

extern "C" {
    void init_fiber();
    void yield_fiber();
    void spawn_fiber(void (*func)(void*), void *arg = nullptr);
    void run_fiber();

    STRM_RESULT push_threadq_fiber(std::thread::id id, void *p);
    STRM_RESULT push_threadq_fast_unsafe_fiber(fiber *fb, void *p);
/*
    void wait_fd_read_fiber(int fd);
    void wait_fd_write_fiber(int fd);
    STRM_RESULT pop_string(shared_stream *p, std::u32string **ret, bool is_yield = true);
    STRM_RESULT push_string(shared_stream *p, std::u32string *ret);
    void push_eof_string(shared_stream *p);
*/
}

class fiber {
public:
    struct context {
        // states of contexts
        static const int READY           = 0x0000;
        static const int RUNNING         = 0x0001;
        static const int SUSPENDING      = 0x0002;
        static const int WAITING_FD      = 0x0004;
        static const int WAITING_STREAM  = 0x0010;
        static const int WAITING_THQ     = 0x0020;
        static const int WAITING_TIMEOUT = 0x0040;
        static const int STOP            = 0x0080;

        uint32_t m_state;
        jmp_buf m_jmp_buf;
        std::unordered_set<int>   m_fd;     // waiting file descripters
        std::unordered_set<void*> m_stream; // waiting streams
        bool    m_is_threadq;               // waiting the thread queue?
        int64_t m_id; // m_id must not be less than or equal to 0
        std::vector<uint64_t> m_stack;
    };

    fiber(int qsize = 4096);
    virtual ~fiber();

    void yield();
    int  spawn(void (*func)(void*), void *arg = nullptr, int stack_size = 0x80000);
    void run();
    void inc_refcnt_threadq() { m_threadq.inc_refcnt(); }
    void dec_refcnt_threadq() { m_threadq.dec_refcnt(); }
    STRM_RESULT push_threadq(void *p) { return m_threadq.push(p); }
    void select_stream(const uintptr_t *fd, const int16_t *fd_flag, int num_fd, // fd, process ID, signal number
                       void * const *stream, int num_stream,
                       bool &is_threadq, int64_t timeout);

/*
    void wait(int id);
    void wait_fd_read(int fd);
    void wait_fd_write(int fd);
    void* pop_threadq();
    void  push_threadq(void *ptr);
    
    template<typename T> STRM_RESULT pop_stream(shared_stream *p, T &ret, bool is_yield = true);
    template<typename T> STRM_RESULT push_stream(shared_stream *p, T ptr);
    template<typename T> void        push_eof_stream(shared_stream *p);
*/
private:
    struct ctx_time {
        double   m_time;
        context *m_ctx;
        
        ctx_time(double t, context *ctx) : m_time(t), m_ctx(ctx) { }
    };
    
    struct k_time { };
    struct k_ctx  { };

    typedef boost::multi_index::multi_index_container<
        ctx_time,
        boost::multi_index::indexed_by<
            boost::multi_index::ordered_non_unique<
                boost::multi_index::tag<k_time>,
                boost::multi_index::member<ctx_time, double, &ctx_time::m_time>>,
            boost::multi_index::hashed_unique<
                boost::multi_index::tag<k_ctx>,
                boost::multi_index::member<ctx_time, context*, &ctx_time::m_ctx>>
        >
    > timeout_t;

    jmp_buf   m_jmp_buf;
    int64_t   m_count;
    context*  m_running;
    context*  m_wait_thq;
    timeout_t m_timeout;
    std::deque<context*> m_suspend;
    std::deque<context*> m_ready;
    std::unordered_map<int64_t, std::unique_ptr<context>> m_id2context;
    std::unordered_map<int, context*>   m_wait_fd;
    std::unordered_map<void*, context*> m_wait_stream;
    
    // for circular buffer
    class threadq {
    public:
        enum qwait_type {
            QWAIT_COND,
            QWAIT_PIPE,
        };
        
        threadq(int qsize);
        virtual ~threadq();
        
        qwait_type m_qwait_type;
        
        STRM_RESULT push(void *p);
        STRM_RESULT pop(void **p);
        
        void inc_refcnt() { __sync_fetch_and_add(&m_refcnt, 1); }
        void dec_refcnt() { __sync_fetch_and_sub(&m_refcnt, 1); }
    
    private:
        volatile int  m_qlen;
        volatile int  m_refcnt;
        volatile bool m_is_qnotified;
        int    m_max_qlen;
        void **m_q;
        void **m_qend;
        void **m_qhead;
        void **m_qtail;
        int    m_qpipe[2];
        spin_lock  m_qlock;
        std::mutex m_qmutex;
        std::condition_variable m_qcond;
    };

    threadq m_threadq;

#ifdef KQUEUE
    int m_kq;
#endif // KQUEUE

    void select_fd(bool is_block);
};

}

#endif // LUNAR_FIBER_HPP
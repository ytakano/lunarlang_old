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

#if (defined(__unix__) || defined(unix) || (defined __APPLE__)) && !defined(USG)
#include <sys/param.h>
#endif

#if (defined BSD)
    #define KQUEUE
#elif (defined __linux__)
    #define EPOLL
#else
    #error unsupported platform!
#endif

#ifdef KQUEUE
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#endif // KQUEUE

#define	TIMESPECCMP(tvp, uvp, cmp)                  \
	(((tvp)->tv_sec == (uvp)->tv_sec) ?             \
	    ((tvp)->tv_nsec cmp (uvp)->tv_nsec) :       \
	    ((tvp)->tv_sec cmp (uvp)->tv_sec))

#define	TIMESPECADD(vvp, uvp)                       \
    do {                                            \
        (vvp)->tv_sec += (uvp)->tv_sec;             \
        (vvp)->tv_nsec += (uvp)->tv_nsec;           \
        if ((vvp)->tv_nsec >= 1000000000) {         \
            (vvp)->tv_sec++;                        \
            (vvp)->tv_nsec -= 1000000000;           \
        }                                           \
    } while (0)

#define	TIMESPECSUB(vvp, uvp)                       \
    do {                                            \
        (vvp)->tv_sec -= (uvp)->tv_sec;             \
        (vvp)->tv_nsec -= (uvp)->tv_nsec;           \
        if ((vvp)->tv_nsec < 0) {                   \
            (vvp)->tv_sec--;                        \
            (vvp)->tv_nsec += 1000000000;           \
        }                                           \
    } while (0)

#ifdef __APPLE__
#define GETTIME(ts)                                 \
    do {                                            \
        timeval tv;                                 \
        gettimeofday(&tv, nullptr);                 \
        (ts)->tv_sec  = tv.tv_sec;                  \
        (ts)->tv_nsec = tv.tv_usec * 1000;          \
    } while (0)
#elif (defined BSD)
#define GETTIME(ts) clock_gettime(CLOCK_MONOTONIC_FAST, ts)
#elif (defined __linux__)
#define GETTIME(ts) clock_gettime(CLOCK_MONOTONIC_COARSE, ts)
#endif // __APPLE__

namespace lunar {

class fiber;

extern "C" {
    uint64_t get_clock();
    bool init_fiber(uint64_t thid); // thid is user defined thread ID
    void yield_fiber();
    void spawn_fiber(void (*func)(void*), void *arg = nullptr);
    void run_fiber();
    uint64_t get_thread_id();
    void* get_fiber(uint64_t thid);
    bool is_timeout_fiber();

#ifdef KQUEUE
    void select_fiber(struct kevent *kev, int num_kev,
                      void * const *stream, int num_stream,
                      bool is_threadq, int64_t timeout);
#endif // KQUEUE

    STRM_RESULT push_threadq_fiber(uint64_t id, void *p);
    STRM_RESULT push_threadq_fast_unsafe_fiber(void *fb, void *p);
    STRM_RESULT pop_threadq_fiber(void **p);
    STRM_RESULT pop_string(shared_stream *p, void **ret);
    STRM_RESULT push_string(shared_stream *p, void *ret);
    STRM_RESULT pop_ptr(shared_stream *p, void **ret);
    STRM_RESULT push_ptr(shared_stream *p, void *ret);
}

class fiber {
public:
    fiber(int qsize = 4096);
    virtual ~fiber();

    void yield();
    int  spawn(void (*func)(void*), void *arg = nullptr, int stack_size = 0x80000);
    void run();
    bool is_timeout() { return m_running->m_is_ev_timeout; }
    void inc_refcnt_threadq() { m_threadq.inc_refcnt(); }
    void dec_refcnt_threadq() { m_threadq.dec_refcnt(); }
    STRM_RESULT push_threadq(void *p) { return m_threadq.push(p); }
    STRM_RESULT pop_threadq(void **p) { return m_threadq.pop(p); }

#ifdef KQUEUE
    void select_stream(struct kevent *kev, int num_kev,
                       void * const *stream, int num_stream,
                       bool is_threadq, int64_t timeout);
#endif // KQUEUE

#ifdef EPOLL
#endif

    template<typename T> STRM_RESULT pop_stream(shared_stream *p, T &ret);
    template<typename T> STRM_RESULT push_stream(shared_stream *p, T data);
    template<typename T> void        push_eof_stream(shared_stream *p);

private:
    struct ev_key {
        uintptr_t m_fd;
        int16_t   m_event;
        
        ev_key(uintptr_t fd, int16_t event) : m_fd(fd), m_event(event) { }
        
        bool operator== (const ev_key &rhs) const {
            return (m_fd == rhs.m_fd) && (m_event == rhs.m_event);
        }
    };

    struct ev_key_hasher {
        std::size_t operator()(const ev_key& k) const {
            using std::size_t;
            using std::hash;
            using std::string;

            return hash<uintptr_t>()(k.m_fd) ^ hash<int16_t>()(k.m_event);
        }
    };
    
    struct context {
        // states of contexts
        static const int READY           = 0x0001;
        static const int RUNNING         = 0x0002;
        static const int SUSPENDING      = 0x0004;
        static const int WAITING_FD      = 0x0008;
        static const int WAITING_STREAM  = 0x0010;
        static const int WAITING_THQ     = 0x0020;
        static const int WAITING_TIMEOUT = 0x0040;
        static const int STOP            = 0x0080;
        
        struct event_data {
            uint16_t m_flags;
            uint32_t m_fflags;
            intptr_t m_data;
            
            event_data(uint16_t flags, uint32_t fflags, intptr_t data)
                : m_flags(flags), m_fflags(fflags), m_data(data) { }
        };
        
        uint32_t m_state;
        jmp_buf m_jmp_buf;
        std::unordered_set<ev_key, ev_key_hasher> m_fd; // waiting file descriptors to read
        std::unordered_set<void*> m_stream;             // waiting streams to read
        std::unordered_set<void*> m_ev_stream;          // streams are ready to read
        std::unordered_map<ev_key, event_data, ev_key_hasher> m_events; // invoked events
        bool m_is_ev_thq;     // the thread queue is ready to read
        bool m_is_ev_timeout; //
        int64_t m_id; // m_id must not be less than or equal to 0
        std::vector<uint64_t> m_stack;
    };

    struct ctx_time {
        uint64_t  m_clock;
        context  *m_ctx;
        
        ctx_time(uint64_t clock, context *ctx) : m_clock(clock), m_ctx(ctx) { }
    };
    
    typedef boost::multi_index::multi_index_container<
        ctx_time,
        boost::multi_index::indexed_by<
            boost::multi_index::ordered_non_unique<
                boost::multi_index::tag<>,
                boost::multi_index::member<ctx_time, uint64_t, &ctx_time::m_clock>>,
            boost::multi_index::hashed_unique<
                boost::multi_index::tag<>,
                boost::multi_index::member<ctx_time, context*, &ctx_time::m_ctx>>
        >
    > timeout_t;

    jmp_buf   m_jmp_buf;
    int64_t   m_count;
    context*  m_running;
    context*  m_wait_thq;
    timeout_t m_timeout;
    std::deque<context*> m_suspend;
    std::unordered_map<int64_t, std::unique_ptr<context>> m_id2context;
    std::unordered_map<ev_key, std::unordered_set<context*>, ev_key_hasher> m_wait_fd;
    std::unordered_map<void*, context*> m_wait_stream;
    
    // for circular buffer
    class threadq {
    public:
        enum qwait_type {
            QWAIT_COND,
            QWAIT_PIPE,
            QWAIT_NONE,
        };
        
        threadq(int qsize);
        virtual ~threadq();
        
        STRM_RESULT push(void *p);
        STRM_RESULT pop(void **p);
        
        int get_len() { return m_qlen; }
        int get_read_fd() { return m_qpipe[0]; }
        qwait_type get_wait_type() { return m_qwait_type; }
        void set_wait_type(qwait_type t) { m_qwait_type = t; }
        void inc_refcnt() { __sync_fetch_and_add(&m_refcnt, 1); }
        void dec_refcnt() { __sync_fetch_and_sub(&m_refcnt, 1); }
        
        void pop_pipe(ssize_t len) {
            char buf[16];
            ssize_t n;
            do {
                n = read(m_qpipe[0], buf, sizeof(buf));
                if (n < 0) {
                    PRINTERR("could not read data from pipe");
                    exit(-1);
                }
                
                assert(n != 0);
                assert(n <= len);
                
                len -= n;
            } while (len > 0);
        }
    
    private:
        volatile int  m_qlen;
        volatile int  m_refcnt;
        volatile bool m_is_qnotified;
        volatile qwait_type m_qwait_type;
        int    m_max_qlen;
        void **m_q;
        void **m_qend;
        void **m_qhead;
        void **m_qtail;
        int    m_qpipe[2];
        spin_lock  m_qlock;
        std::mutex m_qmutex;
        std::condition_variable m_qcond;
        
        friend void fiber::yield();
    };

    threadq m_threadq;

#ifdef KQUEUE
    int m_kq;
#endif // KQUEUE

    void select_fd(bool is_block);
    void resume_timeout();
};

}

#endif // LUNAR_FIBER_HPP
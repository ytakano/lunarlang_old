#include "lunar_green_thread.hpp"

volatile int n = 0;

void
func3(void *arg) {
    auto ws = (lunar::shared_stream*)arg;
    for (;;) {
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 11000);
        auto ret = lunar::push_ptr(ws, nullptr);
        assert(ret == lunar::STRM_SUCCESS);
        fflush(stdout);
    }
}

void
func1(void *arg)
{
    n++;
    while(n != 2); // barrier

    auto rs = new lunar::shared_stream;
    auto ws = new lunar::shared_stream;
    lunar::make_ptr_stream(rs, ws, 1);

    lunar::spawn_green_thread(func3, ws);
    
/*#ifdef KQUEUE
    struct kevent kev;
    EV_SET(&kev, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
#endif // KQUEUE*/

    printf("> ");
    fflush(stdout);

    for (;;) {
        //lunar::select_green_thread(&kev, 1, (void**)&rs, 1, true, 10000);
        lunar::select_green_thread(nullptr, 0, (void**)&rs, 1, true, 5000);
        
        if (lunar::is_timeout_green_thread()) {
            printf("timeout!\n> ");
            fflush(stdout);
        }
        
        if (lunar::is_ready_threadq_green_thread()) {
            void *data;
            auto ret = lunar::pop_threadq_green_thread(&data);
            assert(ret == lunar::STRM_SUCCESS);
            
            printf("recv thread queue!\n> ");
            fflush(stdout);
        }
        
        void **streams;
        ssize_t len;
        lunar::get_streams_ready_green_thread(&streams, &len);
        for (int i = 0; i < len; i++) {
            void *val;
            auto ret = lunar::pop_ptr((lunar::shared_stream*)streams[i], &val);
            assert(ret == lunar::STRM_SUCCESS);
            printf("recv stream!\n> ");
            fflush(stdout);
        }
    }
}

void
func2(void *arg)
{
    n++;
    while(n != 2); // barrier
    
    auto fb = lunar::get_green_thread(1);
    for (;;) {
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 13000);
        lunar::push_threadq_fast_unsafe_green_thread(fb, nullptr);
    }
}

void
thread2()
{
    lunar::init_green_thread(2);
    lunar::spawn_green_thread(func2);
    lunar::run_green_thread();
}

void
thread1()
{
    lunar::init_green_thread(1);
    lunar::spawn_green_thread(func1);
    lunar::run_green_thread();
}

int
main(int argc, char *argv[])
{
    std::thread th1(thread1);
    std::thread th2(thread2);
    
    th1.join();
    th2.join();

    return 0;
}
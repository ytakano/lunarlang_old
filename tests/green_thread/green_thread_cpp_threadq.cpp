#include "lunar_green_thread.hpp"

#include <thread>

volatile int n = 0;
volatile uint64_t cnt = 0;

void
func1(void *arg)
{
    __sync_fetch_and_add(&n, 1);
    while(n != 3); // barrier

    for (;;) {
        void *data;
        if (lunar::pop_threadq_green_thread(&data) == lunar::STRM_NO_MORE_DATA) {
            lunar::select_green_thread(nullptr, 0, nullptr, 0, true, 0);
            continue;
        }

        cnt++;
    }
}

void
func2(void *arg)
{
    __sync_fetch_and_add(&n, 1);
    while(n != 3); // barrier
    
    auto fb = lunar::get_green_thread(1);
    for (;;) lunar::push_threadq_fast_unsafe_green_thread(fb, nullptr);
}

void
thread3()
{
    __sync_fetch_and_add(&n, 1);
    while(n != 3); // barrier

    for (;;) {
        uint64_t c0 = cnt;
        auto t0 = lunar::get_clock();
        sleep(5);
        uint64_t c1 = cnt;
        auto t1 = lunar::get_clock();
        printf("%lf [ops/s]\n", (c1 - c0) / ((t1 - t0) * 1e-3));
        fflush(stdout);
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
    std::thread th3(thread3);
    
    th1.join();
    th2.join();
    th3.join();

    return 0;
}
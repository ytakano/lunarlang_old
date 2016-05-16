#include "lunar_green_thread.hpp"

#include <thread>

volatile int n = 0;

void
func1(void *arg)
{
    n++;
    while(n != 2); // barrier

    int cnt = 0;
    int s   = 0;
    timespec ts0;
    GETTIME(&ts0);
    for (;;) {
        void *data;
        if (lunar::pop_threadq_green_thread(&data) == lunar::STRM_NO_MORE_DATA) {
            s++;
            lunar::select_green_thread(nullptr, 0, nullptr, 0, true, 0);
            continue;
        }

        cnt++;
        if (cnt > 50000000) {
            timespec ts1;
            GETTIME(&ts1);
            TIMESPECSUB(&ts1, &ts0);
            auto sec = ts1.tv_sec + ts1.tv_nsec * 1e-9;

            printf("%lf [ops/s], select = %lf\n",
                   cnt / sec, s / sec);

            cnt = 0;
            s   = 0;
            GETTIME(&ts0);
        }
    }
}

void
func2(void *arg)
{
    n++;
    while(n != 2); // barrier
    
    auto fb = lunar::get_green_thread(1);
    for (;;) lunar::push_threadq_fast_unsafe_green_thread(fb, nullptr);
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
#include "lunar_green_thread.hpp"

uint64_t num = 0;

void
timer_func(void *arg)
{
    for (;;) {
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 5000);
        printf("%llu [ops/s]\n", num / 5);
        num = 0;
    }
}

void
func2(void *arg)
{
    auto rs = (lunar::shared_stream*)arg;
    
    for (;;) {
        lunar::select_green_thread(nullptr, 0, (void**)&rs, 1, false, 0);
        uint64_t n = 0;
        void *ret;
        while (lunar::pop_ptr(rs, &ret) != lunar::STRM_NO_MORE_DATA) n++;
        num += n;
    }
}

void
func1(void *arg)
{
    auto rs = new lunar::shared_stream;
    auto ws = new lunar::shared_stream;
    lunar::make_ptr_stream(rs, ws, 1);

    lunar::spawn_green_thread(func2, rs);
    
    lunar::schedule_green_thread();
    
    for (;;) {
        while(lunar::push_ptr(ws, nullptr) != lunar::STRM_NO_VACANCY);
        lunar::schedule_green_thread();
    }
}

int
main(int argc, char *argv[])
{
    lunar::init_green_thread(0);
    lunar::spawn_green_thread(timer_func);
    lunar::spawn_green_thread(func1);
    lunar::run_green_thread();

    return 0;
}
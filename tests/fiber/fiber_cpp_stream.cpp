#include "lunar_fiber.hpp"

uint64_t num = 0;

void
timer_func(void *arg)
{
    for (;;) {
        lunar::select_fiber(nullptr, 0, nullptr, 0, false, 5000);
        printf("%llu [ops/s]\n", num / 5);
        num = 0;
    }
}

void
func2(void *arg)
{
    auto rs = (lunar::shared_stream*)arg;
    
    for (;;) {
        lunar::select_fiber(nullptr, 0, (void**)&rs->shared_data->stream.ptr, 1, false, 0);
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

    lunar::spawn_fiber(func2, rs);
    
    lunar::yield_fiber();
    
    for (;;) {
        while(lunar::push_ptr(ws, nullptr) != lunar::STRM_NO_VACANCY);
        lunar::yield_fiber();
    }
}

int
main(int argc, char *argv[])
{
    lunar::init_fiber();
    lunar::spawn_fiber(timer_func);
    lunar::spawn_fiber(func1);
    lunar::run_fiber();

    return 0;
}
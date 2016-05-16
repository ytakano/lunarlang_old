#include "lunar_green_thread.hpp"

void
func3(void *arg)
{
    for (;;) {
        printf("func3\n");
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 6000);
    }
}

void
func2(void *arg)
{
    for (;;) {
        printf("func2\n");
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 4000);
    }
}

void
func1(void *arg)
{
    lunar::spawn_green_thread(func2);
    lunar::spawn_green_thread(func3);

    for (;;) {
        printf("func1\n");
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 2000);
    }
}

int
main(int argc, char *argv[])
{
    lunar::init_green_thread(0);
    lunar::spawn_green_thread(func1);
    lunar::run_green_thread();

    return 0;
}
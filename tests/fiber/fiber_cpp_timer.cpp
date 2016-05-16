#include "lunar_fiber.hpp"

void
func3(void *arg)
{
    for (;;) {
        printf("func3\n");
        lunar::select_fiber(nullptr, 0, nullptr, 0, false, 6000);
    }
}

void
func2(void *arg)
{
    for (;;) {
        printf("func2\n");
        lunar::select_fiber(nullptr, 0, nullptr, 0, false, 4000);
    }
}

void
func1(void *arg)
{
    lunar::spawn_fiber(func2);
    lunar::spawn_fiber(func3);

    for (;;) {
        printf("func1\n");
        lunar::select_fiber(nullptr, 0, nullptr, 0, false, 2000);
    }
}

int
main(int argc, char *argv[])
{
    lunar::init_fiber(0);
    lunar::spawn_fiber(func1);
    lunar::run_fiber();

    return 0;
}
#include "lunar_fiber.hpp"

void
func2(void *arg)
{
    printf("func2\n");
}

void
func1(void *arg)
{
    printf("func1\n");
    lunar::spawn_fiber(func2);
    lunar::yield_fiber();
}

int
main(int argc, char *argv[])
{
    lunar::init_fiber();
    lunar::spawn_fiber(func1);
    lunar::run_fiber();

    return 0;
}
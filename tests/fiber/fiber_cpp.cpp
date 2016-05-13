#include "lunar_fiber.hpp"

void
func1(void *arg)
{
    printf("func1\n");
}

int
main(int argc, char *argv[])
{
    lunar::init_fiber();
    lunar::spawn_fiber(func1);
    lunar::run_fiber();

    return 0;
}
#include "lunar_green_thread.hpp"

void
func2(void *arg)
{
    printf("func2\n");
}

void
func1(void *arg)
{
    printf("func1\n");
    lunar::spawn_green_thread(func2);
    lunar::schedule_green_thread();
}

int
main(int argc, char *argv[])
{
    lunar::init_green_thread(0);
    lunar::spawn_green_thread(func1);
    lunar::run_green_thread();

    return 0;
}
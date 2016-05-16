#include "lunar_fiber.hpp"

#include <thread>

void
func1(void *arg)
{
    printf("func1\n");
}

void
func2(void *arg)
{
    printf("func2\n");
}

void
thread2()
{
    lunar::init_fiber();
    lunar::spawn_fiber(func2);
    lunar::run_fiber();
}

void
thread1()
{
    lunar::init_fiber();
    lunar::spawn_fiber(func1);
    lunar::run_fiber();
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
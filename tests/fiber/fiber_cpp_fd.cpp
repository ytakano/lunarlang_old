#include "lunar_fiber.hpp"

#include <iostream>

void
func(void *arg)
{
    printf("> ");
    fflush(stdout);
    struct kevent kev;
    EV_SET(&kev, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
    lunar::select_fiber(&kev, 1, nullptr, 0, false, 10000);
    
    if (lunar::is_timeout_fiber()) {
        printf("\ntimeout!\n");
    } else {
        std::string s;
        std::cin >> s;
        printf("input = %s\n", s.c_str());
    }
}

int
main(int argc, char *argv[])
{
    lunar::init_fiber(0);
    lunar::spawn_fiber(func);
    lunar::run_fiber();

    return 0;
}
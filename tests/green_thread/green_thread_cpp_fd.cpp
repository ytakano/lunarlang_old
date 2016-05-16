#include "lunar_green_thread.hpp"

#include <iostream>

void
func(void *arg)
{
    printf("> ");
    fflush(stdout);

#ifdef KQUEUE
    struct kevent kev;
    EV_SET(&kev, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
    lunar::select_green_thread(&kev, 1, nullptr, 0, false, 10000);
#endif // KQUEUE

    if (lunar::is_timeout_green_thread()) {
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
    lunar::init_green_thread(0);
    lunar::spawn_green_thread(func);
    lunar::run_green_thread();

    return 0;
}
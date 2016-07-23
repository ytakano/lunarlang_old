#include "lunar_green_thread.hpp"

#include <iostream>

volatile int n = 0;

void
func3(void *arg) {
    auto ws = (lunar::shared_stream*)arg;
    int  num[2] = {0, 1};
    for (;;) {
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 11000);
        auto ret = lunar::push_stream_bytes(ws, (char*)num);
        num[0] += 2;
        num[1] += 2;
        assert(ret == lunar::STRM_SUCCESS);
        fflush(stdout);
    }
}

void
func1(void *arg)
{
    __sync_fetch_and_add(&n, 1);
    while(n != 2); // barrier

    auto rs = new lunar::shared_stream;
    auto ws = new lunar::shared_stream;
    lunar::make_bytes_stream(rs, ws, 1, sizeof(int) * 2);

    lunar::spawn_green_thread(func3, ws);

#ifdef KQUEUE
    struct kevent kev;
    EV_SET(&kev, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
#elif (defined EPOLL)
    epoll_event eev;
    eev.data.fd = STDIN_FILENO;
    eev.events  = EPOLLIN;
#endif // KQUEUE

    printf("> ");
    fflush(stdout);

    for (;;) {
#ifdef KQUEUE
        lunar::select_green_thread(&kev, 1, (void**)&rs, 1, true, 5000);
#elif (defined EPOLL)
        lunar::select_green_thread(&eev, 1, (void**)&rs, 1, true, 5000);
#endif // KQUEUE

        if (lunar::is_timeout_green_thread()) {
            printf("timeout!\n> ");
            fflush(stdout);
        }

        if (lunar::is_ready_threadq_green_thread()) {
            int num[2];
            auto ret = lunar::pop_threadq_green_thread((char*)num);
            assert(ret == lunar::STRM_SUCCESS);

            printf("recv thread queue! num[0] = %d, num[1] = %d\n> ", num[0], num[1]);
            fflush(stdout);
        }

        void **streams;
        ssize_t len;
        lunar::get_streams_ready_green_thread(&streams, &len);
        for (int i = 0; i < len; i++) {
            int num[2];
            auto ret = lunar::pop_stream_bytes((lunar::shared_stream*)streams[i], (char*)num);
            assert(ret == lunar::STRM_SUCCESS);
            printf("recv stream! num[0] = %d, num[1] = %d\n> ", num[0], num[1]);
            fflush(stdout);
        }

        lunar::fdevent_green_thread *fdev;
        lunar::get_fds_ready_green_thread(&fdev, &len);
        for (int i = 0; i < len; i++) {
            if (fdev[i].fd == STDIN_FILENO && fdev[i].event == FD_EV_READ) {
                std::string s;
                std::cin >> s;
                printf("input = %s\n> ", s.c_str());
                fflush(stdout);
            }
        }
    }
}

void
func2(void *arg)
{
    __sync_fetch_and_add(&n, 1);
    while(n != 2); // barrier

    auto fb = lunar::get_green_thread(1);
    int num[2] = {0, 1};
    for (;;) {
        lunar::select_green_thread(nullptr, 0, nullptr, 0, false, 13000);
        lunar::push_threadq_fast_unsafe_green_thread(fb, (char*)num);
        num[0] += 2;
        num[1] += 2;
    }
}

void
thread2()
{
    lunar::init_green_thread(2, 1, 1);
    lunar::spawn_green_thread(func2);
    lunar::run_green_thread();
}

void
thread1()
{
    lunar::init_green_thread(1, 1, sizeof(int) * 2);
    lunar::spawn_green_thread(func1);
    lunar::run_green_thread();
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
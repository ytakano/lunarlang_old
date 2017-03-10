#include "lunar_green_thread.hpp"

void
func(void *arg)
{

}

int
main(int argc, char *argv[])
{
    lunar::init_green_thread(0, 1, 1);

    for (int i = 0; i < 100000; i++) {
        lunar::spawn_green_thread(func);
    }

    return 0;
}

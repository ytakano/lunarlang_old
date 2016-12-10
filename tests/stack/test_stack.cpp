#include <lunar_slub_stack.hpp>

int
main(int argc, char *argv[])
{
    lunar::slub_stack slub;
    void* ptr_stack[1024 * 2];

    slub.print_state();

    for (int i = 0; i < 1024 + 32; i++) {
        ptr_stack[i] = slub.allocate();
    }

    slub.print_state();

    for (int i = 512; i < 512 + 128; i++) {
        slub.deallocate(ptr_stack[i]);
    }

    slub.print_state();

    for (int i = 256 - 5; i < 256 + 20; i++) {
        slub.deallocate(ptr_stack[i]);
    }

    slub.print_state();

    return 0;
}
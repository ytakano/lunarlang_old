#include "lunar_gt_x86_64_systemV.hpp"

namespace lunar {

void
green_thread_x86_64_systemV::yield()
{
    // スタックポインタなどのレジスタを復元
    // スタックにプログラムポインタを積む
    // rax = 1
    // ret
}

int
green_thread_x86_64_systemV::spawn(void (*func)(void *))
{
    return 0;
}

__attribute__((noinline))
void
green_thread_x86_64_systemV::ctx_switch()
{
    
}

__attribute__((noinline))
void
green_thread_x86_64_systemV::run()
{
    asm (
        "xorq %rax, %rax;"
//        "lzcntq %0, %1;"
//        : "=r" (max_level)
//        : "r" (m_size)
        );
    // %rax == 0
    // スタックポインタなどのレジスタを保存
    // プログラムカウンタを保存
    
    // if (%rax == 0) {
    //     yeild
    // }
}

}
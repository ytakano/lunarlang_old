#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"
#include "lunar_slab_allocator.hpp"

#include "lunar_ir.hpp"

#include <fstream>
#include <iostream>

int
main(int argc, char *argv[])
{
    lunar::lunar_ir ir;

    std::ifstream ifs("tests/ir/test01.lunar.ir");

    std::istreambuf_iterator<char> it(ifs);
    std::istreambuf_iterator<char> last;
    std::string str(it, last);

    ir.add_file(lunar::to_u32string(str), "tests/ir/test01.lunar.ir");

    ir.compile("tests/ir/test061.lunar.ir");

    ir.print();

    std::unordered_map<int, int, std::hash<int>, std::equal_to<int>, lunar::slab_allocator<std::pair<const int, int>>> um;
    //std::unordered_map<int, int, std::hash<int>, std::equal_to<int>> um;


    for (int i = 0; i < 100000; i++)
        um[i] = i;

    uint64_t n = 0;
    for (int i = 0; i < 100000; i++)
        n += um[i];

    printf("n = %llu\n", n);
/*
    int n = 0;
    lunar::hash_set<int> hs;
    lunar::hash_set<int>::iterator it_hs;

    for (int i = 0; i < 1000; i++) {
        hs.insert(i);
        n += i;
    }

    int m = 0;
    for (it_hs = hs.begin(); it_hs != hs.end(); ++it_hs) {
        m += *it_hs;
        printf("%d\n", *it_hs);
    }
    printf("n = %d, m = %d\n", n, m);

    for (int i = 0; i < 1000; i++) {
        hs.erase(i);
        n -= i;
    }

    printf("n = %d, m = %d\n", n, m);

    for (auto &val: hs) {
        printf("val = %d\n", val);
    }

    lunar::hash_map<int, int> hm;
    lunar::hash_map<int, int>::iterator it_hm;

    hm.insert({10, 20});
    hm[100] = 200;
    hm[100] = 300;
    hm[100] = 400;

    it_hm = hm.find(10);
    printf("val = %d\n", it_hm->second);
    printf("val = %d\n", hm[100]);
*/

    return 0;
}
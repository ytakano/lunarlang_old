#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"

#include "lunar_ir.hpp"
#include "lunar_hash.hpp"

#include <fstream>
#include <iostream>

int
main(int argc, char *argv[])
{
    lunar::lunar_ir ir;

    std::ifstream ifs("tests/ir/test06.lunar.ir");

    std::istreambuf_iterator<char> it(ifs);
    std::istreambuf_iterator<char> last;
    std::string str(it, last);

    ir.add_file(lunar::to_u32string(str), "tests/ir/test06.lunar.ir");

    ir.compile("tests/ir/test06.lunar.ir");

    ir.print();

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

    return 0;
}
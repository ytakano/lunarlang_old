#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"

#include "lunar_ir.hpp"

#include <fstream>
#include <iostream>

int
main(int argc, char *argv[])
{
    lunar::lunar_ir ir;

    std::ifstream ifs("tests/ir/test02.lunar.ir");

    std::istreambuf_iterator<char> it(ifs);
    std::istreambuf_iterator<char> last;
    std::string str(it, last);

    ir.add_file(lunar::to_u32string(str), "tests/ir/test02.lunar.ir");

    ir.compile("tests/ir/test02.lunar.ir");

    ir.print();

    return 0;
}
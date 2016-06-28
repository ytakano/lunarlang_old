#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"

#include "lunar_ir.hpp"

int
main(int argc, char *argv[])
{
    lunar::lunar_ir ir;

    ir.add_file(U"", "a.lunar.ir");
    ir.add_file(U"", "b.lunar.ir");
    ir.add_file(U"", "c.lunar.ir");

    ir.compile("a.lunar.ir");

    return 0;
}
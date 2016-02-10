#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

int
main(int argc, char *argv[])
{
    std::unique_ptr<lunar::stream<char32_t>> stream(new lunar::stream<char32_t>);
    lunar::parsec<char32_t> parsec(std::move(stream));
    
    auto set1 = parsec.make_char_set(U"あいう");

    return 0;
}
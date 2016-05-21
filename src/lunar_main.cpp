#include "lunar_common.hpp"
#include "lunar_parsec2.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"

#include <locale>
#include <codecvt>

int
main(int argc, char *argv[])
{
    lunar::shared_stream rs, ws;
    
    lunar::make_ptr_stream(&rs, &ws);
    
    lunar::parsec2<char32_t> parsec(&rs);

    return 0;
}
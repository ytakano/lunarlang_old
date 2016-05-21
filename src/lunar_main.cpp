#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_shared_stream.hpp"

int
main(int argc, char *argv[])
{
    lunar::shared_stream rs, ws;
    
    lunar::make_ptr_stream(&rs, &ws);
    
    lunar::parsec<char> parsec(&rs);

    return 0;
}
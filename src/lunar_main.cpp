#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_fiber.hpp"
#include "lunar_shared_stream.hpp"

#include <locale>
#include <codecvt>

class parse_error
{
public:
    parse_error(std::string msg, int line, int col) : m_msg(msg), m_line(line), m_col(col) { }

    std::string m_msg;
    int m_line;
    int m_col;
};

bool
one2nine(char32_t c)
{
    return U'1' <= c && c <= U'9';
}

bool
int_parser(lunar::parsec<char32_t> &parsec)
{
    auto psr = parsec.satisfy(one2nine) >> parsec.many(parsec.digit()) || parsec.character(U'0');
    
    return psr();
}

bool
float_parser(lunar::parsec<char32_t> &parsec)
{
    auto psr = parsec.func(int_parser) >> parsec.character(U'.') >> parsec.func(int_parser);
    
    return psr();
}

std::string
read_float(lunar::parsec<char32_t> &parsec)
{
    if (float_parser(parsec)) {
        return lunar::to_string(parsec.get_string());
    }
    
    return "";
}

void
test_parsec()
{
    lunar::init_fiber();

    auto func = [] () {
        auto rs = new lunar::shared_stream;
        auto ws = new lunar::shared_stream;
        lunar::make_ptr_stream(rs, ws, 32);
        
        lunar::parsec<char32_t> parsec(*rs);

        auto text = new std::u32string(U"12345.67abc");
        
        push_string(ws, text);
        push_eof_string(ws);
        
        auto num = read_float(parsec);
        printf("num = %s\n", num.c_str());

        deref_ptr_stream(rs);
        deref_ptr_stream(ws);
        delete rs;
        delete ws;
    };
    
    lunar::spawn_fiber(func);

    lunar::run_fiber();
    printf("end green thread\n");
}

void
thread1()
{
    for (;;) {
        printf("thread 1\n");
        lunar::yield_fiber();
    }
}

void
thread2()
{
    for (;;) {
        printf("thread 2\n");
        lunar::yield_fiber();
    }
}

void
thread3()
{
    for (;;) {
        printf("thread 3\n");
        lunar::yield_fiber();
    }
}

void
test_fiber()
{
    lunar::init_fiber();
    lunar::spawn_fiber(thread1);
    lunar::spawn_fiber(thread2);
    lunar::spawn_fiber(thread3);
    lunar::run_fiber();
}

int
main(int argc, char *argv[])
{
    //test_fiber();
    test_parsec();

    return 0;
}
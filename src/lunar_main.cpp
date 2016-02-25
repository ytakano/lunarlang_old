#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"
#include "lunar_ringq.hpp"
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

int
parse_int(lunar::parsec<char32_t> &parsec)
{
    auto p = parsec.satisfy(one2nine) >> parsec.many(parsec.digit()) || parsec.character(U'0');
    
    if (p()) {
        auto str = lunar::to_string(parsec.get_string());
        return lunar::to_int(str);
    }
    
    return 0;
}

void
test_parsec()
{
    lunar::init_green_thread();

    auto func = [] () {
        auto rs = new lunar::shared_stream;
        auto ws = new lunar::shared_stream;
        lunar::make_ptr_stream(rs, ws, 32);
        
        lunar::parsec<char32_t> parsec(*rs);

        auto text = new std::u32string(U"12345abc");
        
        push_string(ws, text);
        push_eof(ws);
        
        int num = parse_int(parsec);
        printf("num = %d\n", num);

        deref_ptr_stream(rs);
        deref_ptr_stream(ws);
        delete rs;
        delete ws;
    };
    
    lunar::spawn_green_thread(func);

    lunar::run_green_thread();
    printf("end green thread\n");
}

void
thread1()
{
    for (;;) {
        printf("thread 1\n");
        lunar::yield_green_thread();
    }
}

void
thread2()
{
    for (;;) {
        printf("thread 2\n");
        lunar::yield_green_thread();
    }
}

void
thread3()
{
    for (;;) {
        printf("thread 3\n");
        lunar::yield_green_thread();
    }
}

void
test_green_thread()
{
    lunar::init_green_thread();
    lunar::spawn_green_thread(thread1);
    lunar::spawn_green_thread(thread2);
    lunar::spawn_green_thread(thread3);
    lunar::run_green_thread();
}

int
main(int argc, char *argv[])
{
    test_green_thread();
    //test_parsec();

    return 0;
}
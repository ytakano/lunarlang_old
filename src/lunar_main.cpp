#include "lunar_common.hpp"
#include "lunar_parsec.hpp"
#include "lunar_green_thread.hpp"

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
    } else {
        throw parse_error("not integer!", parsec.get_line(), parsec.get_col());
    }
    
    return 0;
}

void
test_parsec()
{
    lunar::parsec<char32_t>::stream_t stream;
    lunar::parsec<char32_t>::chars_t  chars1, chars2, chars3;
    lunar::parsec<char32_t> parsec(stream);
    
    auto text = llvm::make_unique<std::u32string>(U"12345abc");
    
    stream.push_back(std::move(text));
    
    try {
        int num = parse_int(parsec);
        printf("num = %d\n", num);
    } catch (parse_error err) {
        printf("error: %s (line = %d, col = %d)\n", err.m_msg.c_str(), err.m_line, err.m_col);
    }
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

    return 0;
}
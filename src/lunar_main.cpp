#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

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
    
    p();
    
    if (parsec.get_result()) {
        auto str = lunar::to_string(parsec.get_string());
        return lunar::to_int(str);
    } else {
        throw parse_error("not integer!", parsec.get_line(), parsec.get_col());
    }
    
    return 0;
}

int
main(int argc, char *argv[])
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

    return 0;
}
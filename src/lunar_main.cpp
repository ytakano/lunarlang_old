#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

#include <locale>
#include <codecvt>

class foo {
public:
    virtual ~foo() { }
    virtual void say() { };
};

class bar : public foo {
public:
    virtual ~bar() { }

    virtual void say() { printf("hello!\n"); }
};

void func(foo &f)
{
    f.say();
}

int
main(int argc, char *argv[])
{
    bar b;
    func(b);

    lunar::parsec<char32_t>::stream_t stream;
    lunar::parsec<char32_t>::chars_t  chars1, chars2, chars3;
    lunar::parsec<char32_t> parsec(stream);
    
    auto text = llvm::make_unique<std::u32string>(U"aeb2cda, c3d4e6");
    
    stream.push_back(std::move(text));
    
    parsec.make_chars(chars1, U"abc");
    parsec.make_chars(chars2, U"123");
    parsec.make_chars(chars3, U"def");
    auto p1 = parsec.one_of(chars1);
    auto p2 = parsec.one_of(chars2);
    auto p3 = parsec.one_of(chars3);
    auto p4 = p1 >> p2;
    auto p5 = p1 >> p3;
    auto p6 = parsec.try_parse(p4);
    auto p7 = p6 || p5;
    auto p8 = parsec.many(p7);
    
    p8();
    
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> converter;
    auto str = converter.to_bytes(parsec.get_string());
    auto &msg = parsec.get_msg();
    
    auto msgstr = converter.to_bytes(msg.str);
    
    printf("%s\n", str.c_str());
    printf("%s\n", msgstr.c_str());

    return 0;
}
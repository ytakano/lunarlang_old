#ifndef LUNAR_STRING_HPP
#define LUNAR_STRING_HPP

#include <string>
#include <locale>
#include <codecvt>

namespace lunar {
    
static std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> string_converter;

inline
std::u32string
to_u32string(int n)
{
    char str[16];
    snprintf(str, sizeof(str), "%d", n);
    return string_converter.from_bytes(str);
}

inline
std::u32string
to_u32string(char *str)
{
    return string_converter.from_bytes(str);
}

inline
std::u32string
to_u32string(const std::string &str)
{
    return string_converter.from_bytes(str);
}

inline
std::string
to_string(char32_t c)
{
    return string_converter.to_bytes(c);
}

inline
std::string
to_string(char c)
{
    std::string str;
    str.push_back(c);
    return str;
}

inline
std::string
to_string(const std::u32string &str)
{
    return string_converter.to_bytes(str);
}

inline
std::string &
to_string(std::string &str)
{
    return str;
}

inline
std::string
str_convert(const std::u32string &str)
{
    return string_converter.to_bytes(str);
}

inline
std::u32string
str_convert(const std::string &str)
{
    return string_converter.from_bytes(str);
}

inline
int
to_int(const std::string &str)
{
    int n = 0;
    int i = 1;
    
    for (auto it = str.rbegin(); it != str.rend(); ++it) {
        n += i * (*it - U'0');
        i *= 10;
    }
    
    return n;
}

}

#endif // LUNAR_STRING_HPP
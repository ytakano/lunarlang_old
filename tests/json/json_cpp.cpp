#include <lunar_parsec.hpp>

#include <iostream>
#include <limits>

#include <stdlib.h>

typedef std::numeric_limits< double > dbl;

class json_val {
public:
    json_val() { }
    virtual ~json_val() { }
};

class json_double : public json_val {
public:
    json_double(double num) : m_num(num) { }
    virtual ~json_double() { }
    
    double m_num;
};

class json_string : public json_val {
public:
    json_string() { }
    virtual ~json_string() { }
    
    std::string m_str;
};

std::string
parse_frac(lunar::parsec<char> &ps)
{
    std::string s;

    auto dot = ps.character('.')();
    if (! ps.is_success())
        return s;
    
    s  = ".";
    s += ps.parse_many1_char(ps.parse_digit())();
    
    return s;
}

std::string
parse_digit1_9(lunar::parsec<char> &ps)
{
    auto one2nine = [](char c) -> bool {
        return '1' <= c && c <= '9';
    };
    
    std::string s;
    
    auto c = ps.satisfy(one2nine)();
    if (! ps.is_success())
        return s;

    s += c;
    s += ps.parse_many_char(ps.parse_digit())();

    return s;
}

// number          = [ minus ] int [ frac ] [ exp ]
json_double
parse_number(lunar::parsec<char> &ps)
{
    std::string s;

    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto sign = ps.character('-')();
        if (ps.is_success())
            s += "-";
    }
    
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto zero = ps.character('0')();
        if (ps.is_success()) {
            return json_double(0);
        }
    }
    
    s += parse_digit1_9(ps);
    if (! ps.is_success()) {
        return json_double(0);
    }
    
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto frac = parse_frac(ps);
        if (ps.is_success())
            s += frac;
    }
    
    ps.set_is_success(true);
    
    return json_double(strtod(s.c_str(), nullptr));
}

bool
is_unescaped(char c)
{
    if (c == '\x20' || c == '\x21' ||
        ('\x23' <= c && c <= '\x5b') ||
        ('\x5d' <= c && c <= '\x7f') ||
        ((char)-128 <= c && c <= (char)-1))
        return true;
    
    return false;
}

// string          = quotation-mark *char quotation-mark
json_string
parse_string(lunar::parsec<char> &ps)
{
    json_string ret;
    
    ps.character('"')();
    if (! ps.is_success())
        return ret;
    
    char c;
    {
        lunar::parsec<char>::parser_try ptry(ps);
        c = ps.satisfy(is_unescaped)();
    }
    if (ps.is_success())
        ret.m_str += c;
    else
        ps.set_is_success(true);
    
    ps.character('"')();
    if (! ps.is_success())
        return ret;

    return ret;
}

void
parser_json(void *arg)
{
    auto rs = (lunar::shared_stream*)arg;
    
    lunar::parsec<char> ps(rs);
    
    json_double d = parse_number(ps);
    if (ps.is_success()) {
        std::cout.precision(dbl::max_digits10);
        std::cout << "input = " << d.m_num << "\n> " << std::flush;
    } else {
        std::cout << "failed to parse\n> " << std::flush;
    }
    
    lunar::deref_ptr_stream(rs);
}

void
read_stdin(void *arg)
{
    struct kevent kev;
    EV_SET(&kev, STDIN_FILENO, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);

    std::cout << "> " << std::flush;
    for (;;) {
        lunar::select_green_thread(&kev, 1, nullptr, 0, false, 0);
        auto str = new std::string;
        auto rs  = new lunar::shared_stream;
        auto ws  = new lunar::shared_stream;

        lunar::make_ptr_stream(rs, ws, 32);

        std::cin >> *str;
        
        lunar::spawn_green_thread(parser_json, rs);
        
        lunar::push_ptr(ws, str);
        lunar::push_eof(ws);
        
        lunar::deref_ptr_stream(ws);
    }
}

int
main(int argc, char **argv)
{
    lunar::init_green_thread(0);
    
    lunar::spawn_green_thread(read_stdin, nullptr);
    lunar::run_green_thread();
    
    return 0;
}
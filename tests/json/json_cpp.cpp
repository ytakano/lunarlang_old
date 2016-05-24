#include <lunar_parsec.hpp>
#include <iostream>
#include <stdlib.h>

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

std::string parse_many_digit(lunar::parsec<char> &ps);

std::string
parse_frac(lunar::parsec<char> &ps)
{
    std::string s;

    auto dot = ps.character('.')();
    
    if (! dot)
        return s;
    
    auto c = ps.parse_digit()();
    
    if (! c)
        return s;
    
    s  = ".";
    s += c.m_char;
    s += parse_many_digit(ps);
    
    return s;
}

std::string
parse_many_digit(lunar::parsec<char> &ps)
{
    std::string s;

    // many digit
    for (;;) {
        lunar::parsec<char>::parser_try t(ps);
        auto c = ps.parse_digit()();

        if (! c)
            break;

        s += c.m_char;
    }
    
    ps.set_is_result(true);
    
    return s;
}

std::string
parse_digit1_9(lunar::parsec<char> &ps)
{
    auto one2nine = [](char c) -> lunar::parsec<char>::char_t {
        return {c, '1' <= c && c <= '9'};
    };
    
    std::string s;
    
    auto c = ps.satisfy(one2nine)();
    if (! c)
        return s;

    s += c.m_char;
    s += parse_many_digit(ps);

    return s;
}

// number          = [ minus ] int [ frac ] [ exp ]
json_double
parse_number(lunar::parsec<char> &ps)
{
    std::string s;

    {
        lunar::parsec<char>::parser_try t(ps);
        auto sign = ps.character('-')();
        
        if (sign)
            s += "-";
    }
    
    {
        lunar::parsec<char>::parser_try t(ps);
        auto zero = ps.character('0')();
        
        if (zero) {
            return json_double(0);
        }
    }
    
    s += parse_digit1_9(ps);
    
    std::cout << s << std::endl;
    
    if (! ps.get_is_result()) {
        return json_double(0);
    }
    
    {
        lunar::parsec<char>::parser_try t(ps);
        auto frac = parse_frac(ps);
        
        if (ps.get_is_result())
            s += frac;
    }
    
    ps.set_is_result(true);
    
    return json_double(strtod(s.c_str(), nullptr));
}

void
parser_json(void *arg)
{
    auto rs = (lunar::shared_stream*)arg;
    
    lunar::parsec<char> ps(rs);
    
    json_double d = parse_number(ps);
    if (ps.get_is_result()) {
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
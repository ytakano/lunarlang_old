#include <lunar_parsec.hpp>

#include <iostream>
#include <limits>

#include <stdlib.h>

typedef std::numeric_limits< double > dbl;

class json_val {
public:
    json_val() { }
    virtual ~json_val() { }
    
    virtual void print(std::ostream &os) const = 0;
};

class json_double : public json_val {
public:
    json_double(double num) : m_num(num) { }
    virtual ~json_double() { }

    virtual void print(std::ostream &os) const {
        os << m_num;
    }
    
    double m_num;
};

class json_string : public json_val {
public:
    json_string() { }
    virtual ~json_string() { }

    virtual void print(std::ostream &os) const {
        os << "\"" << m_str << "\"";
    }
    
    std::string m_str;
};

std::ostream&
operator<< (std::ostream& os, const json_val &lhs)
{
    lhs.print(os);
    return os;
}

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

// exp             = e [ minus / plus ] 1*DIGIT
std::string
parse_exp(lunar::parsec<char> &ps)
{
    std::string s;
    
    auto c = ps.character('e')();
    if (! ps.is_success())
        return s;
    
    s += c;

    char sign;
    {
        lunar::parsec<char>::parser_try ptry(ps);
        sign = ps.character('-')();
    }

    if (ps.is_success()) {
        s += sign;
    } else {
        lunar::parsec<char>::parser_try ptry(ps);
        sign = ps.character('+')();
        
        if (ps.is_success())
            s += sign;
    }
    
    ps.set_is_success(true);
    
    // int
    s += parse_digit1_9(ps);
    return s;
}

// number          = [ minus ] int [ frac ] [ exp ]
std::unique_ptr<json_double>
parse_number(lunar::parsec<char> &ps)
{
    std::string s;

    // [ minus ]
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto sign = ps.character('-')();
        if (ps.is_success())
            s += "-";
    }
    ps.set_is_success(true);
    
    // 0
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto zero = ps.character('0')();
        if (ps.is_success()) {
            return nullptr;
        }
    }
    
    // int
    s += parse_digit1_9(ps);
    if (! ps.is_success()) {
        return nullptr;
    }
    
    // [ frac ]
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto frac = parse_frac(ps);
        if (ps.is_success())
            s += frac;
    }
    ps.set_is_success(true);
    
    // [ exp ]
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto exp = parse_exp(ps);
        if (ps.is_success())
            s += exp;
    }
    ps.set_is_success(true);
    
    std::cout << "s = " << s << std::endl;
    
    return llvm::make_unique<json_double>(strtod(s.c_str(), nullptr));
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
std::unique_ptr<json_string>
parse_string(lunar::parsec<char> &ps)
{
    auto ret = llvm::make_unique<json_string>();
    
    ps.character('"')();
    if (! ps.is_success())
        return ret;
    
    for (;;) {
        char c;
        {
            lunar::parsec<char>::parser_try ptry(ps);
            c = ps.satisfy(is_unescaped)();
        }

        if (ps.is_success())
            ret->m_str += c;
        else {
            ps.set_is_success(true);
        
            {
                lunar::parsec<char>::parser_try ptry(ps);
                ps.character('\\')();
                if (! ps.is_success()) {
                    ps.set_is_success(true);
                    break;
                }
                
                auto func = [](char rhs) -> bool {
                    return rhs == '"' || rhs == '\\' || rhs == '/' || rhs == 'b' ||
                           rhs == 'f' || rhs == 'n'  || rhs == 'r' || rhs == 't';
                };
                
                c = ps.satisfy(func)();
            }

            if (ps.is_success()) {
                auto conv = [](char rhs) -> char {
                    if (rhs == 'n') return '\n';
                    else if (rhs == 'r') return '\r';
                    else if (rhs == 't') return '\t';
                    else if (rhs == 'f') return '\f';
                    else if (rhs == 'b') return '\b';
                    else return rhs;
                };
                ret->m_str += conv(c);
            } else {
                ps.character('u')();
                if (! ps.is_success())
                    return ret;
                
                auto is_hexdig = [](char x) -> bool {
                    return ('0' <= x && x <= '9') || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F');
                };
                
                auto c1 = ps.satisfy(is_hexdig)();
                if (! ps.is_success())
                    return ret;

                auto c2 = ps.satisfy(is_hexdig)();
                if (! ps.is_success())
                    return ret;
                
                if (c1 != 0 && c2 != 0)
                    ret->m_str += c1 * 16 + c2;

                auto c3 = ps.satisfy(is_hexdig)();
                if (! ps.is_success())
                    return ret;
                
                auto c4 = ps.satisfy(is_hexdig)();
                if (! ps.is_success())
                    return ret;
                
                ret->m_str += c3 * 16 + c4;
            }
        }
    }
    
    ps.character('"')();
    if (! ps.is_success())
        return ret;

    return ret;
}

std::unique_ptr<json_val>
parse_value(lunar::parsec<char> &ps)
{
    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto num = parse_number(ps);
        if (ps.is_success())
            return std::move(num);
    }
    
    auto str = parse_string(ps);
    if (ps.is_success())
        return std::move(str);
    
    return nullptr;
}

void
parser_json(void *arg)
{
    auto rs = (lunar::shared_stream*)arg;
    
    lunar::parsec<char> ps(rs);
    
    auto val = parse_value(ps);
    if (ps.is_success()) {
        std::cout.precision(dbl::max_digits10);
        std::cout << "input = " << *val << "\n> " << std::flush;
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
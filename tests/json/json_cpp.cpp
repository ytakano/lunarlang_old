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

class json_array : public json_val {
public:
    json_array() { }
    virtual ~json_array() { }

    virtual void print(std::ostream &os) const {
        os << "[";

        if (! m_vals.empty()) {
            auto it = m_vals.begin();
            for (;;) {
                (*it)->print(os);
                
                ++it;
                if (it == m_vals.end())
                    break;
                
                os << ",";
            }
        }
        
        os << "]";
    }
    
    std::vector<std::unique_ptr<json_val>> m_vals;
};

class json_object : public json_val {
public:
    json_object() { }
    virtual ~json_object() { }
    
    virtual void print(std::ostream &os) const {
        os << "{";
        
        if (! m_vals.empty()) {
            auto it = m_vals.begin();
            for (;;) {
                it->first->print(os);
                os << ":";
                it->second->print(os);
                
                ++it;
                if (it == m_vals.end())
                    break;
                
                os << ",";
            }
        }
        
        os << "}";
    }
    
    std::vector<std::pair<std::unique_ptr<json_string>, std::unique_ptr<json_val>>> m_vals;
};

class json_bool : public json_val {
public:
    json_bool(bool val) : m_val(val) {}
    virtual ~json_bool() { }
    
    virtual void print(std::ostream &os) const {
        if (m_val)
            os << "true";
        else
            os << "false";
    }
    
    bool m_val;
};

class json_null : public json_val {
public:
    virtual void print(std::ostream &os) const { os << "null"; }
};

std::unique_ptr<json_val>    parse_value(lunar::parsec<char> &ps);
std::unique_ptr<json_string> parse_string(lunar::parsec<char> &ps);

std::ostream&
operator<< (std::ostream& os, const json_val &lhs)
{
    lhs.print(os);
    return os;
}

void
parse_ws(lunar::parsec<char> &ps)
{
    auto func = [](char c) {
        return c == '\x20' || c == '\x09' || c == '\x0a' || c == '\x0d';
    };
    
    auto ws = ps.parse_many_char(ps.satisfy(func))();
}

void
parse_separator(lunar::parsec<char> &ps, char c)
{
    parse_ws(ps);
    
    auto x = ps.character(c)();
    if (! ps.is_success())
        return;
    
    parse_ws(ps);
}

std::unique_ptr<json_null>
parse_null(lunar::parsec<char> &ps)
{
    ps.parse_string("null")();
    if (! ps.is_success())
        return nullptr;
    
    return llvm::make_unique<json_null>();
}

std::unique_ptr<json_bool>
parse_false(lunar::parsec<char> &ps)
{
    ps.parse_string("false")();
    if (! ps.is_success())
        return nullptr;
    
    return llvm::make_unique<json_bool>(false);
}

std::unique_ptr<json_bool>
parse_true(lunar::parsec<char> &ps)
{
    ps.parse_string("true")();
    if (! ps.is_success())
        return nullptr;
    
    return llvm::make_unique<json_bool>(true);
}

void
parse_member(lunar::parsec<char> &ps, json_object *ret)
{
    auto key = parse_string(ps);
    if (! ps.is_success())
        return;

    parse_separator(ps, ':');
    if (! ps.is_success())
        return;

    auto val = parse_value(ps);
    if (! ps.is_success())
        return;
    
    ret->m_vals.push_back({std::move(key), std::move(val)});
}

void
parse_sp_member(lunar::parsec<char> &ps, json_object *ret)
{
    parse_ws(ps);
    
    ps.character(',')();
    if (! ps.is_success())
        return;

    parse_ws(ps);
    
    parse_member(ps, ret);
}

void
parse_members(lunar::parsec<char> &ps, json_object *ret)
{
    parse_member(ps, ret);
    if (!ps.is_success())
        return;

    parse_ws(ps);
    
    for (;;) {
        lunar::parsec<char>::parser_try ptry(ps);
        parse_sp_member(ps, ret);
        if (! ps.is_success())
            break;
    }
    ps.set_is_success(true);
}

std::unique_ptr<json_object>
parse_object(lunar::parsec<char> &ps)
{
    auto ret = llvm::make_unique<json_object>();
    
    parse_separator(ps, '{');
    if (! ps.is_success())
        return nullptr;

    {
        lunar::parsec<char>::parser_try ptry(ps);
        parse_members(ps, ret.get());
    }
    ps.set_is_success(true);
    
    parse_separator(ps, '}');
    if (! ps.is_success())
        return nullptr;
    
    return ret;
}

std::unique_ptr<json_val>
parse_sp_value(lunar::parsec<char> &ps)
{
    parse_ws(ps);

    ps.character(',')();
    if (! ps.is_success())
        return nullptr;

    parse_ws(ps);
    
    return parse_value(ps);
}

void
parse_values(lunar::parsec<char> &ps, json_array *arr)
{
    auto val = parse_value(ps);
    if (! ps.is_success())
        return;

    arr->m_vals.push_back(std::move(val));
    
    parse_ws(ps);

    for (;;) {
        lunar::parsec<char>::parser_try ptry(ps);
        val = parse_sp_value(ps);
        if (! ps.is_success())
            break;
        
        arr->m_vals.push_back(std::move(val));
    }
    ps.set_is_success(true);
}

std::unique_ptr<json_array>
parse_array(lunar::parsec<char> &ps)
{
    auto ret = llvm::make_unique<json_array>();
    
    parse_separator(ps, '[');
    if (! ps.is_success()) {
        return nullptr;
    }
    
    {
        lunar::parsec<char>::parser_try ptry(ps);
        parse_values(ps, ret.get());
    }
    ps.set_is_success(true);
    
    parse_separator(ps, ']');
    if (! ps.is_success())
        return nullptr;
    
    return ret;
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
        auto val = parse_false(ps);
        if (ps.is_success())
            return std::move(val);
    }

    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto val = parse_null(ps);
        if (ps.is_success())
            return std::move(val);
    }

    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto val = parse_true(ps);
        if (ps.is_success())
            return std::move(val);
    }

    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto obj = parse_object(ps);
        if (ps.is_success())
            return std::move(obj);
    }

    {
        lunar::parsec<char>::parser_try ptry(ps);
        auto arr = parse_array(ps);
        if (ps.is_success())
            return std::move(arr);
    }
    
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
    delete rs;
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

        std::getline(std::cin, *str);
        std::cout << *str << std::endl;
        
        lunar::spawn_green_thread(parser_json, rs);
        
        lunar::push_ptr(ws, str);
        lunar::push_eof(ws);
        
        lunar::deref_ptr_stream(ws);
        delete ws;
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
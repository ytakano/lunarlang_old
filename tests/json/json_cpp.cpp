#include <lunar_parsec.hpp>

class json_val {
public:
    json_val() { }
    virtual ~json_val() { }
};

class json_double : public json_val {
public:
    json_double() { }
    virtual ~json_double() { }
    
    double m_num;
};

class json_string : public json_val {
public:
    json_string() { }
    virtual ~json_string() { }
    
    std::u32string m_str;
};

std::unique_ptr<json_double>
parse_number(lunar::parsec<char32_t> &ps)
{
    return nullptr;
}

std::unique_ptr<json_string>
parse_string(lunar::parsec<char32_t> &ps)
{
    auto c1 = ps.character('"');
    
    if (! c1()) {
        return nullptr;
    }
    
    return nullptr;
}

int
main(int argc, char **argv)
{
    
    return 0;
}
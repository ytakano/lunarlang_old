#include <lunar_parsec2.hpp>

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

int
main(int argc, char **argv)
{
    lunar::init_fiber();
    
    auto func = [] () {
        auto rs = new lunar::shared_stream;
        auto ws = new lunar::shared_stream;
        lunar::make_ptr_stream(rs, ws, 32);
        
        lunar::parsec2<char32_t> parsec(*rs);

        auto text = new std::u32string(U"12345.67abc");
        
        push_string(ws, text);
        push_eof_string(ws);
        
        deref_ptr_stream(rs);
        deref_ptr_stream(ws);
        delete rs;
        delete ws;
    };
    
    lunar::spawn_fiber(func);
    lunar::run_fiber();
    
    return 0;
}
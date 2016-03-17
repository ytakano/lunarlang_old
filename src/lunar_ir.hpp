#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"

#include <vector>

namespace lunar {

class lunar_ir_ast {
public:
    lunar_ir_ast() { }
    virtual ~lunar_ir_ast() { }
};

class lunar_ir_type_ast : public lunar_ir_ast {
public:
    enum MEMTYPE {
        UNIQUE,
        SHARED,
        REFERENCE,
        IMMOVABLE,
    };
    
    lunar_ir_type_ast(MEMTYPE memtype, const std::u32string &tname) : m_memtype(memtype), m_typename(tname) { }
    ~lunar_ir_type_ast() { }
    
    MEMTYPE get_memtype() { return m_memtype; }
    const std::u32string & get_typename() { return m_typename; }

private:
    MEMTYPE m_memtype;
    std::u32string m_typename;
};

class lunar_ir_variable_ast : public lunar_ir_ast {
public:
    lunar_ir_variable_ast(std::unique_ptr<lunar_ir_type_ast> type, const std::u32string &name)
        : m_type(std::move(type)), m_name(name) { }
    virtual ~lunar_ir_variable_ast() { }
    
    const lunar_ir_type_ast & get_type() const { return *m_type; }
    const std::u32string & get_name() const { return m_name; }

private:
    std::unique_ptr<lunar_ir_type_ast> m_type;
    std::u32string m_name;
};

class lunar_ir_list_ast : public lunar_ir_ast {
public:
//    lunar_ir_list_ast(std::unique_ptr)

private:

};

}

#endif // LUNAR_IR_HPP
#ifndef LUNAR_LISP_HPP
#define LUNAR_LISP_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"

namespace lunar {

class lunar_lisp_ast {
public:
    lunar_lisp_ast() { }
    virtual ~lunar_lisp_ast() { }
};

class lunar_lisp_variable_ast : public lunar_lisp_ast {
public:
    lunar_lisp_variable_ast() { }
    virtual ~lunar_lisp_variable_ast() { }

private:
    enum {
        UNIQUE,
        SHARED,
        REFERENCE,
    } m_memtype;
    std::u32string m_name;
    std::u32string m_type;
};

}

#endif // LUNAR_LISP_HPP
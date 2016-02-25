#ifndef LUNAR_LISP_HPP
#define LUNAR_LISP_HPP

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
};

}

#endif // LUNAR_LISP_HPP
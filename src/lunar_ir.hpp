#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"

#include <vector>

/*
 * -----------------------------------------------------------------------------
 *
 * IR  := TOP*
 * TOP := FUNC | STRUCT | UNION | DATA | GLOBAL | EXPORT | IMPORT
 * STATEMENT := LET | IF | COND | WHILE | SELECT
 * GLOBAL := ( global ( ( TYPE (IDENTIFIER+) EXPRIDENTLIT )+ ) )
 * EXPORT := ( export IDENTIFIER+ )
 * IMPORT := ( import STR32+ )
 *
 * -----------------------------------------------------------------------------
 *
 * TYPE  := TYPE0 | ( OWNERSHIP TYPE0 )
 * TYPE0 := SCALAR | VECTOR | STRING | BINARY | LIST | STRUCT | DICT | SET | DATA |
 *          FUNCTYPE | RSTREAM | WSTREAM | PTR | UNION | PARSEC | IDENTIFIER
 *
 * OWNERSHIP := unique | shared | ref
 *
 * SCALAR     := SCALARTYPE INITSCALAR | SCALARTYPE
 * SCALARTYPE := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | char | ATOM
 *
 * ATOM := `IDENTIFIER
 *
 * VECTOR := ( vector TYPE SIZE )
 * SIZE := An integer greater than or equal to 0
 *
 * STRING := string
 *
 * BINARY := binary
 *
 * LIST := ( list TYPE )
 *
 * STRUCT := ( struct IDENTIFIER? ( TYPE IDENTIFIER )+ )
 *
 * DATA := ( data IDENTIFIER? ( TYPE IDENTIFIER )+ )
 *
 * UNION := ( union IDENTIFIER? ( TYPE IDENTIFIER )+ )
 *
 * DICT := ( dict TYPE TYPE )
 *
 * SET := ( set TYPE )
 *
 * FUNCTYPE := ( func ( TYPE* ) ( TYPE* ) )
 *
 * RSTREAM := ( rstrm TYPE )
 * WSTREAM := ( wstrm TYPE )
 *
 * PTR := (ptr TYPE ) | ( ptr PTR )
 *
 * PARSEC := parsec
 *
 * -----------------------------------------------------------------------------
 *
 * STEXPR := STATMENT | EXPR
 *
 * FUNC := ( defun IDENTIFIER ( TYPE* ) ( TYPE IDENTIFIER )* STEXPR* )
 * 
 * LET := ( let ( ( TYPE ( IDENTIFIER+ ) EXPRIDENTLIT )* ) STEXPR* )
 *
 * IF := ( if EXPRIDENTLIT EXPRIDENTLIT EXPRIDENTLIT )
 *
 * COND := ( cond ( EXPRIDENTLIT STEXPR* )+ ( else STEXPR* )? )
 *
 * BREAK := ( break )
 *
 * WHILE := ( while EXPRIDENTLIT STEXPR* )
 *
 * SELECT := ( select ( EXPRIDENT STEXPR*)* ( timeout SIZE STEXPR* )? )
 *
 * -----------------------------------------------------------------------------
 *
 * EXPRIDENT := EXPR | IDENTIFIER
 *
 * CALLFUNC := ( EXPRIDENT EXPRIDENTLIT* )
 *
 * RETURN := ( return ( EXPRIDENTLIT* ) )
 *
 * LAMBDA := ( lambda ( TYPE* ) ( TYPE IDENTIFIER )* STEXPR* )
 *
 * NEW := ( new TYPE )
 *
 * STORE := ( store! EXPRIDENT EXPRIDENTLIT )
 *
 * ASSOC := ( assoc! EXPRIDENT EXPRIDENT )
 *
 * TYPEOF := ( type TYPE0 IDENTIFIER )
 *
 * MKSTREAM := ( mkstream TYPE SIZE )
 *
 * PUSH := ( push! EXPRIDENTLIT )
 *
 * POP := ( pop! EXPRIDENTLIT )
 *
 * SPAWN := ( spawn SIZE EXPRIDENT EXPRIDENTLIT SIZE )
 *
 * SCHEDULE := ( schedule )
 *
 * THREAD := ( thread ATOM TYPE SIZE EXPRIDENT EXPRIDENTLIT )
 * 
 * SPIN_LOCK_INIT := ( spin_lock_init EXPRIDENT )
 * SPIN_LOCK      := ( spin_lock EXPRIDENT )
 * SPIN_TRY_LOCK  := ( spin_try_lock EXPRIDENT )
 * SPIN_UNLOCK    := ( spin_unlock EXPRIDENT )
 * 
 * HTM_LOCK_INIT := ( htm_lock_init EXPRIDENT )
 * HTM_LOCK      := ( htm_lock EXPRIDENT )
 * HTM_UNCLOK    := ( htm_unlock EXPRIDENT )
 *
 * PARSECINIT   := ( parser_init string EXPRIDENT ) | ( parsec_init binary EXPRIDENT )
 * PARSEC       := ( parse EXPRIDENT PARSECOPS EXPRIDENTLIT* )
 * PARSECOPS    := PARSECCHAR | PARSECMANY | PARSECMANY1 | PARSECTRY | PARSECTRYEND | PARSECLA | PARSECLAEND | PARSECDIGIT | PARSECHEX | PARSECOCT | PARSECSPACE | PARSECSATIS | PARSECSTR 
 * PARSECCHAR   := character
 * PARSECTRY    := try
 * PARSERTRYEND := try_end
 * PARSECLA     := look_ahead
 * PARSECLAEND  := look_ahead_end
 * PARSECDIGT   := digit
 * PARSECHEX    := hex
 * PARSECOCT    := oct
 * PARSECSPACE  := space
 * PARSECSATIS  := satisfy
 * PARSECSTR    := string
 * PARSECRESULT := result
 *
 * CCALL := ( ccall IDENTIFIER EXPRIDENTLIT* )
 *
 * DLOPEN := ( dlopen EXPRIDENTLIT )
 *
 * DEREF := ( deref EXPRIDENT )
 *
 * INCCNT := ( inccnt EXPRIDENT )
 * DECCNT := ( deccnt EXPRIDENT )
 *
 * ADD   := (+ EXPRIDENTLIT EXPRIDENTLIT+ )
 * MINUS := (- EXPRIDENTLIT EXPRIDENTLIT+ )
 * MULTI := (* EXPRIDENTLIT EXPRIDENTLIT+ )
 * DIV   := (/ EXPRIDENTLIT EXPRIDENTLIT+ )
 * MOD   := (mod EXPRIDENTLIT EXPRIDENTLIT+ )
 *
 * PRINT := ( print EXPRIDENTLIT )
 *
 * TOSTR := ( tostr EXPRIDENTLIT )
 *
 * -----------------------------------------------------------------------------
 *
 * LITERAL := STR32 | STR8 | CHAR32 | CHAR8 | INT | FLOAT | HEX | OCT | BIN
 *
 * STR32  := " CHARS* "
 * STR8   := b " CHARS* "
 * ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
 * CHARS  := ESCAPE | ESCAPE以外の文字
 *
 * CHAR32 := ' CHARS '
 * CHAR8  := ' CHARS '
 *
 * INT     := -? DIGIT
 * DIGIT   := NUM1to9 NUM0to9* | 0
 * NUM1to9 := 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 * NUM0to9 := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 *
 * FLOAT := NUM . EXP
 * EXP   := EE SIGN NUM+
 * EE    := e | E
 * SIGN  := - | +
 *
 * HEX     := 0x HEXNUM2\* | 0X HEXNUM2\*
 * HEXNUM2 := HEXNUM HEXNUM
 * HEXNUM  := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | A | b | B | c | C | d | D | f | F
 *
 * OCT    := 0 OCTNUM*
 * OCTNUM := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
 *
 * BIN    := b BINNUM\* | B BINNUM\*
 * BINNUM := 0 | 1
 *
 * TRUE  := true
 * FALSE := false
 */

namespace lunar {

enum LANG_OWNERSHIP {
    OWN_UNIQUE,
    OWN_SHARED,
    OWN_IMMOVABLE,
    OWN_REF,
};

enum LANG_SCALAR {
    SC_BOOL,
    SC_U64,
    SC_S64,
    SC_U32,
    SC_S32,
    SC_U16,
    SC_S16,
    SC_U8,
    SC_S8,
    SC_DOUBLE,
    SC_FLOAT,
    SC_CHAR,
    SC_ATOM,
};

enum LANG_BASIC_TYPE {
    BT_SCALAR,
    BT_VECTOR,
    BT_STRING,
    BT_BINARY,
    BT_LIST,
    BT_STRUCT,
    BT_DICT,
    BT_SET,
    BT_DATA,
    BT_UNION,
    BT_FUNCTYPE,
    BT_RSTREAM,
    BT_WSTREAM,
    BT_PTR,
    BT_PARSEC,
};

enum LANG_LITERAL {
    LIT_STR32,
    LIT_STR8,
    LIT_CHAR32,
    LIT_CHAR8,
    LIT_INT,
    LIT_FLOAT,
    LIT_HEX,
    LIT_OCT,
    LIT_BIN,
};

class lunar_ir_expr {
public:
    lunar_ir_expr() { }
    virtual ~lunar_ir_expr() { }
};

class lunar_ir_type {
public:
    lunar_ir_type(LANG_BASIC_TYPE type, LANG_OWNERSHIP owner_ship) : m_type(type), m_owner_ship(owner_ship) { }
    virtual ~lunar_ir_type() { }

protected:
    LANG_BASIC_TYPE m_type;
    LANG_OWNERSHIP  m_owner_ship;
};

class lunar_ir_scalar : public lunar_ir_type {
public:
    lunar_ir_scalar(LANG_OWNERSHIP owner_ship, LANG_SCALAR scalar)
        : lunar_ir_type(BT_SCALAR, owner_ship), m_scalar(scalar) { }

private:
    LANG_SCALAR m_scalar;
};

class lunar_ir_vector : public lunar_ir_type {
public:
    lunar_ir_vector(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type, uint64_t size)
        : lunar_ir_type(BT_VECTOR, owner_ship),
          m_type(std::move(type)),
          m_size(size) { }
    
    virtual ~lunar_ir_vector() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    uint64_t m_size;
};

class lunar_ir_list : public lunar_ir_type {
public:
    lunar_ir_list(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_LIST, owner_ship),
          m_type(std::move(type)) { }
    
    virtual ~lunar_ir_list() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_struct : public lunar_ir_type {
public:
    lunar_ir_struct(LANG_OWNERSHIP owner_ship, const std::string &name)
        : lunar_ir_type(BT_STRUCT, owner_ship), m_name(name) { }
    
    virtual ~lunar_ir_struct() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, const std::string &name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(name);
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_member_types;
    std::vector<std::string> m_member_names;
    std::string m_name;
};

class lunar_ir_union : public lunar_ir_type {
public:
    lunar_ir_union(LANG_OWNERSHIP owner_ship, const std::string &name)
        : lunar_ir_type(BT_UNION, owner_ship), m_name(name) { }
    
    virtual ~lunar_ir_union() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, const std::string &name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(name);
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_member_types;
    std::vector<std::string> m_member_names;
    std::string m_name;
};

class lunar_ir_dict : public lunar_ir_type {
public:
    lunar_ir_dict(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> key, std::unique_ptr<lunar_ir_type> val)
        : lunar_ir_type(BT_DICT, owner_ship), m_key(std::move(key)), m_val(std::move(val)) { }

private:
    std::unique_ptr<lunar_ir_type> m_key;
    std::unique_ptr<lunar_ir_type> m_val;
};

class lunar_ir_set : public lunar_ir_type {
public:
    lunar_ir_set(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> val)
        : lunar_ir_type(BT_SET, owner_ship), m_val(std::move(val)) { }

private:
    std::unique_ptr<lunar_ir_type> m_val;
};

class lunar_ir_data : public lunar_ir_type {
public:
    lunar_ir_data(LANG_OWNERSHIP owner_ship, const std::string &name)
        : lunar_ir_type(BT_DATA, owner_ship), m_name(name) { }
    
    virtual ~lunar_ir_data() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, const std::string &name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(name);
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_member_types;
    std::vector<std::string> m_member_names;
    std::string m_name;
};

class lunar_ir_functype : public lunar_ir_type {
public:
    lunar_ir_functype(LANG_OWNERSHIP owner_ship) : lunar_ir_type(BT_FUNCTYPE, owner_ship) { }
    virtual ~lunar_ir_functype() { }
    
    void add_ret(std::unique_ptr<lunar_ir_type> ret)
    {
        m_ret.push_back(std::move(ret));
    }

    void add_arg(std::unique_ptr<lunar_ir_type> arg)
    {
        m_arg.push_back(std::move(arg));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_ret;
    std::vector<std::unique_ptr<lunar_ir_type>> m_arg;
};

class lunar_ir_rstream : public lunar_ir_type {
public:
    lunar_ir_rstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_RSTREAM, OWN_UNIQUE), m_type(std::move(type)) { }

    virtual ~lunar_ir_rstream() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_wstream : public lunar_ir_type {
public:
    lunar_ir_wstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_WSTREAM, OWN_SHARED), m_type(std::move(type)) { }

    virtual ~lunar_ir_wstream() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_string : public lunar_ir_type {
public:
    lunar_ir_string(LANG_OWNERSHIP owner_ship) : lunar_ir_type(BT_STRING, owner_ship) { }
    virtual ~lunar_ir_string() { }
};

class lunar_ir_binary : public lunar_ir_type {
public:
    lunar_ir_binary(LANG_OWNERSHIP owner_ship) : lunar_ir_type(BT_BINARY, owner_ship) { }
    virtual ~lunar_ir_binary() { }
};

class lunar_ir_ptr : public lunar_ir_type {
public:
    lunar_ir_ptr(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_PTR, owner_ship),
          m_type(std::move(type)) { }
    virtual ~lunar_ir_ptr() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_parsec : public lunar_ir_type {
public:
    lunar_ir_parsec(bool is_binary) // binary or string
        : lunar_ir_type(BT_PARSEC, OWN_UNIQUE), m_is_binary(is_binary) { }
    virtual ~lunar_ir_parsec() { }

private:
    bool m_is_binary;
};

class lunar_ir_func : public lunar_ir_expr {
public:
    lunar_ir_func(const std::string &name) : m_name(name) { }
    virtual ~lunar_ir_func() { }
    
    void add_ret(std::unique_ptr<lunar_ir_type> ret)
    {
        m_ret.push_back(std::move(ret));
    }

    void add_arg(std::unique_ptr<lunar_ir_type> arg, const std::string &name)
    {
        m_arg.push_back(std::move(arg));
        m_argname.push_back(name);
    }
    
    void add_expr(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_exprs.push_back(std::move(expr));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_ret;
    std::vector<std::unique_ptr<lunar_ir_type>> m_arg;
    std::vector<std::string> m_argname;
    std::string m_name;
    std::vector<std::unique_ptr<lunar_ir_expr>> m_exprs;
};

}

#endif // LUNAR_IR_HPP
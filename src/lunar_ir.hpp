#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"
#include "lunar_parsec.hpp"

#include <vector>
#include <unordered_map>
#include <unordered_set>

/*
 * -----------------------------------------------------------------------------
 *
 * IR           := TOP*
 * TOP          := FUNC | GLOBAL | IMPORT | EXPR | STATEMENT
 * TOPSTATEMENT := LET | COND | WHILE | SELECT | SCHEDULE | STRUCT | CUNION | UNION
 * STATEMENT    := LET | COND | WHILE | BREAK | SELECT | RETURN | SCHEDULE | STRUCT | CUNION | UNION
 * GLOBAL       := ( global ( ( TYPE (IDENTIFIER+) EXPRIDENTLIT )+ ) )
 * IMPORT       := ( import STR32+ )
 *
 * -----------------------------------------------------------------------------
 *
 * TYPE  := TYPE0 | ( OWNERSHIP TYPE0 )
 * TYPE0 := SCALAR | ARRAY | STRING | BINARY | LIST | STRUCT | DICT | SET | UNION | FUNCTYPE |
 *          RSTREAM | WSTREAM | RFILESTREAM | WFILESTREAM | RSOCKSTREAM | WSOCKSTREAM |
 *          RSIGSTREAM | RTHREADSTREAM | WTHREADSTREAM | PTR | CUNION | PARSEC | IDENTIFIER
 *
 * OWNERSHIP := unique | shared | ref
 *
 * SCALAR     := SCALARTYPE INITSCALAR | SCALARTYPE
 * SCALARTYPE := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | char | atom
 *
 * ARRAY := ( array TYPE SIZE ) | ( array TYPE )
 * SIZE  := INT | HEX | OCT | BIN
 *
 * STRING := string
 *
 * BINARY := binary
 *
 * LIST := ( list TYPE )
 *
 * STRUCT := ( struct IDENTIFIER? ( TYPE IDENTIFIER )* )
 *
 * UNION := ( union IDENTIFIER? ( TYPE IDENTIFIER )* )
 *
 * CUNION := ( cunion IDENTIFIER? ( TYPE IDENTIFIER )* )
 *
 * DICT := ( dict TYPE TYPE )
 *
 * SET := ( set TYPE )
 *
 * FUNCTYPE := ( func ( TYPE* ) ( TYPE* ) )
 *
 * RSTREAM := ( rstrm TYPE )
 * WSTREAM := ( wstrm TYPE )
 * RFILESTREAM := rfilestrm
 * WFILESTREAM := wfilestrm
 * RSOCKSTREAM := rsockstrm
 * WSOCKSTREAM := wsockstrm
 * RSIGSTREAM  := rsigstrm
 * RTHREADTREAM := ( rthreadstrm TYPE )
 * WTHREADTREAM := ( wthreadstrm TYPE )
 *
 * PTR := (ptr TYPE )
 *
 * PARSEC := ( parsec string ) | ( parsec binary)
 *
 * -----------------------------------------------------------------------------
 *
 * FUNC := ( defun IDENTIFIER ( TYPE* ) ( TYPE IDENTIFIER )* STEXPR* )
 *
 * -----------------------------------------------------------------------------
 *
 * STEXPR := STATMENT | EXPR
 *
 * LET := ( let ( (TYPE IDENTIFIER)+ EXPRIDENTLIT )+ STEXPR\* )
 *
 * COND := ( cond ( EXPRIDENTLIT STEXPR* )+ ( else STEXPR* )? )
 *
 * WHILE := ( while EXPRIDENTLIT STEXPR* )
 *
 * SELECT := ( select ( EXPRIDENT STEXPR*)* ( timeout EXPRIDENTLIT STEXPR* )? )
 *
 * BREAK := ( break )
 *
 * RETURN := ( return EXPRIDENTLIT* )
 *
 * SCHEDULE := ( schedule )
 *
 * -----------------------------------------------------------------------------
 *
 * EXPR := SPAWN | THREAD | COPY | ASSOC | INCCNT | DECCNT | IF | LAMBDA | NEW | CALLFUNC |
 *         TYPEOF | MKSTREAM | MKFILESTREAM | MKSOCKSTREAM | PUSH | POP | SPIN_LOCK_INIT |
 *         SPIN_LOCK | SPIN_TRY_LOCK | SPIN_UNLOCK | PARSE | CCALL | DLOPEN | DLCLOSE |
 *         TOPTR | DEREF | ADD | MINUS | MULTI | DIV | MOD | PRINT | TOSTR | BAND | BOR |
 *         BXOR | BNOT | BSL | BSR | BASL | BASR | BPOPCNT | BLZCNT | AND | OR | EQ | NOT
 *         SOCKET | OPEN | MKSIGNALSTREAM
 *
 * EXPRIDENT := EXPR | IDENTIFIER
 *
 * SPAWN := ( spawn EXPRIDENTLIT EXPRIDENT EXPRIDENTLIT )
 *
 * THREAD := ( thread EXPRIDENTLIT TYPE EXPRIDENTLIT EXPRIDENT EXPRIDENTLIT )
 *
 * COPY := ( copy! EXPRIDENT EXPRIDENTLIT )
 *
 * ASSOC := ( assoc! EXPRIDENT EXPRIDENT )
 *
 * INCCNT := ( inccnt EXPRIDENT )
 * DECCNT := ( deccnt EXPRIDENT )
 *
 * IF := ( if EXPRIDENTLIT EXPRIDENTLIT EXPRIDENTLIT )
 *
 * LAMBDA := ( lambda ( TYPE* ) ( TYPE IDENTIFIER )* STEXPR* )
 *
 * NEW := ( new TYPE EXPRIDENTLIT* )
 *
 * CALLFUNC := ( EXPRIDENT EXPRIDENTLIT* )
 *
 * TYPEOF := ( type TYPE0 EXPRIDENTLIT )
 *
 * MKSTREAM       := ( mkstream TYPE EXPRIDENTLIT )
 * MKFILESTREAM   := ( mkfilestream EXPRIDENTLIT )
 * MKSOCKSTREAM   := ( mksockstream EXPRIDENTLIT )
 * MKSIGNALSTREAM := ( mksognalstream EXPRIDENTLIT )
 *
 * PUSH := ( push! EXPRIDENTLIT )
 *
 * POP := ( pop! EXPRIDENTLIT )
 *
 * SPIN_LOCK_INIT := ( spin_lock_init EXPRIDENT )
 * SPIN_LOCK      := ( spin_lock EXPRIDENT )
 * SPIN_TRY_LOCK  := ( spin_try_lock EXPRIDENT )
 * SPIN_UNLOCK    := ( spin_unlock EXPRIDENT )
 *
 * PARSE        := ( parse EXPRIDENT PARSECOPS EXPRIDENTLIT* )
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
 * DLOPEN  := ( dlopen EXPRIDENTLIT )
 * DLCLOSE := ( dlclose EXPRIDENT )
 * DLSYM   := ( dlsym EXPRIDENT EXPRIDENTLIT )
 * CCALL   := ( ccall EXPRIDENT EXPRIDENTLIT* )
 *
 * TOPTR := ( toptr EXPRIDENT )
 * DEREF := ( deref EXPRIDENT )
 *
 * ADD   := ( + EXPRIDENTLIT EXPRIDENTLIT+ )
 * MINUS := ( - EXPRIDENTLIT EXPRIDENTLIT+ )
 * MULTI := ( * EXPRIDENTLIT EXPRIDENTLIT+ )
 * DIV   := ( / EXPRIDENTLIT EXPRIDENTLIT+ )
 * MOD   := ( mod EXPRIDENTLIT EXPRIDENTLIT+ )
 *
 * BAND := ( band EXPRIDENTLIT EXPRIDENTLIT+ )
 * BOR  := ( bor EXPRIDENTLIT EXPRIDENTLIT+ )
 * BXOR := ( bxor EXPRIDENTLIT EXPRIDENTLIT+ )
 * BNOT := ( bnot EXPRIDENTLIT )
 * BSL  := ( bsl EXPRIDENT EXPRIDENT )  // logical left shift
 * BSR  := ( bsr EXPRIDENT EXPRIDENT )  // logical right shift
 * BASL := ( basl EXPRIDENT EXPRIDENT ) // arithmetic left shift
 * BASR := ( basr EXPRIDENT EXPRIDENT ) // arithmetic right shift
 * BPOPCNT := ( bpopcnt EXPRIDENT )
 * BLZCNT  := ( blzcnt EXPRIDENT )
 *
 * AND := ( and EXPRIDENTLIT EXPRIDENTLIT+ )
 * OR  := ( or EXPRIDENTLIT EXPRIDENTLIT+ )
 * EQ  := ( = EXPRIDENTLIT EXPRIDENTLIT+ )
 * NOT := ( not EXPRIDENTLIT )
 *
 * OPEN   := ( open EXPRIDENTLIT (OFLAGS*) )
 * SOCKET := ( socket SOCKDOMAIN SOCKTYPE )
 *
 * PRINT := ( print EXPRIDENTLIT+ )
 *
 * TOSTR := ( tostr EXPRIDENTLIT )
 *
 * -----------------------------------------------------------------------------
 *
 * LITERAL := STR32 | STR8 | CHAR32 | CHAR8 | INT | FLOAT | HEX | OCT | BIN | ATOM
 *
 * ATOM := `IDENTIFIER
 *
 * STR32  := " CHARS* "
 * STR8   := b " CHARS* "
 * ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
 * CHARS  := ESCAPE | ESCAPE以外の文字
 *
 * CHAR32 := ' CHARS '
 * CHAR8  := b ' CHARS '
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
    BT_ARRAY,
    BT_STRING,
    BT_BINARY,
    BT_LIST,
    BT_STRUCT,
    BT_DICT,
    BT_SET,
    BT_UNION,
    BT_CUNION,
    BT_FUNCTYPE,
    BT_RSTREAM,
    BT_WSTREAM,
    BT_RSOCKSTREAM,
    BT_WSOCKSTREAM,
    BT_RFILESTREAM,
    BT_WFILESTREAM,
    BT_RTHREADSTREAM,
    BT_WTHREADSTREAM,
    BT_RSIGSTREAM,
    BT_PTR,
    BT_PARSEC,
    BT_ID,
};

enum IR_TOP {
    IR_FUNC,
    IR_GLOBAL,
    IR_IMPORT,
    IR_EXPR,
    IR_STATEMENT,
};

class lunar_ir_base {
public:
    lunar_ir_base() : m_line(0), m_col(0) { }
    virtual ~lunar_ir_base() { }

    void set_col(uint64_t col) { m_col = col; }
    void set_line(uint64_t line) { m_line = line; }
    uint64_t get_col() { return m_col; }
    uint64_t get_line() { return m_line; }

    virtual void print(const std::string &from) { }

private:
    uint64_t m_line, m_col;
};

class lunar_ir_identifier : public lunar_ir_base
{
public:
    lunar_ir_identifier(std::unique_ptr<std::u32string> id) : m_id(std::move(id)) { }
    virtual ~lunar_ir_identifier() { }

    const std::u32string& get_id() { return *m_id; }

    virtual void print(const std::string &from)
    {
        printf("%s -> \"%llu:%llu: identifier: %s\";\n", from.c_str(), get_line(), get_col(), to_string(*m_id).c_str());
    }

private:
    std::unique_ptr<std::u32string> m_id;
};

class lunar_ir_type : public lunar_ir_base {
public:
    lunar_ir_type(LANG_BASIC_TYPE type, LANG_OWNERSHIP owner_ship) : m_type(type), m_owner_ship(owner_ship) { }
    virtual ~lunar_ir_type() { }

    void print_ownership(const std::string &from) {
        switch (m_owner_ship) {
        case OWN_IMMOVABLE:
            printf("%s -> \"%llu:%llu: ownership: immovable\";\n", from.c_str(), get_line(), get_col());
            break;
        case OWN_SHARED:
            printf("%s -> \"%llu:%llu: ownership: shared\";\n", from.c_str(), get_line(), get_col());
            break;
        case OWN_UNIQUE:
            printf("%s -> \"%llu:%llu: ownership: unique\";\n", from.c_str(), get_line(), get_col());
            break;
        case OWN_REF:
            printf("%s -> \"%llu:%llu: ownership: ref\";\n", from.c_str(), get_line(), get_col());
            break;
        }
    }

protected:
    LANG_BASIC_TYPE m_type;
    LANG_OWNERSHIP  m_owner_ship;
};

class lunar_ir_top : public lunar_ir_base {
public:
    lunar_ir_top(IR_TOP type) : m_type(type) { }
    virtual ~lunar_ir_top() { }

private:
    IR_TOP m_type;
};

class lunar_ir_expr : public lunar_ir_top {
public:
    lunar_ir_expr() : lunar_ir_top(IR_EXPR) { }
    virtual ~lunar_ir_expr() { }
};

class lunar_ir_statement : public lunar_ir_top {
public:
    lunar_ir_statement() : lunar_ir_top(IR_STATEMENT) { }
    virtual ~lunar_ir_statement() { }
};

class lunar_ir_member {
public:
    lunar_ir_member() { }
    virtual ~lunar_ir_member() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, std::unique_ptr<lunar_ir_identifier> name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(std::move(name));
    }

    void print_member(const std::string &from) {
        if (m_member_types.empty())
            return;

        std::ostringstream os;
        os << "\"" << m_line << ":" << m_col << ": member\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());

        for (uint64_t i = 0; i < m_member_types.size(); i++) {
            std::ostringstream os2;
            os2 << "\"" << m_line << ":" << m_col << ": member" << "[" << i << "]\"";
            printf("%s -> %s;\n", os.str().c_str(), os2.str().c_str());
            m_member_types[i]->print(os2.str());
            m_member_names[i]->print(os2.str());
        }
    }

    void set_line_mem(uint64_t line) { m_line = line; }
    void set_col_mem(uint64_t col) { m_col = col; }

private:
    std::vector<std::unique_ptr<lunar_ir_type>>       m_member_types;
    std::vector<std::unique_ptr<lunar_ir_identifier>> m_member_names;
    uint64_t m_line;
    uint64_t m_col;
};

class lunar_ir_def_struct : public lunar_ir_statement, public lunar_ir_member {
public:
    lunar_ir_def_struct(std::unique_ptr<lunar_ir_identifier> name) : m_name(std::move(name)) { }
    virtual ~lunar_ir_def_struct() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": def_struct\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
};

class lunar_ir_def_cunion : public lunar_ir_statement, public lunar_ir_member {
public:
    lunar_ir_def_cunion(std::unique_ptr<lunar_ir_identifier> name) : m_name(std::move(name)) { }
    virtual ~lunar_ir_def_cunion() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": def_union\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
};

class lunar_ir_def_union : public lunar_ir_statement, public lunar_ir_member {
public:
    lunar_ir_def_union(std::unique_ptr<lunar_ir_identifier> name) : m_name(std::move(name)) { }
    virtual ~lunar_ir_def_union() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": def_cunion\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
};

class lunar_ir_import : public lunar_ir_top {
public:
    lunar_ir_import() : lunar_ir_top(IR_IMPORT) { }
    virtual ~lunar_ir_import() { }

    void add_module(std::u32string module)
    {
        m_modules.push_back(module);
    }

private:
    std::vector<std::u32string> m_modules;
};

class lunar_ir_stexpr : public lunar_ir_base {
public:
    lunar_ir_stexpr(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)), m_is_expr(true) { }
    lunar_ir_stexpr(std::unique_ptr<lunar_ir_statement> statement) : m_statement(std::move(statement)), m_is_expr(false) { }
    virtual ~lunar_ir_stexpr() { }

    bool is_expr() { return m_is_expr; }

private:
    std::unique_ptr<lunar_ir_expr>      m_expr;
    std::unique_ptr<lunar_ir_statement> m_statement;
    bool m_is_expr;
};

class lunar_ir_type_id : public lunar_ir_type {
public:
    lunar_ir_type_id(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_identifier> id)
        : lunar_ir_type(BT_ID, owner_ship), m_id(std::move(id)) { }
    virtual ~lunar_ir_type_id() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": type_id\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_id->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_id;
};

class lunar_ir_lit_atom : public lunar_ir_expr {
public:
    lunar_ir_lit_atom(std::u32string str) : m_str(str) { }
    virtual ~lunar_ir_lit_atom() { }

private:
    std::u32string m_str;
};

class lunar_ir_lit_str32 : public lunar_ir_expr {
public:
    lunar_ir_lit_str32(std::u32string str) : m_str(str) { }
    virtual ~lunar_ir_lit_str32() { }

private:
    std::u32string m_str;
};

class lunar_ir_lit_str8 : public lunar_ir_expr {
public:
    lunar_ir_lit_str8(std::u32string str) : m_str(str) { }
    virtual ~lunar_ir_lit_str8() { }

private:
    std::u32string m_str;
};

class lunar_ir_lit_char32 : public lunar_ir_expr {
public:
    lunar_ir_lit_char32(char32_t c) : m_char(c) { }
    virtual ~lunar_ir_lit_char32() { }

private:
    char32_t m_char;
};

class lunar_ir_lit_char8 : public lunar_ir_expr {
public:
    lunar_ir_lit_char8(char c) : m_char(c) { }
    virtual ~lunar_ir_lit_char8() { }

private:
    char m_char;
};

class lunar_ir_lit_int : public lunar_ir_expr {
public:
    lunar_ir_lit_int(int64_t num, const std::u32string &str) : m_num(num), m_str(str) { }
    virtual ~lunar_ir_lit_int() { }

private:
    int64_t m_num;
    std::u32string m_str;
};

class lunar_ir_lit_uint : public lunar_ir_expr {
public:
    lunar_ir_lit_uint(uint64_t num, const std::u32string &str) : m_num(num), m_str(str) { }
    virtual ~lunar_ir_lit_uint() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": uint literal: " << m_num;
        printf("%s -> %s\";\n", from.c_str(), os.str().c_str());
    }

private:
    uint64_t m_num;
    std::u32string m_str;
};

class lunar_ir_lit_float : public lunar_ir_expr {
public:
    lunar_ir_lit_float(double num) : m_num(num) { }
    virtual ~lunar_ir_lit_float() { }

private:
    double m_num;
};

class lunar_ir_scalar : public lunar_ir_type {
public:
    lunar_ir_scalar(LANG_OWNERSHIP owner_ship, LANG_SCALAR scalar)
        : lunar_ir_type(BT_SCALAR, owner_ship), m_scalar(scalar) { }

    LANG_SCALAR get_type() { return m_scalar; }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col();

        switch (m_scalar) {
        case SC_BOOL:
            os << ": scalar: bool\""; break;
        case SC_U64:
            os << ": scalar: u64\""; break;
        case SC_S64:
            os << ": scalar: s64\""; break;
        case SC_U32:
            os << ": scalar: u32\""; break;
        case SC_S32:
            os << ": scalar: s32\""; break;
        case SC_U16:
            os << ": scalar: u16\""; break;
        case SC_S16:
            os << ": scalar: s16\""; break;
        case SC_U8:
            os << ": scalar: u8\""; break;
        case SC_S8:
            os << ": scalar: s8\""; break;
        case SC_DOUBLE:
            os << ": scalar: double\""; break;
        case SC_FLOAT:
            os << ": scalar: float\""; break;
        case SC_CHAR:
            os << ": scalar: char\""; break;
        case SC_ATOM:
            os << ": scalar: atom\""; break;
        }

        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }

private:
    LANG_SCALAR m_scalar;
};

class lunar_ir_array : public lunar_ir_type {
public:
    lunar_ir_array(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type, std::unique_ptr<lunar_ir_lit_uint> size)
        : lunar_ir_type(BT_ARRAY, owner_ship),
          m_type(std::move(type)),
          m_size(std::move(size)) { }
    virtual ~lunar_ir_array() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        std::ostringstream os_type;

        os << "\"" << get_line() << ":" << get_col() << ": array\"";
        os_type << "\"" << get_line() << ":" << get_col() << ": array type\"";

        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        printf("%s -> %s;\n", os.str().c_str(), os_type.str().c_str());

        print_ownership(os.str());
        m_type->print(os_type.str());

        if (m_size) {
            std::ostringstream os_size;
            os_size << "\"" << get_line() << ":" << get_col() << ": array size\"";
            printf("%s -> %s;\n", os.str().c_str(), os_size.str().c_str());
            m_size->print(os_size.str());
        }
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    std::unique_ptr<lunar_ir_lit_uint> m_size;
};

class lunar_ir_list : public lunar_ir_type {
public:
    lunar_ir_list(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_LIST, owner_ship),
          m_type(std::move(type)) { }

    virtual ~lunar_ir_list() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": list\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_struct : public lunar_ir_type, public lunar_ir_member {
public:
    lunar_ir_struct(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_identifier> name)
        : lunar_ir_type(BT_STRUCT, owner_ship), m_name(std::move(name)) { }
    virtual ~lunar_ir_struct() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": struct\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
};

class lunar_ir_cunion : public lunar_ir_type, public lunar_ir_member {
public:
    lunar_ir_cunion(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_identifier> name)
        : lunar_ir_type(BT_CUNION, owner_ship), m_name(std::move(name)) { }
    virtual ~lunar_ir_cunion() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": cunion\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
};

class lunar_ir_dict : public lunar_ir_type {
public:
    lunar_ir_dict(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> key, std::unique_ptr<lunar_ir_type> val)
        : lunar_ir_type(BT_DICT, owner_ship), m_key(std::move(key)), m_val(std::move(val)) { }
    virtual ~lunar_ir_dict() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        std::ostringstream os_key;
        std::ostringstream os_val;

        os << "\"" << get_line() << ":" << get_col() << ": dict\"";
        os_key << "\"" << get_line() << ":" << get_col() << ": dict key\"";
        os_val << "\"" << get_line() << ":" << get_col() << ": dict val\"";

        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        printf("%s -> %s;\n", os.str().c_str(), os_key.str().c_str());
        printf("%s -> %s;\n", os.str().c_str(), os_val.str().c_str());

        print_ownership(os.str());
        m_key->print(os_key.str());
        m_val->print(os_val.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_key;
    std::unique_ptr<lunar_ir_type> m_val;
};

class lunar_ir_set : public lunar_ir_type {
public:
    lunar_ir_set(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_SET, owner_ship), m_type(std::move(type)) { }
    virtual ~lunar_ir_set() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": list\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_union : public lunar_ir_type, public lunar_ir_member {
public:
    lunar_ir_union(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_identifier> name)
        : lunar_ir_type(BT_UNION, owner_ship), m_name(std::move(name)) { }
    virtual ~lunar_ir_union() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": union\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_name->print(os.str());
        print_member(os.str());
    }

private:
    std::unique_ptr<lunar_ir_identifier> m_name;
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

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": functype\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());

        int i = 0;
        for (auto &ret: m_ret) {
            std::ostringstream os_ret;
            os_ret << "\"" << get_line() << ":" << get_col() << ": retval[" << i << "]\"";
            printf("%s -> %s;\n", os.str().c_str(), os_ret.str().c_str());
            ret->print(os_ret.str());
            i++;
        }

        i = 0;
        for (auto &arg: m_arg) {
            std::ostringstream os_arg;
            os_arg << "\"" << get_line() << ":" << get_col() << ": arg[" << i << "]\"";
            printf("%s -> %s;\n", os.str().c_str(), os_arg.str().c_str());
            arg->print(os_arg.str());
            i++;
        }
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

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": rstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_wstream : public lunar_ir_type {
public:
    lunar_ir_wstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_WSTREAM, OWN_SHARED), m_type(std::move(type)) { }
    virtual ~lunar_ir_wstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": wstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_rsigstream : public lunar_ir_type {
public:
    lunar_ir_rsigstream() : lunar_ir_type(BT_RSIGSTREAM, OWN_UNIQUE) { }
    virtual ~lunar_ir_rsigstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": rsigstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_rsockstream : public lunar_ir_type {
public:
    lunar_ir_rsockstream() : lunar_ir_type(BT_RSOCKSTREAM, OWN_UNIQUE) { }
    virtual ~lunar_ir_rsockstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": rsockstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_wsockstream : public lunar_ir_type {
public:
    lunar_ir_wsockstream() : lunar_ir_type(BT_WSOCKSTREAM, OWN_SHARED) { }
    virtual ~lunar_ir_wsockstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": wsockstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_rfilestream : public lunar_ir_type {
public:
    lunar_ir_rfilestream() : lunar_ir_type(BT_RFILESTREAM, OWN_UNIQUE) { }
    virtual ~lunar_ir_rfilestream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": rfilesteam\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_wfilestream : public lunar_ir_type {
public:
    lunar_ir_wfilestream() : lunar_ir_type(BT_WFILESTREAM, OWN_SHARED) { }
    virtual ~lunar_ir_wfilestream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": wfilestream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_rthreadstream : public lunar_ir_type {
public:
    lunar_ir_rthreadstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_RTHREADSTREAM, OWN_UNIQUE), m_type(std::move(type)) { }
    virtual ~lunar_ir_rthreadstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": rthreadstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_wthreadstream : public lunar_ir_type {
public:
    lunar_ir_wthreadstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_WTHREADSTREAM, OWN_SHARED), m_type(std::move(type)) { }
    virtual ~lunar_ir_wthreadstream() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": wthreadstream\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
        m_type->print(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_string : public lunar_ir_type {
public:
    lunar_ir_string(LANG_OWNERSHIP owner_ship) : lunar_ir_type(BT_STRING, owner_ship) { }
    virtual ~lunar_ir_string() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": string\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_binary : public lunar_ir_type {
public:
    lunar_ir_binary(LANG_OWNERSHIP owner_ship) : lunar_ir_type(BT_BINARY, owner_ship) { }
    virtual ~lunar_ir_binary() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": binary\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }
};

class lunar_ir_ptr : public lunar_ir_type {
public:
    lunar_ir_ptr(LANG_OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_PTR, owner_ship),
          m_type(std::move(type)) { }
    virtual ~lunar_ir_ptr() { }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": ptr\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_parsec : public lunar_ir_type {
public:
    lunar_ir_parsec(bool is_binary) // binary or string
        : lunar_ir_type(BT_PARSEC, OWN_UNIQUE), m_is_binary(is_binary) { }
    virtual ~lunar_ir_parsec() { }

    bool is_binary() { return m_is_binary; }

    virtual void print(const std::string &from)
    {
        std::ostringstream os;
        os << "\"" << get_line() << ":" << get_col() << ": parsec\"";
        printf("%s -> %s;\n", from.c_str(), os.str().c_str());
        print_ownership(os.str());
    }

private:
    bool m_is_binary;
};

class lunar_ir_var : public lunar_ir_base {
public:
    lunar_ir_var(std::unique_ptr<lunar_ir_type> type, const std::u32string &name)
        : m_type(std::move(type)), m_name(name) { }

    std::u32string& get_name() { return m_name; }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    std::u32string m_name;
};

class lunar_ir_func : public lunar_ir_top {
public:
    lunar_ir_func(const std::u32string &name) : lunar_ir_top(IR_FUNC), m_name(name) { }
    virtual ~lunar_ir_func() { }

    void add_ret(std::unique_ptr<lunar_ir_type> ret)
    {
        m_ret.push_back(std::move(ret));
    }

    void add_arg(std::unique_ptr<lunar_ir_var> var)
    {
        m_argmap[var->get_name()] = var.get();
        m_args.push_back(std::move(var));
    }

    void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_stexprs.push_back(std::move(stexpr));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_ret;
    std::vector<std::unique_ptr<lunar_ir_var>>  m_args;
    std::unordered_map<std::u32string, lunar_ir_var*> m_argmap;
    std::u32string m_name;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
};

class lunar_ir_let : public lunar_ir_statement {
public:
    class def {
    public:
        void add_var(std::unique_ptr<lunar_ir_var> var)
        {
            m_argmap[var->get_name()] = var.get();
            m_vars.push_back(std::move(var));
        }

        void set_expr(std::unique_ptr<lunar_ir_expr> expr)
        {
            m_expr = std::move(expr);
        }

    private:
        std::vector<std::unique_ptr<lunar_ir_var>>     m_vars;
        std::unordered_map<std::u32string, lunar_ir_var*> m_argmap;
        std::unique_ptr<lunar_ir_expr> m_expr;
    };

    lunar_ir_let() { }
    virtual ~lunar_ir_let() { }

    void add_defs(std::unique_ptr<def> def)
    {
        m_defs.push_back(std::move(def));
    }

    void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_stexprs.push_back(std::move(stexpr));
    }

private:
    std::vector<std::unique_ptr<def>> m_defs;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
};

class lunar_ir_cond : public lunar_ir_statement {
public:
    class cond {
    public:
        void set_cond(std::unique_ptr<lunar_ir_expr> expr)
        {
            m_expr = std::move(expr);
        }

        void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
        {
            m_stexprs.push_back(std::move(stexpr));
        }

    private:
        std::unique_ptr<lunar_ir_expr> m_expr; // condition
        std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
    };

    lunar_ir_cond() { }
    virtual ~lunar_ir_cond() { }

    void add_cond(std::unique_ptr<cond> c)
    {
        m_conds.push_back(std::move(c));
    }

    void add_else(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_elses.push_back(std::move(stexpr));
    }

private:
    std::vector<std::unique_ptr<cond>> m_conds;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_elses;
};

class lunar_ir_while : public lunar_ir_statement {
public:
    lunar_ir_while() { }
    virtual ~lunar_ir_while() { }

    void set_cond(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_cond = std::move(expr);
    }

    void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_stexprs.push_back(std::move(stexpr));
    }

private:
    std::unique_ptr<lunar_ir_expr> m_cond;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
};

class lunar_ir_select : public lunar_ir_statement {
public:
    class cond {
    public:
        void set_cond(std::unique_ptr<lunar_ir_expr> expr)
        {
            m_expr = std::move(expr);
        }

        void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
        {
            m_stexprs.push_back(std::move(stexpr));
        }

    private:
        std::unique_ptr<lunar_ir_expr> m_expr; // condition
        std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
    };

    lunar_ir_select() { }
    virtual ~lunar_ir_select() { }

    void set_timeout(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_timeout = std::move(expr);
    }

    void add_timeout(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_elses.push_back(std::move(stexpr));
    }

private:
    std::vector<std::unique_ptr<cond>> m_conds;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_elses;
    std::unique_ptr<lunar_ir_expr> m_timeout; // no timeout when -1
};

class lunar_ir_break : public lunar_ir_statement {
public:
    lunar_ir_break() { }
    virtual ~lunar_ir_break() { }
};

class lunar_ir_return : public lunar_ir_statement {
public:
    lunar_ir_return() { }
    virtual ~lunar_ir_return() { }

    void add_expr(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_exprs.push_back(std::move(expr));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_expr>> m_exprs;
};

class lunar_ir_schedule : public lunar_ir_statement {
public:
    lunar_ir_schedule() { }
    virtual ~lunar_ir_schedule() { }
};

class lunar_ir_spawn : public lunar_ir_expr {
public:
    lunar_ir_spawn(std::unique_ptr<lunar_ir_expr> ssize,
                   std::unique_ptr<lunar_ir_expr> func,
                   std::unique_ptr<lunar_ir_expr> arg)
        : m_ssize(std::move(ssize)),
          m_func(std::move(func)),
          m_arg(std::move(arg)) { }
    virtual ~lunar_ir_spawn() { }

private:
    std::unique_ptr<lunar_ir_expr> m_ssize;  // stack size
    std::unique_ptr<lunar_ir_expr> m_func;
    std::unique_ptr<lunar_ir_expr> m_arg;
};

class lunar_ir_thread : public lunar_ir_expr {
public:
    lunar_ir_thread(std::unique_ptr<lunar_ir_expr> name,
                    std::unique_ptr<lunar_ir_type> type,
                    std::unique_ptr<lunar_ir_expr> qsize,
                    std::unique_ptr<lunar_ir_expr> func,
                    std::unique_ptr<lunar_ir_expr> arg)
        : m_name(std::move(name)),
          m_type(std::move(type)),
          m_qsize(std::move(qsize)),
          m_func(std::move(func)),
          m_arg(std::move(arg)) { }
    virtual ~lunar_ir_thread() { }

private:
    std::unique_ptr<lunar_ir_expr> m_name;  // name (atom)
    std::unique_ptr<lunar_ir_type> m_type;
    std::unique_ptr<lunar_ir_expr> m_qsize; // size of thread queue
    std::unique_ptr<lunar_ir_expr> m_func;
    std::unique_ptr<lunar_ir_expr> m_arg;
};

class lunar_ir_copy : public lunar_ir_expr {
public:
    lunar_ir_copy(std::unique_ptr<lunar_ir_expr> dst, std::unique_ptr<lunar_ir_expr> src)
        : m_dst(std::move(dst)),
          m_src(std::move(src)) { }
    virtual ~lunar_ir_copy() { }

private:
    std::unique_ptr<lunar_ir_expr> m_dst;
    std::unique_ptr<lunar_ir_expr> m_src;
};

class lunar_ir_assoc : public lunar_ir_expr {
public:
    lunar_ir_assoc(std::unique_ptr<lunar_ir_expr> dst, std::unique_ptr<lunar_ir_expr> src)
        : m_dst(std::move(dst)),
          m_src(std::move(src)) { }
    virtual ~lunar_ir_assoc() { }

private:
    std::unique_ptr<lunar_ir_expr> m_dst;
    std::unique_ptr<lunar_ir_expr> m_src;
};

class lunar_ir_inccnt : public lunar_ir_expr {
public:
    lunar_ir_inccnt(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_inccnt() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_deccnt : public lunar_ir_expr {
public:
    lunar_ir_deccnt(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_deccnt() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_if : public lunar_ir_expr {
public:
    lunar_ir_if(std::unique_ptr<lunar_ir_expr> cond,
                std::unique_ptr<lunar_ir_expr> expr1,
                std::unique_ptr<lunar_ir_expr> expr2)
        : m_cond(std::move(cond)),
          m_expr1(std::move(expr1)),
          m_expr2(std::move(expr2)) { }
    virtual ~lunar_ir_if() { }

private:
    std::unique_ptr<lunar_ir_expr> m_cond;
    std::unique_ptr<lunar_ir_expr> m_expr1;
    std::unique_ptr<lunar_ir_expr> m_expr2;
};

class lunar_ir_lambda : public lunar_ir_expr {
public:
    lunar_ir_lambda() { }
    virtual ~lunar_ir_lambda() { }

    void add_ret(std::unique_ptr<lunar_ir_type> ret)
    {
        m_ret.push_back(std::move(ret));
    }

    void add_arg(std::unique_ptr<lunar_ir_var> var)
    {
        m_argmap[var->get_name()] = var.get();
        m_args.push_back(std::move(var));
    }

    void add_stexpr(std::unique_ptr<lunar_ir_stexpr> stexpr)
    {
        m_stexprs.push_back(std::move(stexpr));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_ret;
    std::vector<std::unique_ptr<lunar_ir_var>>  m_args;
    std::unordered_map<std::u32string, lunar_ir_var*> m_argmap;
    std::vector<std::unique_ptr<lunar_ir_stexpr>> m_stexprs;
};

class lunar_ir_new : public lunar_ir_expr {
public:
    lunar_ir_new(std::unique_ptr<lunar_ir_type> type) : m_type(std::move(type)) { }
    virtual ~lunar_ir_new() { }

    void add_arg(std::unique_ptr<lunar_ir_expr> arg)
    {
        m_args.push_back(std::move(arg));
    }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    std::vector<std::unique_ptr<lunar_ir_expr>> m_args;
};

class lunar_ir_callfunc : public lunar_ir_expr {
public:
    lunar_ir_callfunc(std::unique_ptr<lunar_ir_expr> func) : m_func(std::move(func)) { }
    virtual ~lunar_ir_callfunc() { }

    void add_arg(std::unique_ptr<lunar_ir_expr> arg)
    {
        m_args.push_back(std::move(arg));
    }

private:
    std::unique_ptr<lunar_ir_expr> m_func;
    std::vector<std::unique_ptr<lunar_ir_expr>> m_args;
};

class lunar_ir_typeof : public lunar_ir_expr {
public:
    lunar_ir_typeof(LANG_BASIC_TYPE type, std::unique_ptr<lunar_ir_expr> size)
        : m_type(type), m_size(std::move(size)) { }
    virtual ~lunar_ir_typeof() { }

private:
    LANG_BASIC_TYPE m_type;
    std::unique_ptr<lunar_ir_expr> m_size;
};

class lunar_ir_mkstream : public lunar_ir_expr {
public:
    lunar_ir_mkstream(std::unique_ptr<lunar_ir_type> type, std::unique_ptr<lunar_ir_expr> size)
        : m_type(std::move(type)), m_size(std::move(size)) { }
    virtual ~lunar_ir_mkstream() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    std::unique_ptr<lunar_ir_expr> m_size;
};

class lunar_ir_mkfilestream : public lunar_ir_expr {
public:
    lunar_ir_mkfilestream(std::unique_ptr<lunar_ir_expr> expr)
        : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_mkfilestream() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_mksockstream : public lunar_ir_expr {
public:
    lunar_ir_mksockstream(std::unique_ptr<lunar_ir_expr> expr)
        : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_mksockstream() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_mksignalstream : public lunar_ir_expr {
public:
    lunar_ir_mksignalstream(std::unique_ptr<lunar_ir_expr> expr)
        : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_mksignalstream() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_push : public lunar_ir_expr
{
public:
    lunar_ir_push(std::unique_ptr<lunar_ir_expr> stream, std::unique_ptr<lunar_ir_expr> data)
        : m_stream(std::move(stream)),
          m_data(std::move(data)) { }
    virtual ~lunar_ir_push() { }

private:
    std::unique_ptr<lunar_ir_expr> m_stream;
    std::unique_ptr<lunar_ir_expr> m_data;
};

class lunar_ir_pop : public lunar_ir_expr
{
public:
    lunar_ir_pop(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_pop() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_spin_lock_init : public lunar_ir_expr
{
public:
    lunar_ir_spin_lock_init(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_spin_lock_init() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_spin_lock : public lunar_ir_expr
{
public:
    lunar_ir_spin_lock(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_spin_lock() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_spin_try_lock : public lunar_ir_expr
{
public:
    lunar_ir_spin_try_lock(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_spin_try_lock() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_spin_unlock : public lunar_ir_expr
{
public:
    lunar_ir_spin_unlock(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_spin_unlock() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_parse : public lunar_ir_expr
{
public:
    enum PARSECOPS {
        PASECCHAR,
        PARSECTRY,
        PARSECTRYEND,
        PARSECLA,
        PARSECLAEND,
        PARSECDIGIT,
        PARSECHEX,
        PARSECOCT,
        PARSECSPACE,
        PARSECSATIS,
        PARSECSTR,
        PARSECRESULT
    };

    lunar_ir_parse(std::unique_ptr<lunar_ir_expr> parsec, PARSECOPS op)
        : m_parsec(std::move(parsec)), m_op(op) { }
    virtual ~lunar_ir_parse() { }

    void add_expr(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_exprs.push_back(std::move(expr));
    }

private:
    std::unique_ptr<lunar_ir_expr> m_parsec;
    PARSECOPS m_op;
    std::vector<std::unique_ptr<lunar_ir_expr>> m_exprs;
};

class lunar_ir_dlopen : public lunar_ir_expr
{
public:
    lunar_ir_dlopen(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_dlopen() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_dlclose : public lunar_ir_expr
{
public:
    lunar_ir_dlclose(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_dlclose() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_dlsym : public lunar_ir_expr {
public:
    lunar_ir_dlsym(std::unique_ptr<lunar_ir_expr> handle, std::unique_ptr<lunar_ir_expr> symbol)
        : m_handle(std::move(handle)), m_symbol(std::move(symbol)) { }
    virtual ~lunar_ir_dlsym() { }

private:
    std::unique_ptr<lunar_ir_expr> m_handle;
    std::unique_ptr<lunar_ir_expr> m_symbol;
};

class lunar_ir_dlccall : public lunar_ir_expr {
public:
    lunar_ir_dlccall(std::unique_ptr<lunar_ir_expr> symbol) : m_symbol(std::move(symbol)) { }
    virtual ~lunar_ir_dlccall() { }

    void add_arg(std::unique_ptr<lunar_ir_expr> arg)
    {
        m_args.push_back(std::move(arg));
    }

private:
    std::unique_ptr<lunar_ir_expr> m_symbol;
    std::vector<std::unique_ptr<lunar_ir_expr>> m_args;
};

class lunar_ir_tostr : public lunar_ir_expr {
public:
    lunar_ir_tostr(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_tostr() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_toptr : public lunar_ir_expr {
public:
    lunar_ir_toptr(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_toptr() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_deref : public lunar_ir_expr {
public:
    lunar_ir_deref(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_deref() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_binops : public lunar_ir_expr {
public:
    lunar_ir_binops() { }
    virtual ~lunar_ir_binops() { }

    void add_expr(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_exprs.push_back(std::move(expr));
    }

protected:
    std::vector<std::unique_ptr<lunar_ir_expr>> m_exprs;
};

class lunar_ir_add : public lunar_ir_binops {
public:
    lunar_ir_add() { }
    virtual ~lunar_ir_add() { }
};

class lunar_ir_minus : public lunar_ir_binops {
public:
    lunar_ir_minus() { }
    virtual ~lunar_ir_minus() { }
};

class lunar_ir_multi : public lunar_ir_binops {
public:
    lunar_ir_multi() { }
    virtual ~lunar_ir_multi() { }
};

class lunar_ir_div : public lunar_ir_binops {
public:
    lunar_ir_div() { }
    virtual ~lunar_ir_div() { }
};

class lunar_ir_mod : public lunar_ir_binops {
public:
    lunar_ir_mod() { }
    virtual ~lunar_ir_mod() { }
};

class lunar_ir_band : public lunar_ir_binops {
public:
    lunar_ir_band() { }
    virtual ~lunar_ir_band() { }
};

class lunar_ir_bor : public lunar_ir_binops {
public:
    lunar_ir_bor() { }
    virtual ~lunar_ir_bor() { }
};

class lunar_ir_bxor : public lunar_ir_binops {
public:
    lunar_ir_bxor() { }
    virtual ~lunar_ir_bxor() { }
};

class lunar_ir_bnot : public lunar_ir_expr {
public:
    lunar_ir_bnot(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_bnot() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_bpopcnt : public lunar_ir_expr {
public:
    lunar_ir_bpopcnt(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_bpopcnt() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_blzcnt : public lunar_ir_expr {
public:
    lunar_ir_blzcnt(std::unique_ptr<lunar_ir_expr> expr) : m_expr(std::move(expr)) { }
    virtual ~lunar_ir_blzcnt() { }

private:
    std::unique_ptr<lunar_ir_expr> m_expr;
};

class lunar_ir_bsl : public lunar_ir_expr {
public:
    lunar_ir_bsl(std::unique_ptr<lunar_ir_expr> lexpr, std::unique_ptr<lunar_ir_expr> rexpr)
        : m_lexpr(std::move(lexpr)), m_rexpr(std::move(rexpr)) { }
    virtual ~lunar_ir_bsl() { }

private:
    std::unique_ptr<lunar_ir_expr> m_lexpr;
    std::unique_ptr<lunar_ir_expr> m_rexpr;
};

class lunar_ir_bsr : public lunar_ir_expr {
public:
    lunar_ir_bsr(std::unique_ptr<lunar_ir_expr> lexpr, std::unique_ptr<lunar_ir_expr> rexpr)
        : m_lexpr(std::move(lexpr)), m_rexpr(std::move(rexpr)) { }
    virtual ~lunar_ir_bsr() { }

private:
    std::unique_ptr<lunar_ir_expr> m_lexpr;
    std::unique_ptr<lunar_ir_expr> m_rexpr;
};

class lunar_ir_basl : public lunar_ir_expr {
public:
    lunar_ir_basl(std::unique_ptr<lunar_ir_expr> lexpr, std::unique_ptr<lunar_ir_expr> rexpr)
        : m_lexpr(std::move(lexpr)), m_rexpr(std::move(rexpr)) { }
    virtual ~lunar_ir_basl() { }

private:
    std::unique_ptr<lunar_ir_expr> m_lexpr;
    std::unique_ptr<lunar_ir_expr> m_rexpr;
};

class lunar_ir_basr : public lunar_ir_expr {
public:
    lunar_ir_basr(std::unique_ptr<lunar_ir_expr> lexpr, std::unique_ptr<lunar_ir_expr> rexpr)
        : m_lexpr(std::move(lexpr)), m_rexpr(std::move(rexpr)) { }
    virtual ~lunar_ir_basr() { }

private:
    std::unique_ptr<lunar_ir_expr> m_lexpr;
    std::unique_ptr<lunar_ir_expr> m_rexpr;
};

class lunar_ir_and : public lunar_ir_binops {
public:
    lunar_ir_and() { }
    virtual ~lunar_ir_and() { }
};

class lunar_ir_or : public lunar_ir_binops {
public:
    lunar_ir_or() { }
    virtual ~lunar_ir_or() { }
};

class lunar_ir_eq : public lunar_ir_binops {
public:
    lunar_ir_eq() { }
    virtual ~lunar_ir_eq() { }
};

class lunar_ir_not : public lunar_ir_binops {
public:
    lunar_ir_not() { }
    virtual ~lunar_ir_not() { }
};

class lunar_ir_print : public lunar_ir_expr {
public:
    lunar_ir_print() { }
    virtual ~lunar_ir_print() { }

    void add_expr(std::unique_ptr<lunar_ir_expr> expr)
    {
        m_exprs.push_back(std::move(expr));
    }

protected:
    std::vector<std::unique_ptr<lunar_ir_expr>> m_exprs;
};

class lunar_ir_open : public lunar_ir_expr {
public:
    enum OFLAGS {
        OFLAG_RDONLY,
        OFLAG_WONLY,
        OFLAG_RDWR,
        OFLAG_APPEND,
        OFLAG_CREAT,
        OFLAG_TRUNC,
        OFLAG_EXCL,
        OFLAG_SHLOCK,
        OFLAG_EXLOCK,
        OFLAG_NOFOLLOW,
        OFLAG_SYMLINK,
        OFLAG_EVTONLY,
        OFLAG_CLOEXEC,
    };

    lunar_ir_open(std::unique_ptr<lunar_ir_expr> file) : m_file(std::move(file)) { }
    virtual ~lunar_ir_open() { }

private:
    std::unique_ptr<lunar_ir_expr> m_file;
    std::vector<OFLAGS> m_flags;
};

class lunar_ir_socket : public lunar_ir_expr {
public:
    enum SOCKDOMAIN {
        DOMAIN_PF_UNIX,
        DOMAIN_PF_INET,
        DOMIAN_PF_INET6,
    };

    enum SOCKTYPE {
        TYPE_SOCK_STREAM,
        TYPE_SOCK_DGRAM,
        TYPE_SOCK_RAW,
    };

    lunar_ir_socket(SOCKDOMAIN domain, SOCKTYPE type) : m_domain(domain), m_type(type) { }
    virtual ~lunar_ir_socket() { }

private:
    SOCKDOMAIN m_domain;
    SOCKTYPE   m_type;
};

class lunar_ir_module {
public:
    lunar_ir_module(std::string file) : m_file(file) { }
    virtual ~lunar_ir_module() { }

    const std::string& get_filename() { return m_file; }

    void add_top_elm(std::unique_ptr<lunar_ir_top> elm)
    {
        m_top_elms.push_back(std::move(elm));
    }

    void print()
    {
        for (auto &elm: m_top_elms) {
            elm->print("\"top\"");
        }
    }

private:
    std::string m_file;
    std::vector<std::unique_ptr<lunar_ir_top>> m_top_elms;
};

class lunar_ir {
public:
    lunar_ir();
    virtual ~lunar_ir();

    void add_file(const std::u32string &buf, std::string file)
    {
        std::lock_guard<std::mutex> lock(m_mutex);

        m_files[file] = buf;
        m_fileq.push_back(file);
    }

    void compile(const std::string &mainfile);
    void run(int idx);

    void print()
    {
        for (auto &m: m_modules) {
            printf("digraph \"%s\" {", m.first.c_str());
            m.second->print();
            printf("}\n\n");
        }
    }

private:
    void parse_module(std::unique_ptr<lunar_ir_module> module, parsec<char32_t> &ps);
    bool parse_top(lunar_ir_module *module, parsec<char32_t> &ps);
    void parse_member(lunar_ir_member *member, lunar_ir_module *module, parsec<char32_t> &ps);
    bool parse_type0_str(lunar_ir_module *module, parsec<char32_t> &ps, const char32_t *str);
    void parse_types(std::function<void(std::unique_ptr<lunar_ir_type>)> add_type,
                     lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_identifier>     parse_identifier(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_type>           parse_type(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_type>           parse_type0(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own, int ownline, int owncol);
    std::unique_ptr<lunar_ir_array>         parse_array(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_expr>           parse_expr(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_set>            parse_set(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_list>           parse_list(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_rstream>        parse_rstrm(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_wstream>        parse_wstrm(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_rthreadstream>  parse_rthreadstrm(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_wthreadstream>  parse_wthreadstrm(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_parsec>         parse_parsec(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_ptr>            parse_ptr(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_dict>           parse_dict(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_functype>       parse_functype(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_uint(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_int>        parse_lit_int(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_hex(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_oct(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_bin(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_size(lunar_ir_module *module, parsec<char32_t> &ps);
    LANG_OWNERSHIP                           parse_ownership(lunar_ir_module *module, parsec<char32_t> &ps);
    template <typename T> std::unique_ptr<T> parse_def_member(lunar_ir_module *module, parsec<char32_t> &ps);
    template <typename T> std::unique_ptr<T> parse_def_member_own(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);

    const std::string& get_line(const std::string &file, uint64_t num);

    std::unordered_map<std::string, std::unique_ptr<lunar_ir_module>> m_modules;
    std::unordered_map<std::string, std::u32string> m_files;
    std::unordered_map<std::string, std::unique_ptr<std::vector<std::string>>> m_lines;
    std::deque<std::string> m_fileq;
    std::mutex m_mutex;

    friend void run_parse(void *ir);
};

}

#endif // LUNAR_IR_HPP
#include "lunar_ir.hpp"
#include "MCJITHelper.hpp"

#include <functional>

#include <stdlib.h>
#include <limits.h>

namespace lunar {

std::unordered_set<char32_t> idh_set;
std::unordered_set<char32_t> idt_set;
std::unordered_map<std::u32string, LANG_SCALAR> scalar_set;
std::unordered_map<char32_t, char32_t> esc_set;
std::unordered_map<char32_t, char32_t> hex_set;

lunar_ir::lunar_ir()
{
    idh_set.insert(U'0');
    idh_set.insert(U'1');
    idh_set.insert(U'2');
    idh_set.insert(U'3');
    idh_set.insert(U'4');
    idh_set.insert(U'5');
    idh_set.insert(U'6');
    idh_set.insert(U'7');
    idh_set.insert(U'8');
    idh_set.insert(U'9');
    idh_set.insert(U'!');
    idh_set.insert(U'~');
    idh_set.insert(U'#');
    idh_set.insert(U'$');
    idh_set.insert(U'%');
    idh_set.insert(U'^');
    idh_set.insert(U'&');
    idh_set.insert(U'*');
    idh_set.insert(U'(');
    idh_set.insert(U')');
    idh_set.insert(U'-');
    idh_set.insert(U'=');
    idh_set.insert(U'{');
    idh_set.insert(U'}');
    idh_set.insert(U'[');
    idh_set.insert(U']');
    idh_set.insert(U'|');
    idh_set.insert(U'\\');
    idh_set.insert(U':');
    idh_set.insert(U';');
    idh_set.insert(U'"');
    idh_set.insert(U'\'');
    idh_set.insert(U'<');
    idh_set.insert(U'>');
    idh_set.insert(U',');
    idh_set.insert(U'.');
    idh_set.insert(U'?');
    idh_set.insert(U'/');

    idt_set.insert(U'!');
    idt_set.insert(U'~');
    idt_set.insert(U'#');
    idt_set.insert(U'$');
    idt_set.insert(U'%');
    idt_set.insert(U'^');
    idt_set.insert(U'&');
    idt_set.insert(U'*');
    idt_set.insert(U'(');
    idt_set.insert(U')');
    idt_set.insert(U'-');
    idt_set.insert(U'=');
    idt_set.insert(U'{');
    idt_set.insert(U'}');
    idt_set.insert(U'[');
    idt_set.insert(U']');
    idt_set.insert(U'|');
    idt_set.insert(U'\\');
    idt_set.insert(U':');
    idt_set.insert(U';');
    idt_set.insert(U'"');
    idt_set.insert(U'\'');
    idt_set.insert(U'<');
    idt_set.insert(U'>');
    idt_set.insert(U',');
    idt_set.insert(U'.');
    idt_set.insert(U'?');
    idt_set.insert(U'/');
    idt_set.insert(U'\u0009');
    idt_set.insert(U'\u000A');
    idt_set.insert(U'\u000B');
    idt_set.insert(U'\u000C');
    idt_set.insert(U'\u000D');
    idt_set.insert(U'\u001C');
    idt_set.insert(U'\u001D');
    idt_set.insert(U'\u001E');
    idt_set.insert(U'\u001F');
    idt_set.insert(U'\u0020');
    idt_set.insert(U'\u00A0');
    idt_set.insert(U'\u11A3');
    idt_set.insert(U'\u11A4');
    idt_set.insert(U'\u11A5');
    idt_set.insert(U'\u11A6');
    idt_set.insert(U'\u11A7');
    idt_set.insert(U'\u1689');
    idt_set.insert(U'\u2000');
    idt_set.insert(U'\u2001');
    idt_set.insert(U'\u2002');
    idt_set.insert(U'\u2003');
    idt_set.insert(U'\u2004');
    idt_set.insert(U'\u2005');
    idt_set.insert(U'\u2006');
    idt_set.insert(U'\u2007');
    idt_set.insert(U'\u2008');
    idt_set.insert(U'\u2009');
    idt_set.insert(U'\u200A');
    idt_set.insert(U'\u200B');
    idt_set.insert(U'\u202F');
    idt_set.insert(U'\u205F');
    idt_set.insert(U'\u2060');
    idt_set.insert(U'\u3000');
    idt_set.insert(U'\u3164');
    idt_set.insert(U'\uFEFF');

    scalar_set[U"bool"]   = SC_BOOL;
    scalar_set[U"u64"]    = SC_U64;
    scalar_set[U"s64"]    = SC_S64;
    scalar_set[U"u32"]    = SC_U32;
    scalar_set[U"s32"]    = SC_S32;
    scalar_set[U"u16"]    = SC_U16;
    scalar_set[U"s16"]    = SC_S16;
    scalar_set[U"u8"]     = SC_U8;
    scalar_set[U"s8"]     = SC_S8;
    scalar_set[U"double"] = SC_DOUBLE;
    scalar_set[U"float"]  = SC_FLOAT;
    scalar_set[U"char"]   = SC_CHAR;

    esc_set[U'a']  = U'\a';
    esc_set[U'b']  = U'\b';
    esc_set[U'f']  = U'\f';
    esc_set[U'r']  = U'\r';
    esc_set[U'n']  = U'\n';
    esc_set[U't']  = U'\t';
    esc_set[U'v']  = U'\v';
    esc_set[U'\\'] = U'\\';
    esc_set[U'?']  = U'\?';
    esc_set[U'\''] = U'\'';
    esc_set[U'\"'] = U'"';
    esc_set[U'\0'] = U'\0';
    esc_set[U'U']  = U'U';
    esc_set[U'u']  = U'u';

    hex_set[U'0']  =  0;
    hex_set[U'1']  =  1;
    hex_set[U'2']  =  2;
    hex_set[U'3']  =  3;
    hex_set[U'4']  =  4;
    hex_set[U'5']  =  5;
    hex_set[U'6']  =  6;
    hex_set[U'7']  =  7;
    hex_set[U'8']  =  8;
    hex_set[U'9']  =  9;
    hex_set[U'a']  = 10;
    hex_set[U'A']  = 10;
    hex_set[U'b']  = 11;
    hex_set[U'B']  = 11;
    hex_set[U'c']  = 12;
    hex_set[U'C']  = 12;
    hex_set[U'd']  = 13;
    hex_set[U'D']  = 13;
    hex_set[U'e']  = 14;
    hex_set[U'E']  = 14;
    hex_set[U'f']  = 15;
    hex_set[U'F']  = 15;
}

lunar_ir::~lunar_ir()
{
    idh_set.clear();
    idt_set.clear();
    scalar_set.clear();
}

void
lunar_ir::compile(const std::string &mainfile)
{
    int num = std::thread::hardware_concurrency();
    std::thread **th = new std::thread*[num];

    for (int i = 0; i < num; i++) {
        th[i] = new std::thread(std::bind(&lunar_ir::run, this, i));
    }

    for (int i = 0; i < num; i++) {
        th[i]->join();
        delete th[i];
    }
    delete[] th;
}

#define print_parse_err(str, module, ps)                                \
do {                                                                    \
    auto msg = (ps).get_errmsg();                                       \
    std::string spaces;                                                 \
    for (int i = 1; i < msg.col; i++)                                   \
        spaces += ' ';                                                  \
    fprintf(stderr, "%s:%d\n%s:%d:%d: error: %s\n%s\n%s^\n",            \
            __FILE__, __LINE__, module->get_filename().c_str(),         \
            msg.line, msg.col,                                          \
            (str), get_line(module->get_filename(), msg.line).c_str(),  \
            spaces.c_str());                                            \
} while (0)

#define print_parse_err_linecol(str, module, ps, line, col)     \
do {                                                            \
    std::string spaces;                                         \
    for (int i = 1; i < col; i++)                               \
        spaces += ' ';                                          \
    fprintf(stderr, "%s:%d\n%s:%d:%d: error: %s\n%s\n%s^\n",    \
            __FILE__, __LINE__, module->get_filename().c_str(), \
            (line), (col), (str),                               \
            get_line(module->get_filename(), line).c_str(),     \
            spaces.c_str());                                    \
} while (0)

#define print_parse_warn_linecol(str, module, ps, line, col)    \
do {                                                            \
    std::string spaces;                                         \
    for (int i = 1; i < col; i++)                               \
        spaces += ' ';                                          \
    fprintf(stderr, "%s:%d\n%s:%d:%d: warning: %s\n%s\n%s^\n",  \
            __FILE__, __LINE__, module->get_filename().c_str(), \
            (line), (col), (str),                               \
            get_line(module->get_filename(), line).c_str(),     \
            spaces.c_str());                                    \
} while (0)

const std::string&
lunar_ir::get_line(const std::string &file, uint64_t num)
{
    std::vector<std::string> *lines;
    auto it = m_lines.find(file);
    if (it == m_lines.end()) {
        auto vec = llvm::make_unique<std::vector<std::string>>();
        lines = vec.get();
        m_lines[file] = std::move(vec);

        // split with '\n'
        std::stringstream ss(to_string(m_files[file]));
        std::string item;
        while (getline(ss, item, '\n')) {
            lines->push_back(item);
        }
    } else {
        lines = it->second.get();
    }

    return (*lines)[num - 1];
}

bool
head_identifier(char32_t c)
{
    if (idh_set.find(c) == idh_set.end())
        return true;

    return false;
}

bool
tail_identifier(char32_t c)
{
    if (idt_set.find(c) == idt_set.end())
        return true;

    return false;
}

LANG_OWNERSHIP
lunar_ir::parse_ownership(lunar_ir_module *module, parsec<char32_t> &ps)
{
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"shared");
    }
    if (ps.is_success())
        return OWN_SHARED;

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"unique");
    }
    if (ps.is_success())
        return OWN_UNIQUE;

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"ref");
    }
    if (ps.is_success())
        return OWN_REF;

    return OWN_IMMOVABLE;
}

bool
lunar_ir::parse_type0_str(lunar_ir_module *module, parsec<char32_t> &ps, const char32_t *str)
{
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(str);
    }
    if (ps.is_success()) {
        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (ps.is_success()) {
            return true;
        } else if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return false;
        }
    }

    return false;
}

std::unique_ptr<lunar_ir_literal>
lunar_ir::parse_literal(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });

    // parse char32
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'\'');
    }

    if (ps.is_success())
        return parse_lit_char32(module, ps);

    // parse char8
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"b'");
    }

    if (ps.is_success())
        return parse_lit_char8(module, ps);

    // parse str32
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'"');
    }

    if (ps.is_success())
        return parse_lit_str32(module, ps);

    // parse str8
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"b\"");
    }

    if (ps.is_success())
        return parse_lit_str8(module, ps);

    // parse atom
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'`');
    }

    if (ps.is_success()) {
        auto id = parse_identifier(module, ps);
        if (ps.is_success())
            return llvm::make_unique<lunar_ir_lit_atom>(id->get_id());
        else
            return nullptr;
    }

    // parse hex
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0x");
    }

    if (ps.is_success())
        return parse_lit_hex(module, ps);

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0x");
    }

    if (ps.is_success())
        return parse_lit_hex(module, ps);


    // parse bin
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0b");
    }

    if (ps.is_success())
        return parse_lit_bin(module, ps);

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0B");
    }

    if (ps.is_success())
        return parse_lit_bin(module, ps);

    // parse oct
    bool is_oct;
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'0');
        if (ps.is_success()) {
            ps.parse_oct_digit();
            if (ps.is_success()) {
                is_oct = true;
            } else
                is_oct = false;
        } else {
            is_oct = false;
        }
    }

    if (is_oct)
        return parse_lit_oct(module, ps);

    // parse float or int
    bool is_num;
    bool is_minus;

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'-');
    }

    if (ps.is_success()) {
        is_num   = true;
        is_minus = true;
    } else {
        is_minus = false;

        {
            parsec<char32_t>::parser_look_ahead plahead(ps);
            ps.parse_digit();
        }

        if (ps.is_success())
            is_num = true;
        else {
            return nullptr;
        }
    }

    // parse float
    {
        parsec<char32_t>::parser_try ptry(ps);
        auto ret = parse_lit_float(module, ps);
        if (ps.is_success())
            return std::move(ret);
    }

    // parse int
    if (is_minus)
        return parse_lit_int(module, ps);

    // parse uint
    return parse_lit_uint(module, ps);
}

std::unique_ptr<lunar_ir_expridlit>
lunar_ir::parse_expridlit(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'(');
    }

    // parse expr
    if (ps.is_success()) {
        auto expr = parse_expr(module, ps);
        if (ps.is_success()) {
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
            if (ps.is_success()) {
                expr->set_line(line);
                expr->set_col(col);
                return llvm::make_unique<lunar_ir_expridlit>(lunar_ir_expridlit::EXPRIDLIT_EXPR, std::move(expr));
            } else {
                print_parse_err("expected \")\"", module, ps);
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }

    // parse literal
    bool is_lit = false;
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_digit();
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'-');
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'`');
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'\'');
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"b'");
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'"');
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

    if (! is_lit) {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"b\"");
        if (ps.is_success()) {
            is_lit = true;
            goto LITERAL;
        }
    }

LITERAL:
    if (is_lit) {
        auto lit = parse_literal(module, ps);
        if (ps.is_success()) {
            lit->set_line(line);
            lit->set_col(col);
            return llvm::make_unique<lunar_ir_expridlit>(lunar_ir_expridlit::EXPRIDLIT_LITERAL, std::move(lit));
        } else {
            return nullptr;
        }
    }

    // parse id
    auto id = parse_identifier(module, ps);

    if (ps.is_success()) {
        id->set_line(line);
        id->set_col(col);
        return llvm::make_unique<lunar_ir_expridlit>(lunar_ir_expridlit::EXPRIDLIT_ID, std::move(id));
    }

    return nullptr;
}

std::unique_ptr<lunar_ir_exprid>
lunar_ir::parse_exprid(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    // parse expr
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'(');
    }

    if (ps.is_success()) {
        auto expr = parse_expr(module, ps);
        if (ps.is_success()) {
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
            if (ps.is_success()) {
                expr->set_line(line);
                expr->set_col(col);
                return llvm::make_unique<lunar_ir_exprid>(lunar_ir_exprid::EXPRID_EXPR, std::move(expr));
            } else {
                print_parse_err("expected \")\"", module, ps);
                return nullptr;
            }
        } else {
            return nullptr;
        }
    }

    // parse id
    auto id = parse_identifier(module, ps);

    if (ps.is_success()) {
        id->set_line(line);
        id->set_col(col);
        return llvm::make_unique<lunar_ir_exprid>(lunar_ir_exprid::EXPRID_ID, std::move(id));
    }

    return nullptr;
}

std::unique_ptr<lunar_ir_expr>
lunar_ir::parse_expr(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });

    auto exprid = parse_exprid(module, ps);
    if (! ps.is_success())
        return nullptr;

    auto expr = llvm::make_unique<lunar_ir_expr>(std::move(exprid));

    // parse arguments
    for (;;) {
        {
            parsec<char32_t>::parser_look_ahead plahead(ps);
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
        }

        if (ps.is_success())
            return expr;

        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return nullptr;
        }

        int line, col;
        line = ps.get_line();
        col  = ps.get_col();

        auto expridlit = parse_expridlit(module, ps);
        if (ps.is_success()) {
            expridlit->set_line(line);
            expridlit->set_col(col);
            expr->add_arg(std::move(expridlit));
        } else {
            return nullptr;
        }
    }

    return nullptr;
}

std::unique_ptr<lunar_ir_array>
lunar_ir::parse_array(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
    }

    if (ps.is_success())
        return llvm::make_unique<lunar_ir_array>(own, std::move(type), nullptr);

    ps.parse_many1_char([&]() { return ps.parse_space(); });
    if (! ps.is_success()) {
        print_parse_err("need white space", module, ps);
        return nullptr;
    }

    auto size = parse_size(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_array>(own, std::move(type), std::move(size));
}

std::unique_ptr<lunar_ir_set>
lunar_ir::parse_set(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_set>(own, std::move(type));
}

std::unique_ptr<lunar_ir_list>
lunar_ir::parse_list(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_list>(own, std::move(type));
}

std::unique_ptr<lunar_ir_dict>
lunar_ir::parse_dict(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto key = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    ps.parse_many1_char([&]() { return ps.parse_space(); });
    if (! ps.is_success()) {
        print_parse_err("need white space", module, ps);
        return nullptr;
    }

    auto val = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_dict>(own, std::move(key), std::move(val));
}

std::unique_ptr<lunar_ir_rstream>
lunar_ir::parse_rstrm(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_rstream>(std::move(type));
}

std::unique_ptr<lunar_ir_wstream>
lunar_ir::parse_wstrm(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_wstream>(std::move(type));
}

std::unique_ptr<lunar_ir_rthreadstream>
lunar_ir::parse_rthreadstrm(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_rthreadstream>(std::move(type));
}

std::unique_ptr<lunar_ir_wthreadstream>
lunar_ir::parse_wthreadstrm(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_wthreadstream>(std::move(type));
}

std::unique_ptr<lunar_ir_parsec>
lunar_ir::parse_parsec(lunar_ir_module *module, parsec<char32_t> &ps)
{
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"string");
    }

    if (ps.is_success())
        return llvm::make_unique<lunar_ir_parsec>(true);

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"binary");
    }

    if (ps.is_success())
        return llvm::make_unique<lunar_ir_parsec>(true);

    print_parse_err("expected \"string\" or \"binary\"", module, ps);

    return nullptr;
}

std::unique_ptr<lunar_ir_ptr>
lunar_ir::parse_ptr(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto type = parse_type(module, ps);
    if (! ps.is_success())
        return nullptr;

    return llvm::make_unique<lunar_ir_ptr>(own, std::move(type));
}

void
lunar_ir::parse_types(std::function<void(std::unique_ptr<lunar_ir_type>)> add_type,
                      lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    ps.character(U'(');
    if (! ps.is_success()) {
        print_parse_err("expected \"(\"", module, ps);
        return;
    }

    ps.parse_many_char([&]() { return ps.parse_space(); });
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U')');
    }

    if (! ps.is_success()) {
        for (;;) {
            auto type = parse_type(module, ps);
            add_type(std::move(type));

            {
                parsec<char32_t>::parser_look_ahead plahead(ps);
                ps.parse_many_char([&]() { return ps.parse_space(); });
                ps.character(U')');
                if (ps.is_success())
                    break;
            }

            ps.parse_many1_char([&]() { return ps.parse_space(); });
            if (! ps.is_success()) {
                print_parse_err("need white space", module, ps);
                return;
            }
        }

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return;
        }
    }
}

std::unique_ptr<lunar_ir_func>
lunar_ir::parse_func(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto func = llvm::make_unique<lunar_ir_func>(own);

    parse_types([&](std::unique_ptr<lunar_ir_type> t) { func->add_ret(std::move(t)); }, module, ps, own);
    if (! ps.is_success())
        return nullptr;

    ps.parse_many1_char([&]() { return ps.parse_space(); });
    if (! ps.is_success()) {
        print_parse_err("need white space", module, ps);
        return nullptr;
    }

    parse_types([&](std::unique_ptr<lunar_ir_type> t) { func->add_arg(std::move(t)); }, module, ps, own);
    if (! ps.is_success())
        return nullptr;

    return func;
}

std::unique_ptr<lunar_ir_type>
lunar_ir::parse_type0(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own, int ownline, int owncol)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });

    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'(');
    }

    std::unique_ptr<lunar_ir_type> type;
    if (ps.is_success()) {
        // array, dict, set, list, struct, union, func, rstrm, wstrm, rthreadstrm, wthreadstrm, ptr, cunion, parsec
        ps.parse_many_char([&]() { return ps.parse_space(); });

        if (parse_type0_str(module, ps, U"array")) {
            type = parse_array(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"set")) {
            type = parse_set(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"list")) {
            type = parse_list(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"ptr")) {
            type = parse_ptr(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"dict")) {
            type = parse_dict(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"struct")) {
            type = parse_def_member_own<lunar_ir_struct>(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"union")) {
            type = parse_def_member_own<lunar_ir_union>(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"cunion")) {
            type = parse_def_member_own<lunar_ir_cunion>(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"rstrm")) {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rstrm must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = parse_rstrm(module, ps);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"wstrm")) {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wstrm must be shared", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = parse_wstrm(module, ps);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"rthreadstrm")) {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rthreadstrm must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = parse_rthreadstrm(module, ps);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"wthreadstrm")) {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wthreadstrm must be shared", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = parse_wthreadstrm(module, ps);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"parsec")) {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("parsec must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = parse_parsec(module, ps);
            if (! ps.is_success()) return nullptr;
        } else if (parse_type0_str(module, ps, U"func")) {
            type = parse_func(module, ps, own);
            if (! ps.is_success()) return nullptr;
        } else {
            print_parse_err("invalid type specifier", module, ps);
            return nullptr;
        }

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return nullptr;
        }
    } else {
        // SCALAR, string, binary, rfilestrm, wfilestrm, rsockstrm, wsockstrm, rsigstrm, IDENTIFIER
        auto id = parse_identifier(module, ps);
        if (! ps.is_success())
            return nullptr;

        auto it_sc = scalar_set.find(id->get_id());
        if (it_sc != scalar_set.end()) {
            return llvm::make_unique<lunar_ir_scalar>(own, it_sc->second);
        }

        auto s = id->get_id();
        if (s == U"string") {
            type = llvm::make_unique<lunar_ir_string>(own);
        } else if (s == U"binary") {
            type = llvm::make_unique<lunar_ir_binary>(own);
        } else if (s == U"rfilestrm") {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rfilestrm must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = llvm::make_unique<lunar_ir_rfilestream>();
        } else if (s == U"wfilestrm") {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wfilestrm must be shared", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = llvm::make_unique<lunar_ir_wfilestream>();
        } else if (s == U"rsockstrm") {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rsockstrm must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = llvm::make_unique<lunar_ir_rsockstream>();
        } else if (s == U"wsockstrm") {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wsockstrm must be shared", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = llvm::make_unique<lunar_ir_wsockstream>();
        } else if (s == U"rsigstrm") {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rsigstrm must be unique", module, ps, ownline, owncol);
                ps.set_is_success(false);
                return nullptr;
            }
            type = llvm::make_unique<lunar_ir_rsigstream>();
        } else  if (s == U"array") {
            print_parse_err_linecol("array needs a type specifier", module, ps, ownline, owncol);
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"list") {
            print_parse_err_linecol("list needs a type specifier", module, ps, ownline, owncol);
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"set") {
            print_parse_err_linecol("set needs a type specifier", module, ps, ownline, owncol);
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"dict") {
            print_parse_err_linecol("dict needs 2 type specifiers", module, ps, ownline, owncol);
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"ptr") {
            print_parse_err_linecol("ptr needs a type specifier", module, ps, ownline, owncol);
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"rstrm") {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rstrm must be unique", module, ps, ownline, owncol);
            } else {
                print_parse_err("rstrm needs type specifier", module, ps);
            }
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"wstrm") {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wstrm must be shared", module, ps, ownline, owncol);
            } else {
                print_parse_err("wstrm needs a type specifier", module, ps);
            }
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"rthreadstrm") {
            if (own != OWN_UNIQUE) {
                print_parse_err_linecol("rthreadstrm must be unique", module, ps, ownline, owncol);
            } else {
                print_parse_err("rthreadstrm needs type specifier\n", module, ps);
            }
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"wthreadstrm") {
            if (own != OWN_SHARED) {
                print_parse_err_linecol("wthreadstrm must be shared", module, ps, ownline, owncol);
            } else {
                print_parse_err("wthreadstrm needs a type specifier", module, ps);
            }
            ps.set_is_success(false);
            return nullptr;
        } else  if (s == U"parsec") {
            if (own != OWN_UNIQUE) {
                print_parse_err("parsec must be unique", module, ps);
            } else {
                print_parse_err("parsec needs a type specifier", module, ps);
            }
            ps.set_is_success(false);
            return nullptr;
        } else if (s == U"struct" || s == U"union" || s == U"cunion") {
            s += U" needs members";
            print_parse_err(to_string(s).c_str(), module, ps);
            ps.set_is_success(false);
            return nullptr;
        } else {
            type = llvm::make_unique<lunar_ir_type_id>(own, std::move(id));
        }
    }

    type->set_line(line);
    type->set_col(col);

    return type;
}

std::unique_ptr<lunar_ir_type>
lunar_ir::parse_type(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });

    int line, col, ownline, owncol;
    line = ps.get_line();
    col  = ps.get_col();

    std::unique_ptr<lunar_ir_type> type;
    LANG_OWNERSHIP own = OWN_IMMOVABLE;
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'(');
        if (ps.is_success()) {
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ownline = ps.get_line();
            owncol  = ps.get_col();
            own = parse_ownership(module, ps);
        }
    }

    if (ps.is_success()) {
        // TYPE := ( OWNERSHIP TYPE0 )
        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return nullptr;
        }

        type = parse_type0(module, ps, own, ownline, owncol);
        if (! ps.is_success())
            return nullptr;

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return nullptr;
        }
    } else {
        // TYPE := TYPE0
        type = parse_type0(module, ps, OWN_IMMOVABLE, line, col);
        if (! ps.is_success()) {
            return nullptr;
        }
    }

    type->set_line(line);
    type->set_col(col);

    return type;
}

void
lunar_ir::parse_member(lunar_ir_member *member, lunar_ir_module *module, parsec<char32_t> &ps)
{
    for (;;) {
        {
            parsec<char32_t>::parser_try ptry(ps);
            ps.character(U'(');
            if (! ps.is_success()) {
                ps.set_is_success(true);
                return;
            }
        }

        auto type = parse_type(module, ps);
        if (! ps.is_success())
            return;

        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return;
        }

        auto name = parse_identifier(module, ps);
        if (! ps.is_success())
            return;

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return;
        }

        member->add_member(std::move(type), std::move(name));

        {
            parsec<char32_t>::parser_look_ahead plahead(ps);
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
            if (ps.is_success())
                return;
        }

        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return;
        }
    }
}

std::unique_ptr<lunar_ir_identifier>
lunar_ir::parse_identifier(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto id = llvm::make_unique<std::u32string>();

    uint64_t line, col;
    line = ps.get_line();
    col  = ps.get_col();

    *id += ps.satisfy(head_identifier);
    if (! ps.is_success()) {
        print_parse_err("invalid character", module, ps);
        return nullptr;
    }

    *id += ps.parse_many_char([&]() { return ps.satisfy(tail_identifier); });

    auto ret = llvm::make_unique<lunar_ir_identifier>(std::move(id));

    ret->set_line(line);
    ret->set_col(col);

    return ret;
}

template <typename T>
std::unique_ptr<T>
lunar_ir::parse_def_member_own(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own)
{
    auto name = parse_identifier(module, ps);

    if (ps.is_success()) {
        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return nullptr;
        }
    }

    uint64_t line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'(');
        if (! ps.is_success()) {
            print_parse_err("expected \"(\"", module, ps);
            return nullptr;
        }
    }

    auto def = llvm::make_unique<T>(own, std::move(name));
    parse_member(def.get(), module, ps);
    if (! ps.is_success())
        return nullptr;

    def->set_line_mem(line);
    def->set_col_mem(col);

    return def;
}

template <typename T>
std::unique_ptr<T>
lunar_ir::parse_def_member(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });
    auto name = parse_identifier(module, ps);
    if (! ps.is_success())
        return nullptr;

    ps.parse_many1_char([&]() { return ps.parse_space(); });
    if (! ps.is_success()) {
        print_parse_err("need white space", module, ps);
        return nullptr;
    }

    uint64_t line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'(');
        if (! ps.is_success()) {
            print_parse_err("expected \"(\"", module, ps);
            return nullptr;
        }
    }

    auto def = llvm::make_unique<T>(std::move(name));
    parse_member(def.get(), module, ps);
    if (! ps.is_success())
        return nullptr;

    def->set_line_mem(line);
    def->set_col_mem(col);

    return def;
}

std::unique_ptr<lunar_ir_lit_float>
lunar_ir::parse_lit_float(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'-');
    }

    std::u32string str;

    if (ps.is_success())
        str += U'-';

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'0');
    }

    if (ps.is_success()) {
        str += U'0';
    } else {
        auto c = ps.satisfy([&](char32_t c) { return U'1' <= c && c <= U'9'; });
        if (! ps.is_success()) {
            return nullptr;
        }

        str += c;
        str += ps.parse_many_char([&]() { return ps.parse_digit(); });
    }

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'.');
    }

    if (! ps.is_success())
        return nullptr;

    str += U'.';
    str += ps.parse_many_char([&]() { return ps.parse_digit(); });

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.satisfy([&](char32_t c) { return c == U'e' || c == U'E'; });
    }

    if (ps.is_success()) {
        str += U'e';

        {
            parsec<char32_t>::parser_try ptry(ps);
            ps.character(U'-');
        }

        if (ps.is_success()) {
            str += U'-';
        } else {
            {
                parsec<char32_t>::parser_try ptry(ps);
                ps.character(U'+');
            }
        }

        {
            parsec<char32_t>::parser_try ptry(ps);
            ps.character(U'0');
        }

        if (ps.is_success()) {
            str += U'0';
        } else {
            auto c = ps.satisfy([&](char32_t c) { return U'1' <= c && c <= U'9'; });
            if (! ps.is_success()) {
                return nullptr;
            }

            str += c;
            str += ps.parse_many_char([&]() { return ps.parse_digit(); });
        }
    }

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'f');
    }

    bool is_float;
    if (ps.is_success()) {
        is_float = true;
    } else {
        is_float = false;
        ps.set_is_success(true);
    }

    double num;
    if (is_float)
        num = strtof(to_string(str).c_str(), nullptr);
    else
        num = strtod(to_string(str).c_str(), nullptr);

    if (errno == ERANGE)
        print_parse_warn_linecol("floating point overflow or underflow", module, ps, line, col);

    auto ret = llvm::make_unique<lunar_ir_lit_float>(num, is_float);
    ret->set_line(line);
    ret->set_col(col);

    return ret;
}

std::unique_ptr<lunar_ir_lit_uint>
lunar_ir::parse_lit_uint(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.character(U'0');
    }

    if (ps.is_success()) {
        auto literal = llvm::make_unique<lunar_ir_lit_uint>(0, U"0");
        literal->set_line(line);
        literal->set_col(col);
        return literal;
    }

    auto c = ps.satisfy([](char32_t c) { return U'1' <= c && c <= U'9'; });
    if (!ps.is_success()) {
        print_parse_err_linecol("expected \"1..9\"", module, ps, line, col);
        return nullptr;
    }

    auto tail = ps.parse_many_char([&]() { return ps.parse_digit(); });
    auto str = c + tail;

    uint64_t num = strtoull(to_string(str).c_str(), nullptr, 10);
    if (errno == ERANGE) {
        print_parse_warn_linecol("integer overflow", module, ps, line, col);
    } else if (errno == EINVAL) {
        print_parse_err_linecol("could not convert to integer", module, ps, line, col);
        ps.set_is_success(false);
        return nullptr;
    }

    auto literal = llvm::make_unique<lunar_ir_lit_uint>(num, str);
    literal->set_line(line);
    literal->set_col(col);

    return literal;
}

std::unique_ptr<lunar_ir_lit_int>
lunar_ir::parse_lit_int(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    ps.character(U'-');
    if (! ps.is_success()) {
        print_parse_err_linecol("expected \"-\"", module, ps, line, col);
        return nullptr;
    }

    auto c = ps.satisfy([](char32_t c) { return U'1' <= c && c <= U'9'; });
    if (! ps.is_success()) {
        print_parse_err_linecol("expected \"1..9\"", module, ps, line, col);
        return nullptr;
    }

    auto tail = ps.parse_many_char([&]() { return ps.parse_digit(); });
    auto str = U'-' + (c + tail);

    int64_t num = strtoll(to_string(str).c_str(), nullptr, 10);
    if (errno == ERANGE) {
        print_parse_warn_linecol("integer overflow or underflow", module, ps, line, col);
    } else if (errno == EINVAL) {
        print_parse_err_linecol("could not convert to integer", module, ps, line, col);
        ps.set_is_success(false);
        return nullptr;
    }

    auto literal = llvm::make_unique<lunar_ir_lit_int>(num, str);
    literal->set_line(line);
    literal->set_col(col);

    return literal;
}

std::unique_ptr<lunar_ir_lit_uint>
lunar_ir::parse_lit_hex(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    ps.character(U'0');
    if (! ps.is_success()) {
        print_parse_err("expected \"0\"", module, ps);
        return nullptr;
    }

    ps.satisfy([&](char32_t c) { return c == U'x' || c == U'X'; });
    if (! ps.is_success()) {
        print_parse_err("expected \"x\" or \"X\"", module, ps);
        return nullptr;
    }

    auto str = ps.parse_many1_char([&]() { return ps.parse_hex_digit(); });
    if (! ps.is_success()) {
        print_parse_err("expected \"0..f\" or \"0..F\"", module, ps);
        return nullptr;
    }

    uint64_t num = strtoull(to_string(str).c_str(), nullptr, 16);
    if (errno == ERANGE) {
        print_parse_warn_linecol("integer overflow", module, ps, line, col);
    } else if (errno == EINVAL) {
        print_parse_err_linecol("could not convert to integer", module, ps, line, col);
        ps.set_is_success(false);
        return nullptr;
    }

    auto literal = llvm::make_unique<lunar_ir_lit_uint>(num, str);
    literal->set_line(line);
    literal->set_col(col);

    return literal;
}

std::unique_ptr<lunar_ir_lit_uint>
lunar_ir::parse_lit_oct(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    ps.character(U'0');
    if (! ps.is_success()) {
        print_parse_err("expected \"0\"", module, ps);
        return nullptr;
    }

    auto str = ps.parse_many1_char([&]() { return ps.parse_oct_digit(); });
    if (! ps.is_success()) {
        print_parse_err("expected \"0..7\"", module, ps);
        return nullptr;
    }

    uint64_t num = strtoull(to_string(str).c_str(), nullptr, 8);
    if (errno == ERANGE) {
        print_parse_warn_linecol("integer overflow", module, ps, line, col);
    } else if (errno == EINVAL) {
        print_parse_err_linecol("could not convert to integer", module, ps, line, col);
        ps.set_is_success(false);
        return nullptr;
    }

    auto literal = llvm::make_unique<lunar_ir_lit_uint>(num, str);
    literal->set_line(line);
    literal->set_col(col);

    return literal;
}

std::unique_ptr<lunar_ir_lit_uint>
lunar_ir::parse_lit_bin(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    ps.character(U'0');
    if (! ps.is_success()) {
        print_parse_err("expected \"0\"", module, ps);
        return nullptr;
    }

    ps.satisfy([&](char32_t c) { return c == U'b' || c == U'B'; });
    if (! ps.is_success()) {
        print_parse_err("expected \"b\" or \"B\"", module, ps);
        return nullptr;
    }

    auto func = [](char32_t c) { return c == U'0' || c == U'1'; };

    auto str = ps.parse_many1_char([&]() { return ps.satisfy(func); });
    if (! ps.is_success()) {
        print_parse_err("expected \"0\" or \"1\"", module, ps);
        return nullptr;
    }

    uint64_t num = strtoull(to_string(str).c_str(), nullptr, 2);
    if (errno == ERANGE) {
        print_parse_warn_linecol("integer overflow", module, ps, line, col);
    } else if (errno == EINVAL) {
        print_parse_err_linecol("could not convert to integer", module, ps, line, col);
        ps.set_is_success(false);
        return nullptr;
    }

    auto literal = llvm::make_unique<lunar_ir_lit_uint>(num, str);
    literal->set_line(line);
    literal->set_col(col);

    return literal;
}

std::unique_ptr<lunar_ir_lit_char8>
lunar_ir::parse_lit_char8(lunar_ir_module *module, parsec<char32_t> &ps)
{
    int line, col;
    line = ps.get_line();
    col  = ps.get_col();

    char32_t c = parse_lit_char(module, &ps, U'\'');
    if (! ps.is_success()) {
        if (c == U'\'')
            print_parse_err("need a character", module, ps);
        return nullptr;
    }

    if (c > 255)
        print_parse_warn_linecol("character is greater than 255", module, ps, line, col);

    ps.character(U'\'');
    if (! ps.is_success()) {
        print_parse_err("expected \"'\"", module, ps);
        return nullptr;
    }

    return llvm::make_unique<lunar_ir_lit_char8>(c);
}

std::unique_ptr<lunar_ir_lit_char32>
lunar_ir::parse_lit_char32(lunar_ir_module *module, parsec<char32_t> &ps)
{
    char32_t c = parse_lit_char(module, &ps, U'\'');
    if (! ps.is_success()) {
        if (c == U'\'')
            print_parse_err("need a character", module, ps);
        return nullptr;
    }

    ps.character(U'\'');
    if (! ps.is_success()) {
        print_parse_err("expected \"'\"", module, ps);
        return nullptr;
    }

    return llvm::make_unique<lunar_ir_lit_char32>(c);
}

std::unique_ptr<lunar_ir_lit_str8>
lunar_ir::parse_lit_str8(lunar_ir_module *module, parsec<char32_t> &ps)
{
    std::u32string str = ps.parse_many_char(std::bind(&lunar_ir::parse_lit_char, this, module, &ps, U'"'));

    if (! ps.is_success())
        return nullptr;

    ps.character(U'"');
    if (! ps.is_success()) {
        print_parse_err("expected \"\"\"", module, ps);
        return nullptr;
    }

    return llvm::make_unique<lunar_ir_lit_str8>(str);
}

std::unique_ptr<lunar_ir_lit_str32>
lunar_ir::parse_lit_str32(lunar_ir_module *module, parsec<char32_t> &ps)
{
    std::u32string str = ps.parse_many_char(std::bind(&lunar_ir::parse_lit_char, this, module, &ps, U'"'));

    if (! ps.is_success())
        return nullptr;

    ps.character(U'"');
    if (! ps.is_success()) {
        print_parse_err("expected \"\"\"", module, ps);
        return nullptr;
    }

    return llvm::make_unique<lunar_ir_lit_str32>(str);
}

char32_t
lunar_ir::parse_lit_char(lunar_ir_module *module, parsec<char32_t> *ps, char32_t endc)
{
    char32_t val = ps->satisfy([&](char32_t c) { return c != endc; });
    if (! ps->is_success())
        return endc;

    if (val == U'\\') {
        char32_t esc = ps->satisfy([&](char32_t c) { return esc_set.find(c) != esc_set.end(); });
        if (! ps->is_success()) {
            print_parse_err("not escaped character", module, *ps);
            return 0;
        }

        if (esc == U'U') {
            val = 0;
            for (int i = 0; i < 8; i++) {
                val <<= 4;
                char32_t c = ps->parse_hex_digit();
                if (! ps->is_success()) {
                    print_parse_err("not hexadecimal", module, *ps);
                    return 0;
                }
                val |= hex_set[c];
            }
        } else if (esc == U'u') {
            val = 0;
            for (int i = 0; i < 4; i++) {
                val <<= 4;
                char32_t c = ps->parse_hex_digit();
                if (! ps->is_success()) {
                    print_parse_err("not hexadecimal", module, *ps);
                    return 0;
                }
                val |= hex_set[c];
            }
        } else {
            return esc_set[esc];
        }
    }

    return val;
}

std::unique_ptr<lunar_ir_lit_uint>
lunar_ir::parse_size(lunar_ir_module *module, parsec<char32_t> &ps)
{
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0x");
    }

    if (ps.is_success())
        return parse_lit_hex(module, ps);

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0X");
    }

    if (ps.is_success())
        return parse_lit_hex(module, ps);

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0b");
    }

    if (ps.is_success())
        return parse_lit_bin(module, ps);

    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.parse_string(U"0B");
    }

    if (ps.is_success())
        return parse_lit_bin(module, ps);

    bool is_oct;
    {
        parsec<char32_t>::parser_look_ahead plahead(ps);
        ps.character(U'0');
        if (ps.is_success()) {
            ps.parse_oct_digit();
            if (ps.is_success())
                is_oct = true;
            else
                is_oct = false;
        }
    }

    if (is_oct)
        return parse_lit_oct(module, ps);

    return parse_lit_uint(module, ps);
}

std::unique_ptr<lunar_ir_let>
lunar_ir::parse_let(lunar_ir_module *module, parsec<char32_t> &ps)
{
    // ( ( ( ( TYPE IDENTIFIER )+ ) EXPRIDENTLIT )+ ) STEXPR*
    ps.parse_many_char([&]() { return ps.parse_space(); });

    auto let = llvm::make_unique<lunar_ir_let>();
    let->set_line(ps.get_line());
    let->set_col(ps.get_col());

    ps.character(U'(');
    if (! ps.is_success()) {
        print_parse_err("expected \"(\"", module, ps);
        return nullptr;
    }

    // ( ( ( TYPE IDENTIFIER )+ ) EXPRIDENTLIT )+
    for (;;) {
        auto def = llvm::make_unique<lunar_ir_let::def>();

        ps.parse_many_char([&]() { return ps.parse_space(); });

        def->set_line(ps.get_line());
        def->set_col(ps.get_col());

        ps.character(U'(');
        if (! ps.is_success()) {
            print_parse_err("expected \"(\"", module, ps);
            return nullptr;
        }

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U'(');
        if (! ps.is_success()) {
            print_parse_err("expected \"(\"", module, ps);
            return nullptr;
        }

        //( TYPE IDENTIFIER )+
        for (;;) {
            ps.parse_many_char([&]() { return ps.parse_space(); });

            int line, col;
            line = ps.get_line();
            col  = ps.get_col();

            ps.character(U'(');
            if (! ps.is_success()) {
                print_parse_err("expected \"(\"", module, ps);
                return nullptr;
            }

            auto type = parse_type(module, ps);
            if (! ps.is_success())
                return nullptr;

            ps.parse_many1_char([&]() { return ps.parse_space(); });
            if (! ps.is_success()) {
                print_parse_err("need white space", module, ps);
                return nullptr;
            }

            auto id = parse_identifier(module, ps);
            if (! ps.is_success())
                return nullptr;

            auto var = llvm::make_unique<lunar_ir_var>(std::move(type), std::move(id));
            var->set_line(line);
            var->set_col(col);

            def->add_var(std::move(var));

            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
            if (! ps.is_success()) {
                print_parse_err("expected \"(\"", module, ps);
                return nullptr;
            }

            {
                parsec<char32_t>::parser_look_ahead plahead(ps);
                ps.parse_many_char([&]() { return ps.parse_space(); });
                ps.character(U')');
                if (ps.is_success())
                    break;
            }

            ps.parse_many1_char([&]() { return ps.parse_space(); });
            if (! ps.is_success()) {
                print_parse_err("need white space", module, ps);
                return nullptr;
            }
        }

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \"(\"", module, ps);
            return nullptr;
        }

        // EXPRIDENTLIT
        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return nullptr;
        }

        auto expridlit = parse_expridlit(module, ps);
        if (! ps.is_success())
            return nullptr;

        def->set_expridlit(std::move(expridlit));

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return nullptr;
        }

        let->add_def(std::move(def));

        {
            parsec<char32_t>::parser_look_ahead plahead(ps);
            ps.parse_many_char([&]() { return ps.parse_space(); });
            ps.character(U')');
            if (ps.is_success())
                break;
        }

        ps.parse_many1_char([&]() { return ps.parse_space(); });
        if (! ps.is_success()) {
            print_parse_err("need white space", module, ps);
            return nullptr;
        }
    }

    // TODO:
    // STEXPR*

    ps.parse_many_char([&]() { return ps.parse_space(); });
    ps.character(U')');
    if (! ps.is_success()) {
        print_parse_err("expected \"(\"", module, ps);
        return nullptr;
    }

    return let;
}

std::unique_ptr<lunar_ir_top>
lunar_ir::parse_topstatement_expr(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char([&]() { return ps.parse_space(); });

    // parse struct
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"struct ");
    }

    if (ps.is_success()) {
        std::unique_ptr<lunar_ir_top> def = parse_def_member<lunar_ir_def_struct>(module, ps);
        if (ps.is_success())
            return def;
        else
            return nullptr;
    }

    // parse union
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"union ");
    }

    if (ps.is_success()) {
        std::unique_ptr<lunar_ir_top> def = parse_def_member<lunar_ir_def_union>(module, ps);
        if (ps.is_success())
            return def;
        else
            return nullptr;
    }

    // parse cunion
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"cunion ");
    }

    if (ps.is_success()) {
        std::unique_ptr<lunar_ir_top> def = parse_def_member<lunar_ir_def_cunion>(module, ps);
        if (ps.is_success())
            return def;
        else
            return nullptr;
    }

    // parse let
    {
        parsec<char32_t>::parser_try ptry(ps);
        ps.parse_string(U"let ");
    }

    if (ps.is_success()) {
        std::unique_ptr<lunar_ir_top> let = parse_let(module, ps);
        if (ps.is_success())
            return let;
        else
            return nullptr;
    }

    return parse_expr(module, ps);
}

bool
lunar_ir::parse_top(lunar_ir_module *module, parsec<char32_t> &ps)
{
    for (;;) {
        ps.parse_many_char([&]() { return ps.parse_space(); });

        uint64_t line, col;

        line = ps.get_line();
        col  = ps.get_col();

        ps.character(U'(');
        if (! ps.is_success()) {
            if (ps.is_eof()) {
                ps.set_is_success(true);
                return true;
            } else {
                print_parse_err("expected \"(\"", module, ps);
                return false;
            }
        }

        auto def = parse_topstatement_expr(module, ps);
        if (! ps.is_success()) {
            return false;
        }

        def->set_line(line);
        def->set_col(col);

        module->add_top_elm(std::move(def));

        ps.parse_many_char([&]() { return ps.parse_space(); });
        ps.character(U')');
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return false;
        }
    }

    return true;
}

void
lunar_ir::parse_module(std::unique_ptr<lunar_ir_module> module, parsec<char32_t> &ps)
{
    if (parse_top(module.get(), ps)) {
        m_modules[module->get_filename()] = std::move(module);
    }
}

void
run_parse(void *ptr)
{
    lunar_ir *ir = (lunar_ir*)ptr;

    for (;;) {
        std::string file;
        {
            std::lock_guard<std::mutex> lock(ir->m_mutex);
            auto it = ir->m_fileq.begin();
            if (it == ir->m_fileq.end())
                break;

            file = *it;
            ir->m_fileq.pop_front();
        }

        auto str = &ir->m_files[file];
        shared_stream rs;
        shared_stream ws;

        make_ptr_stream(&rs, &ws, 2);

        push_ptr(&ws, str);
        push_eof(&ws);

        parsec<char32_t> ps(&rs);
        ps.set_nodel();
        auto module = llvm::make_unique<lunar_ir_module>(file);

        ir->parse_module(std::move(module), ps);

        deref_ptr_stream(&ws);
        deref_ptr_stream(&rs);
    }
}

void
lunar_ir::run(int idx)
{
    init_green_thread(idx);
    spawn_green_thread(run_parse, this);
    run_green_thread();
}

}
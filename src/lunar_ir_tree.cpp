#include "lunar_ir_tree.hpp"
#include "lunar_string.hpp"

namespace lunar{

void
lunar_ir_identifier::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << from << " -> \"" << get_line() << ":" << get_col() << ": identifier: " << to_string(*m_id) << "\";\n";
    s += os.str();
}

void
lunar_ir_type::print_ownership(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << from << " -> \"" << get_line() << ":" << get_col() << ": ownership: ";
    switch (m_owner_ship) {
    case OWN_IMMOVABLE:
        os << "immovable\";\n";
        break;
    case OWN_SHARED:
        os << "shared\";\n";
        break;
    case OWN_UNIQUE:
        os << "unique\";\n";
        break;
    case OWN_REF:
        os << "ref\";\n";
        break;
    }

    s += os.str();
}

void
lunar_ir_exprid::print(std::string &s, const std::string &from)
{
    switch (m_type) {
    case EXPRID_EXPR:
        m_expr->print(s, from);
        break;
    case EXPRID_ID:
        m_id->print(s, from);
        break;
    }
}

void
lunar_ir_expridlit::print(std::string &s, const std::string &from)
{
    switch (m_type) {
    case EXPRIDLIT_EXPR:
        m_expr->print(s, from);
        break;
    case EXPRIDLIT_ID:
        m_id->print(s, from);
        break;
    case EXPRIDLIT_LITERAL:
        m_literal->print(s, from);
        break;
    }
}

void
lunar_ir_expr::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": function call\"";
    s += from + " -> " + os.str() + ";\n";

    m_func->print(s, os.str());

    int i = 0;
    for (auto &arg: m_args) {
        std::ostringstream os_arg;
        os_arg << "\"" << arg->get_line() << ":" << arg->get_col() << ": arg[" << i << "]\"";
        s += os.str() + " -> " + os_arg.str() + ";\n";
        arg->print(s, os_arg.str());
        i++;
    }
}

void
lunar_ir_member::print_member(std::string &s, const std::string &from)
{
    if (m_member_types.empty())
        return;

    std::ostringstream os;
    os << "\"" << m_line << ":" << m_col << ": member\"";
    s += from + " -> " + os.str() + ";\n";

    for (uint64_t i = 0; i < m_member_types.size(); i++) {
        std::ostringstream os2;
        os2 << "\"" << m_line << ":" << m_col << ": member" << "[" << i << "]\"";
        s += os.str() + " -> " + os2.str() + ";\n";
        m_member_types[i]->print(s, os2.str());
        m_member_names[i]->print(s, os2.str());
    }
}

void
lunar_ir_def_struct::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": def_struct\"";
    s += from + " -> " + os.str() + ";\n";
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_def_cunion::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": def_union\"";
    s += from + " -> " + os.str() + ";\n";
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_def_union::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": def_cunion\"";
    s += from + " -> " + os.str() + ";\n";
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_type_id::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": type_id\"";
    s += from + " -> " + ";\n";
    print_ownership(s, os.str());
    m_id->print(s, os.str());
}

void
lunar_ir_lit_atom::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": atom literal: " << to_string(m_str);
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_str32::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": str32 literal: " << to_string(m_str);
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_str8::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": str8 literal: " << to_string(m_str);
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_char32::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": char32 literal: " << m_char;
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_char8::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": char8 literal: " << m_char;
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_int::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": int literal: " << m_num;
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_uint::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": uint literal: " << m_num;
    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_lit_float::print(std::string &s, const std::string &from)
{
    std::ostringstream os;

    if (m_is_float)
        os << "\"" << get_line() << ":" << get_col() << ": float literal: " << m_num;
    else
        os << "\"" << get_line() << ":" << get_col() << ": double literal: " << m_num;

    s += from + " -> " + os.str() + "\";\n";
}

void
lunar_ir_scalar::print(std::string &s, const std::string &from)
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

    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_array::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    std::ostringstream os_type;

    os << "\"" << get_line() << ":" << get_col() << ": array\"";
    os_type << "\"" << get_line() << ":" << get_col() << ": array type\"";

    s += from + " -> " + os.str() + ";\n";
    s += os.str() + " -> " + os_type.str() + ";\n";

    print_ownership(s, os.str());
    m_type->print(s, os_type.str());

    if (m_size) {
        std::ostringstream os_size;
        os_size << "\"" << get_line() << ":" << get_col() << ": array size\"";
        s += os.str() + " -> " + os_size.str() + ";\n";
        m_size->print(s, os_size.str());
    }
}

void
lunar_ir_list::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": list\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_struct::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": struct\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_cunion::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": cunion\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_dict::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    std::ostringstream os_key;
    std::ostringstream os_val;

    os << "\"" << get_line() << ":" << get_col() << ": dict\"";
    os_key << "\"" << get_line() << ":" << get_col() << ": dict key\"";
    os_val << "\"" << get_line() << ":" << get_col() << ": dict val\"";

    s += from + " -> " + os.str() + ";\n";
    s += os.str() + " -> " + os_key.str() + ";\n";
    s += os.str() + " -> " + os_val.str() + ";\n";

    print_ownership(s, os.str());
    m_key->print(s, os_key.str());
    m_val->print(s, os_val.str());
}

void
lunar_ir_set::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": list\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_union::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": union\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_name->print(s, os.str());
    print_member(s, os.str());
}

void
lunar_ir_func::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": func\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());

    int i = 0;
    for (auto &ret: m_ret) {
        std::ostringstream os_ret;
        os_ret << "\"" << get_line() << ":" << get_col() << ": retval[" << i << "]\"";
        s += os.str() + " -> " + os_ret.str() + ";\n";
        ret->print(s, os_ret.str());
        i++;
    }

    i = 0;
    for (auto &arg: m_arg) {
        std::ostringstream os_arg;
        os_arg << "\"" << get_line() << ":" << get_col() << ": arg[" << i << "]\"";
        s += os.str() + " -> " + os_arg.str() + ";\n";
        arg->print(s, os_arg.str());
        i++;
    }
}

void
lunar_ir_rstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": rstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_wstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": wstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_rsigstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": rsigstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_rsockstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": rsockstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_wsockstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": wsockstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_rfilestream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": rfilesteam\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_wfilestream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": wfilestream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_rthreadstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": rthreadstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_wthreadstream::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": wthreadstream\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
    m_type->print(s, os.str());
}

void
lunar_ir_binary::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": binary\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_string::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": string\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_ptr::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": ptr\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_parsec::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": parsec\"";
    s += from + " -> " + os.str() + ";\n";
    print_ownership(s, os.str());
}

void
lunar_ir_let::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": let\"";
    s += from + " -> " + os.str() + ";\n";

    int i = 0;
    for (auto &def: m_defs) {
        std::ostringstream os_def;
        os_def << "\"" << def->get_line() << ":" << def->get_col() << ": def[" << i << "]\"";
        s += os.str() + " -> " + os_def.str() + ";\n";
        def->print(s, os_def.str());
        i++;
    }

    for (auto &stexpr: m_stexprs)
        stexpr->print(s, os.str());
}

void
lunar_ir_let::def::print(std::string &s, const std::string &from)
{
    int i = 0;
    for (auto &var: m_vars) {
        std::ostringstream os_var;
        os_var << "\"" << var->get_line() << ":" << var->get_col() << ": var[" << i << "]\"";
        s += from + " -> " + os_var.str() + ";\n";
        var->print(s, os_var.str());
        i++;
    }

    m_expridlit->print(s, from);
}

void
lunar_ir_var::print(std::string &s, const std::string &from)
{
    m_type->print_ownership(s, from);
    m_id->print(s, from);
}

void
lunar_ir_cond::print(std::string &s, const std::string &from)
{
    std::ostringstream os;
    os << "\"" << get_line() << ":" << get_col() << ": cond\"";
    s += from + " -> " + os.str() + ";\n";

    int i = 0;
    for (auto &cond: m_conds) {
        std::ostringstream os_cond;
        os_cond << "\"" << cond->get_line() << ":" << cond->get_col() << ": cond[" << i << "]\"";
        s += os.str() + " -> " + os_cond.str() + ";\n";
        cond->print(s, os_cond.str());
        i++;
    }

    if (m_else) {
        std::ostringstream os_else;
        os_else << "\"" << m_else->get_line() << ":" << m_else->get_col() << ": else\"";
        s += os.str() + " -> " + os_else.str() + ";\n";
        m_else->print(s, os_else.str());
    }
}

void
lunar_ir_cond::cond::print(std::string &s, const std::string &from)
{
    std::ostringstream os_cond;
    os_cond << "\"" << get_line() << ":" << get_col() << ": condition\"";
    s += from + " -> " + os_cond.str() + ";\n";

    m_expridlit->print(s, os_cond.str());

    int i = 0;
    for (auto &stexpr: m_stexprs) {
        std::ostringstream os_stexpr;
        os_stexpr << "\"" << stexpr->get_line() << ":" << stexpr->get_col() << ": stexpr[" << i << "]\"";
        s += from + " -> " + os_stexpr.str() + ";\n";
        stexpr->print(s, os_stexpr.str());
        i++;
    }
}

void
lunar_ir_while::print(std::string &s, const std::string &from)
{
    std::ostringstream os_cond;
    os_cond << "\"" << get_line() << ":" << get_col() << ": while\"";
    s += from + " -> " + os_cond.str() + ";\n";

    m_cond->print(s, os_cond.str());

    int i = 0;
    for (auto &stexpr: m_stexprs) {
        std::ostringstream os_stexpr;
        os_stexpr << "\"" << stexpr->get_line() << ":" << stexpr->get_col() << ": stexpr[" << i << "]\"";
        s += from + " -> " + os_stexpr.str() + ";\n";
        stexpr->print(s, os_stexpr.str());
        i++;
    }
}

}
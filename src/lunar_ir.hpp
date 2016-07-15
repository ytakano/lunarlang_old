#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"
#include "lunar_parsec.hpp"
#include "lunar_ir_tree.hpp"

#include <unordered_set>

namespace lunar {

class lunar_ir_module {
public:
    lunar_ir_module(std::string file) : m_file(file) { }
    virtual ~lunar_ir_module() { }

    const std::string& get_filename() { return m_file; }

    void add_top_elm(std::unique_ptr<lunar_ir_top> elm)
    {
        m_top_elms.push_back(std::move(elm));
    }

    void print(std::string &s)
    {
        for (auto &elm: m_top_elms) {
            elm->print(s, "\"top\"");
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
            std::string s;
            s = "digraph \"";
            s += m.first;
            s += "\" {\n";
            m.second->print(s);
            s += "}";
            printf("%s\n\n", s.c_str());
        }
    }

private:
    void parse_module(std::unique_ptr<lunar_ir_module> module, parsec<char32_t> &ps);
    bool parse_top(lunar_ir_module *module, parsec<char32_t> &ps);
    void parse_member(lunar_ir_member *member, lunar_ir_module *module, parsec<char32_t> &ps);
    bool parse_str_space(lunar_ir_module *module, parsec<char32_t> &ps, const char32_t *str);
    void parse_types(std::function<void(std::unique_ptr<lunar_ir_type>)> add_type,
                     lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    char32_t parse_lit_char(lunar_ir_module *module, parsec<char32_t> *ps, char32_t endc);
    std::unique_ptr<lunar_ir_identifier>     parse_identifier(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_type>           parse_type(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_type>           parse_type0(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own, int ownline, int owncol);
    std::unique_ptr<lunar_ir_array>          parse_array(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
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
    std::unique_ptr<lunar_ir_func>           parse_func(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_uint(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_int>        parse_lit_int(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_hex(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_oct(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_lit_bin(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_uint>       parse_size(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_top>            parse_topstatement_expr(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_exprid>         parse_exprid(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_literal>        parse_literal(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_float>      parse_lit_float(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_expridlit>      parse_expridlit(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_char32>     parse_lit_char32(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_char8>      parse_lit_char8(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_str32>      parse_lit_str32(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_lit_str8>       parse_lit_str8(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_let>            parse_let(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_cond>           parse_cond(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_stexpr>         parse_stexpr(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_while>          parse_while(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_select>         parse_select(lunar_ir_module *module, parsec<char32_t> &ps);
    std::unique_ptr<lunar_ir_block>          parse_block(lunar_ir_module *module, parsec<char32_t> &ps);
    LANG_OWNERSHIP                           parse_ownership(lunar_ir_module *module, parsec<char32_t> &ps);
    template <typename T> std::unique_ptr<T> parse_def_member(lunar_ir_module *module, parsec<char32_t> &ps);
    template <typename T> std::unique_ptr<T> parse_def_member_own(lunar_ir_module *module, parsec<char32_t> &ps, LANG_OWNERSHIP own);
    template <typename T> void               parse_stexprs(lunar_ir_module *module, parsec<char32_t> &ps, T *ptr);

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
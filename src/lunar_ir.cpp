#include "lunar_ir.hpp"
#include "MCJITHelper.hpp"

#include <functional>

namespace lunar {

std::unordered_set<char32_t> idh_set;
std::unordered_set<char32_t> idt_set;

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

    idt_set.insert(U'0');
    idt_set.insert(U'1');
    idt_set.insert(U'2');
    idt_set.insert(U'3');
    idt_set.insert(U'4');
    idt_set.insert(U'5');
    idt_set.insert(U'6');
    idt_set.insert(U'7');
    idt_set.insert(U'8');
    idt_set.insert(U'9');
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
}

lunar_ir::~lunar_ir()
{
    idh_set.clear();
    idt_set.clear();
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

void
lunar_ir::print_parse_err(const std::string &str, lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto msg = ps.get_errmsg();

    std::string spaces;

    for (int i = 1; i < msg.col; i++)
        spaces += ' ';

    fprintf(stderr, "%s:%d:%d: error: %s\n%s\n%s^\n", module->get_filename().c_str(), msg.line, msg.col,
            str.c_str(), get_line(module->get_filename(), msg.line).c_str(), spaces.c_str());
}

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

std::unique_ptr<lunar_ir_identifier>
lunar_ir::parse_identifier(lunar_ir_module *module, parsec<char32_t> &ps)
{
    auto id = llvm::make_unique<std::u32string>();

    uint64_t line, col;
    line = ps.get_line();
    col  = ps.get_col();

    *id += ps.satisfy(head_identifier)();
    if (! ps.is_success()) {
        print_parse_err("invalid character", module, ps);
        return nullptr;
    }

    *id += ps.parse_many_char(ps.satisfy(tail_identifier))();

    auto ret = llvm::make_unique<lunar_ir_identifier>(std::move(id));

    ret->set_line(line);
    ret->set_col(col);

    return ret;
}

std::unique_ptr<lunar_ir_def_struct>
lunar_ir::parse_struct(lunar_ir_module *module, parsec<char32_t> &ps)
{
    ps.parse_many_char(ps.parse_space())();

    auto name = parse_identifier(module, ps);
    if (! ps.is_success())
        return nullptr;

    auto def = llvm::make_unique<lunar_ir_def_struct>(std::move(name));

    return def;
}

void
lunar_ir::parse_top(lunar_ir_module *module, parsec<char32_t> &ps)
{
    for (;;) {
        ps.parse_many_char(ps.parse_space())();

        uint64_t line, col;

        line = ps.get_line();
        col  = ps.get_col();

        ps.character(U'(')();
        if (! ps.is_success()) {
            if (ps.is_eof()) {
                ps.set_is_success(true);
            } else {
                print_parse_err("expected \"(\"", module, ps);
            }

            return;
        }

        ps.parse_many_char(ps.parse_space())();

        // parse struct
        {
            lunar::parsec<char32_t>::parser_try ptry(ps);
            ps.parse_string(U"struct")();
        }

        if (ps.is_success()) {
            auto def = parse_struct(module, ps);
            if (ps.is_success()) {
                def->set_col(col);
                def->set_line(line);
                goto success;
            } else {
                return;
            }
        }

success:
        ps.parse_many_char(ps.parse_space())();
        ps.character(U')')();
        if (! ps.is_success()) {
            print_parse_err("expected \")\"", module, ps);
            return;
        }
    }
}

void
lunar_ir::parse_module(std::unique_ptr<lunar_ir_module> module, parsec<char32_t> &ps)
{
    parse_top(module.get(), ps);
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

        printf("parse %s\n", file.c_str());

        auto str = &ir->m_files[file];
        lunar::shared_stream rs;
        lunar::shared_stream ws;

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
    lunar::init_green_thread(idx);
    lunar::spawn_green_thread(run_parse, this);
    lunar::run_green_thread();
}

}
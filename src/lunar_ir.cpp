#include "lunar_ir.hpp"
#include "MCJITHelper.hpp"

#include <functional>

namespace lunar {

lunar_ir::lunar_ir()
{

}

lunar_ir::~lunar_ir()
{

}

void
lunar_ir::compile(std::string mainfile)
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
lunar_ir::parse_module(std::unique_ptr<lunar_ir_module> module, parsec<char32_t> &ps)
{

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

        auto str = new std::u32string;
        lunar::shared_stream rs;
        lunar::shared_stream ws;

        make_ptr_stream(&rs, &ws, 2);
        
        push_ptr(&ws, str);
        push_eof(&ws);

        *str = ir->m_files[file];

        parsec<char32_t> ps(&rs);
        auto module = llvm::make_unique<lunar_ir_module>(file);

        ir->parse_module(std::move(module), ps);

        delete str;
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
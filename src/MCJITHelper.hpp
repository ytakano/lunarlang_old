#ifndef MCJITHELPER_HPP
#define MCJITHELPER_HPP

#include <string>
#include <unordered_map>
#include <memory>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ExecutionEngine/MCJIT.h>

class MCJITHelper {
public:
    MCJITHelper(llvm::LLVMContext &context) : m_context(context) { }

private:
    llvm::LLVMContext &m_context;
    std::unordered_map<std::string, std::unique_ptr<llvm::Module>> m_modules;
};

#endif // MCJITHELPER_HPP
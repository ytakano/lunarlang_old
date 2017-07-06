#ifndef MCJITHELPER_HPP
#define MCJITHELPER_HPP

#include "lunar_common.hpp"

#include <string>
#include <vector>
#include <memory>

#if LLVM_VERSION_MAJOR >= 4 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 8)
#include <llvm/Analysis/BasicAliasAnalysis.h>
#endif

#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

class MCJITHelper
{
public:
    MCJITHelper(llvm::LLVMContext& C) : Context(C), OpenModule(NULL) {}
    ~MCJITHelper();

    llvm::Function *getFunction(const std::string FnName);
    llvm::Module *getModuleForNewFunction(std::string &FnName);
    void *getPointerToFunction(llvm::Function *F);
    void *getPointerToNamedFunction(const std::string &Name);
    llvm::ExecutionEngine *compileModule(llvm::Module *M);
    void closeCurrentModule();
    void dump();

private:
    typedef std::vector<llvm::Module*> ModuleVector;

    llvm::LLVMContext  &Context;
    llvm::Module       *OpenModule;
    ModuleVector        Modules;
    std::map<llvm::Module*, llvm::ExecutionEngine*> EngineMap;
};

class HelpingMemoryManager : public llvm::SectionMemoryManager
{
    HelpingMemoryManager(const HelpingMemoryManager&) = delete;
    void operator=(const HelpingMemoryManager&) = delete;

public:
    HelpingMemoryManager(MCJITHelper *Helper) : MasterHelper(Helper) {}
    virtual ~HelpingMemoryManager() {}

    /// This method returns the address of the specified function.
    /// Our implementation will attempt to find functions in other
    /// modules associated with the MCJITHelper to cross link functions
    /// from one generated module to another.
    ///
    /// If \p AbortOnFailure is false and no function with the given name is
    /// found, this function returns a null pointer. Otherwise, it prints a
    /// message to stderr and aborts.
    virtual void *getPointerToNamedFunction(const std::string &Name,
                                            bool AbortOnFailure = true);
private:
    MCJITHelper *MasterHelper;
};

#endif // MCJITHELPER_HPP
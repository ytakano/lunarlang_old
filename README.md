# Project TSUKUYOMI (月読)

![うちはイタチ](https://dl.dropboxusercontent.com/u/74342673/itachi.jpg "うちはイタチ")

## Lunar Language

### Build

    $ ls /path/to/llvm/cmake
    AddLLVM.cmake              HandleLLVMStdlib.cmake
    AddLLVMDefinitions.cmake   LLVM-Config.cmake
    AddOCaml.cmake             LLVMConfig.cmake
    AddSphinxTarget.cmake      LLVMConfigVersion.cmake
    ChooseMSVCCRT.cmake        LLVMExports-release.cmake
    CrossCompile.cmake         LLVMExports.cmake
    FindOCaml.cmake            LLVMParseArguments.cmake
    FindSphinx.cmake           LLVMProcessSources.cmake
    GetSVN.cmake               TableGen.cmake
    HandleLLVMOptions.cmake
    $ cmake -DLLVM_DIR=/path/to/llvm/cmake
    $ make
# Project TSUKUYOMI (月読)

![うちはイタチ](https://photos-4.dropbox.com/t/2/AAC-0bnQ0wQl_IR-iCnh1j7qZodlJmnVqb0D3tZBQM2Ojw/12/74342673/jpeg/32x32/1/_/1/2/itachi.jpg/ENu82DkYpgkgBygH/3XmwzazQoSZZMq69rj_82pNvOcfnmJRw_GZp0M4jrz0?size=1280x960&size_mode=3 "うちはイタチ")

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
    $ cmake -DLLVM_DIR=/path/to/llvm/cmake .
    $ make
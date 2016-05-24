all: json_haskell json_haskell_atto json_cpp

json_haskell: json_haskell.hs
	ghc json_haskell.hs

json_haskell_atto: json_haskell_atto.hs
	ghc json_haskell_atto.hs

json_cpp: json_cpp.cpp
	c++ -g -O0 -I../../src -L../../src `/homebrew/bin/llvm-config-3.7 --cxxflags` -o json_cpp json_cpp.cpp ../../src/liblunarlang_static.a

clean:
	rm -f json_haskell json_haskell_atto json_cpp

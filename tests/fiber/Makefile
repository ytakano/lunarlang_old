all: fiber_cpp

fiber_cpp: fiber_cpp.cpp
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp ../../src/liblunarlang_static.a fiber_cpp.cpp

clean:
	rm -f fiber_cpp

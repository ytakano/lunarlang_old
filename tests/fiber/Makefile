all: fiber_cpp fiber_cpp_timer fiber_cpp_stream

fiber_cpp: fiber_cpp.cpp
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp ../../src/liblunarlang_static.a fiber_cpp.cpp

fiber_cpp_timer: fiber_cpp_timer.cpp
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_timer ../../src/liblunarlang_static.a fiber_cpp_timer.cpp

fiber_cpp_stream: fiber_cpp_stream.cpp
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_stream ../../src/liblunarlang_static.a fiber_cpp_stream.cpp

clean:
	rm -f fiber_cpp fiber_cpp_timer fiber_cpp_stream

all: fiber_cpp fiber_cpp_timer fiber_cpp_stream fiber_cpp_threadq fiber_cpp_fd

fiber_cpp: fiber_cpp.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp ../../src/liblunarlang_static.a fiber_cpp.cpp

fiber_cpp_timer: fiber_cpp_timer.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_timer ../../src/liblunarlang_static.a fiber_cpp_timer.cpp

fiber_cpp_stream: fiber_cpp_stream.cpp ../../src/liblunarlang_static.a
	c++ -O3 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_stream ../../src/liblunarlang_static.a fiber_cpp_stream.cpp

fiber_cpp_threadq: fiber_cpp_threadq.cpp ../../src/liblunarlang_static.a
	c++ -O3 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_threadq ../../src/liblunarlang_static.a fiber_cpp_threadq.cpp

fiber_cpp_fd: fiber_cpp_fd.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o fiber_cpp_fd ../../src/liblunarlang_static.a fiber_cpp_fd.cpp

clean:
	rm -f fiber_cpp fiber_cpp_timer fiber_cpp_stream fiber_cpp_threadq fiber_cpp_fd

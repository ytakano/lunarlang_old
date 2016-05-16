all: green_thread_cpp green_thread_cpp_timer green_thread_cpp_stream green_thread_cpp_threadq green_thread_cpp_fd

green_thread_cpp: green_thread_cpp.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o green_thread_cpp ../../src/liblunarlang_static.a green_thread_cpp.cpp

green_thread_cpp_timer: green_thread_cpp_timer.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o green_thread_cpp_timer ../../src/liblunarlang_static.a green_thread_cpp_timer.cpp

green_thread_cpp_stream: green_thread_cpp_stream.cpp ../../src/liblunarlang_static.a
	c++ -O3 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o green_thread_cpp_stream ../../src/liblunarlang_static.a green_thread_cpp_stream.cpp

green_thread_cpp_threadq: green_thread_cpp_threadq.cpp ../../src/liblunarlang_static.a
	c++ -O3 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o green_thread_cpp_threadq ../../src/liblunarlang_static.a green_thread_cpp_threadq.cpp

green_thread_cpp_fd: green_thread_cpp_fd.cpp ../../src/liblunarlang_static.a
	c++ -g -O0 -I../../src -L../../ `/homebrew/bin/llvm-config-3.7 --cxxflags` -o green_thread_cpp_fd ../../src/liblunarlang_static.a green_thread_cpp_fd.cpp

clean:
	rm -f green_thread_cpp green_thread_cpp_timer green_thread_cpp_stream green_thread_cpp_threadq green_thread_cpp_fd

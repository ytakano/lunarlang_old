all: bench_container

bench_container: bench_container.cpp
	c++ ../../src/liblunarlang_static.a -std=c++11 -O3 bench_container.cpp `/homebrew/bin/llvm-config-3.8 --cxxflags --ldflags` -DNDEBUG -o bench_container

clean:
	rm -f bench_container

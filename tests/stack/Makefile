all: test_stack

test_stack: test_stack.cpp
	c++ -g -O0 -I../../src -L../../src `/homebrew/bin/llvm-config-3.8 --cxxflags` -o test_stack test_stack.cpp

clean:
	rm -f test_stack

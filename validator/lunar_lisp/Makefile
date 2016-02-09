lunar_lisp_parser: lunar_lisp_parser.hs
	ghc lunar_lisp_parser.hs

test:
	./lunar_lisp_parser lisp/*

all: lunar_lisp_parser test

clean:
	rm lunar_lisp_parser *.o *.hi

all: typeinfer

typeinfer: MLType.hs SmallML.hs TestUnification.hs typeinfer.hs
	ghc --make typeinfer.hs

clean:
	rm -f *.o *.hi typeinfer

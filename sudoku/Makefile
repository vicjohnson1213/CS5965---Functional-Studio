CC = ghc

all: test

test:
	ghc solver.hs -prof -fprof-auto -fprof-cafs
	rm -rf *.hi *.o

clean:
	rm -rf *.hi *.o solver
CC = ghc

all: sol gen

sol: Solver.hs
	$(CC) Solver.hs
	rm -rf *.hi *.o

gen: Generator.hs
	$(CC) Generator.hs
	rm -rf *.hi *.o

web: Web.hs
	$(CC) Web.hs
	rm -rf *.hi *.o

clean:
	rm -rf *.hi *.o Solver Generator Web
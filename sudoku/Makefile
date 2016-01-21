CC = ghc

all: sol gen

sol:
	$(CC) Solver.hs
	rm -rf *.hi *.o

gen:
	$(CC) Generator.hs
	rm -rf *.hi *.o

clean:
	rm -rf *.hi *.o Solver Generator
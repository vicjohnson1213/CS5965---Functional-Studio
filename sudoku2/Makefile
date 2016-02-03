CC = ghc

all: sol

sol: Solver.hs Sudoku.hs
	ghc Solver.hs Sudoku.hs
	rm *.hi *.o

clean:
	rm *.hi *.o Solver
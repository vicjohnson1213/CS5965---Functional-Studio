CC = ghc

all: sol gen client

sol: Solver.hs
	$(CC) Solver.hs
	rm -rf *.hi *.o

gen: Generator.hs
	$(CC) Generator.hs
	rm -rf *.hi *.o

client: Client.hs
	$(CC) Client.hs
	rm -rf *.hi *.o

server: Server.hs
	$(CC) Server.hs
	rm -rf *.hi *.o

clean:
	rm -rf *.hi *.o Solver Generator Client Server
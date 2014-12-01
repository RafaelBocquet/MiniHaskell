all:
	cabal configure
	cabal build
	cp dist/build/MiniHaskell/MiniHaskell petitghc

clean:
	cabal clean

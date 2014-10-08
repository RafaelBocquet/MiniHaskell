all:
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Small/Token.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Small/Lexer.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Small/Parser.hs

	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Location.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Name.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Expression.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Type.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Syntax/Module.hs

	ghc -Wall -isrc -ibuild -odir build -hidir build src/Primitive.hs

	ghc -Wall -isrc -ibuild -odir build -hidir build src/Desugar/Rename.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Desugar/Typecheck.hs
	ghc -Wall -isrc -ibuild -odir build -hidir build src/Desugar/Unify.hs

	ghc -Wall -isrc -ibuild -odir build -hidir build src/Driver/Driver.hs

	ghc -Wall -isrc -ibuild -odir build -hidir build src/Main.hs -o build/Main

clean:
	$(RM) -r build

ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif
run: all
	build/Main $(RUN_ARGS)

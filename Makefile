all:
	alex -i -obuild/Syntax/Full/Lexer.hs src/Syntax/Full/Lexer.x
	happy -i -obuild/Syntax/Full/Parser.hs src/Syntax/Full/Parser.y
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Full/Token.hs
	ghc -isrc -ibuild -odir build -hidir build build/Syntax/Full/Lexer.hs
	ghc -isrc -ibuild -odir build -hidir build build/Syntax/Full/Parser.hs

	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Small/Token.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Small/Lexer.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Small/Parser.hs

	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Location.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Name.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Expression.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Type.hs
	ghc -isrc -ibuild -odir build -hidir build src/Syntax/Module.hs

	ghc -isrc -ibuild -odir build -hidir build src/Primitive.hs

	ghc -isrc -ibuild -odir build -hidir build src/Desugar/Rename.hs
	ghc -isrc -ibuild -odir build -hidir build src/Desugar/Typecheck.hs
	ghc -isrc -ibuild -odir build -hidir build src/Desugar/Unify.hs

	ghc -isrc -ibuild -odir build -hidir build src/Backend/Mips.hs
	ghc -isrc -ibuild -odir build -hidir build src/Backend/Runtime.hs

	ghc -isrc -ibuild -odir build -hidir build src/Driver/Driver.hs

	ghc -isrc -ibuild -odir build -hidir build src/Main.hs -o build/Main

clean:
	$(RM) -r build

ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif
run: all
	build/Main $(RUN_ARGS)
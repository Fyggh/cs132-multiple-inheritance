BINARIES = hasty

all: bin $(BINARIES)

bin:
	mkdir -p bin tests

doc:
	haddock --html -o html src/**/*.hs

src/Parser/HastyLexer.hs: src/Parser/HastyLexer.x Makefile
	alex src/Parser/HastyLexer.x

lexer: src/Parser/HastyLexer.hs

src/Parser/HastyParser.hs: src/Parser/HastyLexer.hs src/Parser/HastyParser.y Makefile
	happy -i src/Parser/HastyParser.y
	
src/Parser/DebugHastyParser.hs: src/Parser/HastyLexer.hs src/Parser/HastyParser.y Makefile
	cp src/Parser/HastyParser.y src/Parser/DebugHastyParser.y
	sed -i 's/HastyParser/DebugHastyParser/g' src/Parser/DebugHastyParser.y
	happy -i -a -d src/Parser/DebugHastyParser.y
	rm src/Parser/DebugHastyParser.y

parser: src/Parser/HastyParser.hs src/Parser/DebugHastyParser.hs

hasty: src/hasty.hs src/Parser/HastyLexer.hs src/Parser/HastyParser.hs src/Parser/DebugHastyParser.hs $(shell find ./src -type f)
	ghc --make -no-pie -o ./hasty -odir bin -hidir bin -Wall -isrc src/hasty.hs

clean:
	rm -f src/Parser/*.hs src/Parser/*.info
	rm -rf programs/*.o programs/*.hi programs/*.liveness programs/*.assem programs/*.s programs/*.t programs/*.nt programs/*.tasty
	rm -rf bin
	rm -f ./hasty


.PHONY: clean all

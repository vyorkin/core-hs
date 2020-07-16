GHC_OPTIONS := --ghc-options='-fdiagnostics-color=never -ferror-spans -fhide-source-paths' # -fprint-unicode-syntax

CH := 02
CH_DIR := src/Core/Ch$(CH)

dev: all
	ghcid --restart=$(CH_DIR)/Lexer.x --restart=$(CH_DIR)/Parser.y --command="cabal repl $(GHC_OPTIONS)" | source-highlight -s haskell -f esc
repl:
	cabal repl $(GHC_OPTIONS)
all:
	cabal build $(GHC_OPTIONS) all
clean:
	cabal clean
check:
	cabal check
test:
	cabal test
test-accept:
	cabal test --test-options=--accept
parser:
	happy -agc --strict $(CH_DIR)/Parser.y -i$(CH_DIR)/Parser.info
parser-clean:
	rm -f $(CH_DIR)/Parser.{hs,info}
tags:
	rm -f tags codex.tags
	codex update --force
	haskdogs --hasktags-args "-b"
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling
hoogle:
	hoogle server --local

.PHONY: dev repl clean all test test-accept check parser parser-clean tags prof noprof hoogle

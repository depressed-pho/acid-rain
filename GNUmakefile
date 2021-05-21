CABAL_BUILD_OPTS?= \
	-j \
	--enable-executable-dynamic \
	--allow-newer=Cabal \
	--allow-newer=base \
	--allow-newer=containers \
	--allow-newer=ghc-prim \
	--allow-newer=primitive-extras \
	--ghc-option=-fno-ignore-asserts \
	--ghc-option=-dth-dec-file \
	--flags="debug"

# Because the compilation mode of Emacs get confused by escape
# sequences.
BUILD_TERM=dumb

# Because the system terminfo in NetBSD 8.1 doesn't recognize
# screen.xterm-256color and falls back to 8 colors mode.
RUN_TERM=xterm-256color

all:
	env TERM=$(BUILD_TERM) cabal v2-build $(CABAL_BUILD_OPTS) all

freeze:
	cabal v2-freeze

doc:
	cabal v2-haddock $(CABAL_BUILD_OPTS) all

run:
	env TERM=$(RUN_TERM) cabal v2-run $(CABAL_BUILD_OPTS) all

repl: # Usage: make COMP=foo
	cabal v2-repl $(CABAL_BUILD_OPTS) $(COMP)

clean:
	cabal v2-clean

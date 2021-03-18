CABAL_BUILD_OPTS?= --enable-executable-dynamic

# Because the compilation mode of Emacs get confused by escape
# sequences.
BUILD_TERM=dumb

# Because the system terminfo currently doesn't recognize
# screen.xterm-256color and falls back to 8 colors mode.
RUN_TERM=xterm-256color

all:
	env TERM=$(BUILD_TERM) cabal v2-build $(CABAL_BUILD_OPTS) all

freeze:
	cabal v2-freeze

doc:
	cabal v2-haddock all

run:
	env TERM=$(RUN_TERM) cabal v2-run $(CABAL_BUILD_OPTS) all

clean:
	cabal v2-clean

CABAL_BUILD_OPTS?= --enable-executable-dynamic

# Because the system terminfo currently doesn't recognize
# screen.xterm-256color and falls back to 8 colors mode.
export TERM=xterm-256color

all:
	cabal v2-build $(CABAL_BUILD_OPTS) all

freeze:
	cabal v2-freeze

doc:
	cabal v2-haddock all

run:
	cabal v2-run $(CABAL_BUILD_OPTS) all

clean:
	cabal v2-clean

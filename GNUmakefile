CABAL_BUILD_OPTS?= --enable-executable-dynamic

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

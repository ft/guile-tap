PROJECT = guile-tap
TOPDIR = .
LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/test

GUILE_BINARY ?= guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CFLAGS = -Wunsupported-warning -Wunused-variable # -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)

TESTGUILE = ./tools/run-single-test
PROVE = '$(TESTGUILE)' ./bin/tap-harness --colour -e '$(TESTGUILE)'

INSTALL = $(GUILE_BINARY) --no-auto-compile ./tools/install
DESTDIR =
PREFIX = /usr/local
DOCDIR = $(PREFIX)/share/doc/$(PROJECT)

MODULES_CORE =  $(TOPDIR)/scheme/test/tap.scm
MODULES_CORE += $(TOPDIR)/scheme/test/tap-harness.scm
MODULES = $(MODULES_CORE)
OBJECTS = ${MODULES:.scm=.go}
TESTS = test/*.scm

.SUFFIXES: .scm .go

all: $(OBJECTS)

.scm.go:
	$(COMPILE) -o $@ $<

doc:
	@(cd doc && $(MAKE) all;)

test:
	$(PROVE) $(TESTS)

test-verbose:
	$(PROVE) --verbose $(TESTS)

failures:
	$(PROVE) examples/*.scm || true

install: all
	$(INSTALL) DESTDIR="$(DESTDIR)" DOCDIR="$(DOCDIR)" PREFIX="$(PREFIX)"

clean:
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +
	(cd doc && $(MAKE) clean;)

.PHONY: all clean doc failures install test

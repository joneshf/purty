.SUFFIXES:
Makefile:;

ACCEPTANCE_SCRIPT := test/acceptance.sh
ACCEPTANCE_SCRIPTFLAGS := --verbose
BINARY := purty
BINDIR := bin
CP := cp
GIT := git
LIB_HS := $(wildcard lib/*.hs)
LIBS := $(LIB_HS)
MKDIR := mkdir
PURTY_JS := $(CURDIR)/$(BINDIR)/purty.js
OS := linux
SRC_HS := $(wildcard src/*.hs)
SRCS := $(SRC_HS)
STACK := stack
STACKFLAGS :=
STACK_BUILDFLAGS :=
STACK_BUILD := $(STACK) $(STACKFLAGS) build $(STACK_BUILDFLAGS)
TEST_ACCEPTANCE_PURS := $(wildcard test/acceptance/*.purs)
TEST_GOLDEN_FORMATTED_PURS := $(wildcard test/golden/files/formatted/*.purs)
TEST_GOLDEN_HS := $(wildcard test/golden/*.hs)
TEST_GOLDEN_ORIGINAL_PURS := $(wildcard test/golden/files/original/*.purs)
TEST_GOLDEN_PURS := $(TEST_GOLDEN_FORMATTED_PURS) $(TEST_GOLDEN_ORIGINAL_PURS)
TESTS := $(TEST_ACCEPTANCE_PURS) $(TEST_GOLDEN_HS) $(TEST_GOLDEN_PURS)

.DEFAULT_GOAL := test

$(BINDIR)/$(BINARY): $(LIBS) $(SRCS) $(TESTS) package.yaml stack.yaml
	$(STACK_BUILD) --copy-bins --local-bin-path $(BINDIR) --no-run-tests --test

$(BINDIR)/$(OS):
	@$(MKDIR) -p $@

$(BINDIR)/$(OS)/$(BINARY): $(BINDIR)/$(BINARY) | $(BINDIR)/$(OS)
	@$(CP) $< $@

.PHONY: clean
clean:
	@$(GIT) clean -X --force $(BINDIR)/*

.PHONY: test
test: test-acceptance test-golden

.PHONY: test-acceptance
test-acceptance: test-acceptance-binary test-acceptance-npm

.PHONY: test-acceptance-binary
test-acceptance-binary: $(ACCEPTANCE_SCRIPT) $(BINDIR)/$(BINARY)
	$(info Testing binary interface)
	$(ACCEPTANCE_SCRIPT) $(ACCEPTANCE_SCRIPTFLAGS)

.PHONY: test-acceptance-npm
test-acceptance-npm: $(ACCEPTANCE_SCRIPT) $(BINDIR)/$(OS)/$(BINARY) $(PURTY_JS)
	$(info Testing npm interface)
	$(ACCEPTANCE_SCRIPT) $(ACCEPTANCE_SCRIPTFLAGS) --purty $(PURTY_JS)

.PHONY: test-golden
test-golden: $(BINDIR)/$(BINARY)
	$(STACK_BUILD) --test purty:test:golden

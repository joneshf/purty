.SUFFIXES:
Makefile:;

ACCEPTANCE_SCRIPT := test/acceptance.sh
ACCEPTANCE_SCRIPTFLAGS := --verbose
PURTY_JS := $(CURDIR)/bin/purty.js
STACK := stack
STACKFLAGS :=
STACK_TESTFLAGS :=
STACK_TEST := $(STACK) $(STACKFLAGS) build --test $(STACK_TESTFLAGS)

.DEFAULT_GOAL := test

.PHONY: test
test: test-acceptance test-golden

.PHONY: test-acceptance
test-acceptance: test-acceptance-binary test-acceptance-npm

.PHONY: test-acceptance-binary
test-acceptance-binary: $(ACCEPTANCE_SCRIPT)
	$(info Testing binary interface)
	$(ACCEPTANCE_SCRIPT) $(ACCEPTANCE_SCRIPTFLAGS)

.PHONY: test-acceptance-npm
test-acceptance-npm: $(ACCEPTANCE_SCRIPT)
	$(info Testing npm interface)
	$(ACCEPTANCE_SCRIPT) $(ACCEPTANCE_SCRIPTFLAGS) --purty $(PURTY_JS)

.PHONY: test-golden
test-golden:
	$(STACK_TEST) $(STACK_TESTFLAGS) purty:test:golden

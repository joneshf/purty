.SUFFIXES:
Makefile:;

ACCEPTANCE_SCRIPT := test/acceptance.sh
ACCEPTANCE_SCRIPTFLAGS := --verbose
BINARY := purty
BINDIR := bin
BUILDDIR := .build
CIDIR := ci
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
VERSION_DHALL_HASKELL := 1.30.0
VERSION_DHALL_TO_JSON := 1.6.2
VERSION_PURTY :=

BINTRAY_DHALL := $(CIDIR)/bintray.dhall
BINTRAY_JSON := $(BUILDDIR)/$(OS)/bintray.json
CONFIGURED_BINTRAY_DHALL := $(BUILDDIR)/$(OS)/bintray-configured.dhall
CONFIGURED_PACKAGE_DHALL := $(BUILDDIR)/package-configured.dhall
DHALL_TO_JSON := $(BUILDDIR)/$(OS)/dhall-to-json
DHALL_TO_JSON_TAR := $(BUILDDIR)/$(OS)/dhall-json-$(VERSION_DHALL_TO_JSON).tar.bz2
NPM_PACKAGE_DHALL := $(CIDIR)/npm/package.dhall
PACKAGE_JSON := package.json
PURTY_TAR := $(BUILDDIR)/$(OS)/purty-$(VERSION_PURTY).tar.gz
PURTY_TAR_UPLOADED_FILENAME := purty-$(VERSION_PURTY)-$(OS).tar.gz
RELEASE_DATE := $(BUILDDIR)/release-date

ifeq ($(OS),linux)
DHALL_TO_JSON_ARCHIVE_FILE := ./bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 2
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-linux.tar.bz2
else ifeq ($(OS),osx)
DHALL_TO_JSON_ARCHIVE_FILE := bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 1
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-macos.tar.bz2
endif

.DEFAULT_GOAL := test

$(BINDIR)/$(BINARY): $(LIBS) $(SRCS) $(TESTS) package.yaml stack.yaml
	$(STACK_BUILD) --copy-bins --local-bin-path $(BINDIR) --no-run-tests --test

$(BINDIR)/$(OS) $(BUILDDIR) $(BUILDDIR)/$(OS):
	@$(MKDIR) -p $@

$(BINDIR)/$(OS)/$(BINARY): $(BINDIR)/$(BINARY) | $(BINDIR)/$(OS)
	@$(CP) $< $@

$(BINTRAY_JSON): $(CONFIGURED_BINTRAY_DHALL) $(DHALL_TO_JSON) | $(BUILDDIR)/$(OS)
	$(info Generating $@ file)
	@$(DHALL_TO_JSON) --file $< --output $@

$(CONFIGURED_BINTRAY_DHALL): $(BINTRAY_DHALL) $(RELEASE_DATE) $(PURTY_TAR) | $(BUILDDIR)/$(OS)
	echo '$(CURDIR)/$< {date = "$(shell cat $(RELEASE_DATE))", tarFile = "$(PURTY_TAR)", uploadedFilename = "$(PURTY_TAR_UPLOADED_FILENAME)", version = "$(VERSION_PURTY)"}' > $@

$(CONFIGURED_PACKAGE_DHALL): $(NPM_PACKAGE_DHALL) | $(BUILDDIR)
	echo '$(CURDIR)/$< {version = "$(VERSION_PURTY)"}' > $@

$(DHALL_TO_JSON_TAR): | $(BUILDDIR)/$(OS)
	$(info Downloading dhall-to-json binary)
	curl --location --output $@ $(DHALL_TO_JSON_URI)

$(DHALL_TO_JSON): $(DHALL_TO_JSON_TAR) | $(BUILDDIR)/$(OS)
	@tar --extract --file $< --directory $(dir $@) --bzip2 --strip-components $(DHALL_TO_JSON_ARCHIVE_STRIP) $(DHALL_TO_JSON_ARCHIVE_FILE)
	@touch $@

$(PACKAGE_JSON): $(CONFIGURED_PACKAGE_DHALL) $(DHALL_TO_JSON)
	$(info Generating $@ file)
	@$(DHALL_TO_JSON) --file $< --output $@

$(PURTY_TAR): $(BINDIR)/$(OS)/purty | $(BUILDDIR)/$(OS)
	$(info Creating $@ tarball)
	@tar --create --file $@ --directory $(BINDIR)/$(OS) --gzip purty

.PHONY: $(RELEASE_DATE)
$(RELEASE_DATE): | $(BUILDDIR)
	$(info Capturing current date)
	@date '+%Y-%m-%d' > $@

.PHONY: bintray-artifacts
bintray-artifacts: $(BINTRAY_JSON) $(PURTY_TAR)

.PHONY: clean
clean:
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm $(PACKAGE_JSON)
	@$(GIT) clean -X --force $(BINDIR)/*

.PHONY: npm-publish
npm-publish: $(PACKAGE_JSON)
	$(info Publishing to npm)
	npm publish

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

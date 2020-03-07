.SUFFIXES:
Makefile:;

ACCEPTANCE_SCRIPT := test/acceptance.sh
ACCEPTANCE_SCRIPTFLAGS := --verbose
BAZEL_BINDIR := bazel-bin
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
VERSIONDIR := version
VERSION_PURTY_FILE := $(VERSIONDIR)/purty
VERSION_BAZEL := 2.2.0
VERSION_DHALL_HASKELL := 1.30.0
VERSION_DHALL_TO_JSON := 1.6.2
VERSION_HLINT := 2.2.11
VERSION_PURTY :=
VERSION_WEEDER := 1.0.8

ALL_HASKELL_FILES := Setup.hs $(LIB_HS) $(SRC_HS) $(TEST_GOLDEN_HS)
BINTRAY_DHALL := $(CIDIR)/bintray.dhall
BINTRAY_JSON := $(BUILDDIR)/$(OS)/bintray.json
CONFIGURED_BINTRAY_DHALL := $(BUILDDIR)/$(OS)/bintray-configured.dhall
CONFIGURED_PACKAGE_DHALL := $(BUILDDIR)/package-configured.dhall
DHALL_TO_JSON := $(BUILDDIR)/$(OS)/dhall-to-json
DHALL_TO_JSON_TAR := $(BUILDDIR)/$(OS)/dhall-json-$(VERSION_DHALL_TO_JSON).tar.bz2
FORMATDIR := $(BUILDDIR)/format
FORMAT_HASKELL_FILES := $(addprefix $(FORMATDIR)/,$(ALL_HASKELL_FILES))
LINTDIR_HLINT := $(BUILDDIR)/lint/hlint
LINTDIR_ORMOLU := $(BUILDDIR)/lint/ormolu
LINTDIR_WEEDER := $(BUILDDIR)/lint/weeder
LINT_HASKELL_HLINT_FILES := $(addprefix $(LINTDIR_HLINT)/,$(ALL_HASKELL_FILES))
LINT_HASKELL_ORMOLU_FILES := $(addprefix $(LINTDIR_ORMOLU)/,$(ALL_HASKELL_FILES))
NPM_PACKAGE_DHALL := $(CIDIR)/npm/package.dhall
ORMOLU := $(BUILDDIR)/ormolu
PACKAGE_JSON := package.json
PURTY_TAR := $(BUILDDIR)/$(OS)/purty-$(VERSION_PURTY).tar.gz
PURTY_TAR_UPLOADED_FILENAME := purty-$(VERSION_PURTY)-$(OS).tar.gz
RELEASE_DATE := $(BUILDDIR)/release-date

ifeq ($(OS),linux)
BAZEL := $(BUILDDIR)/bazel
BAZEL_PURTY_BINARY := src/purty
DHALL_TO_JSON_ARCHIVE_FILE := ./bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 2
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-linux.tar.bz2
HLINT := $(BUILDDIR)/hlint
HLINT_ARCHIVE := $(BUILDDIR)/hlint-$(VERSION_HLINT)-x86_64-linux.tar.gz
HLINT_ARCHIVE_FILE := hlint-$(VERSION_HLINT)/hlint
HLINT_ARCHIVE_STRIP := 1
HLINT_ARCHIVE_URI := https://github.com/ndmitchell/hlint/releases/download/v$(VERSION_HLINT)/hlint-$(VERSION_HLINT)-x86_64-linux.tar.gz
WEEDER := $(BUILDDIR)/weeder
WEEDER_ARCHIVE := $(BUILDDIR)/weeder-$(VERSION_WEEDER)-x86_64-linux.tar.gz
WEEDER_ARCHIVE_FILE := weeder-$(VERSION_WEEDER)/weeder
WEEDER_ARCHIVE_STRIP := 1
WEEDER_ARCHIVE_URI := https://github.com/ndmitchell/weeder/releases/download/v$(VERSION_WEEDER)/weeder-$(VERSION_WEEDER)-x86_64-linux.tar.gz
else ifeq ($(OS),osx)
BAZEL := $(BUILDDIR)/bazel
BAZEL_PURTY_BINARY := src/purty
DHALL_TO_JSON_ARCHIVE_FILE := bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 1
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-macos.tar.bz2
HLINT := $(BUILDDIR)/hlint
HLINT_ARCHIVE := $(BUILDDIR)/hlint-$(VERSION_HLINT)-x86_64-osx.tar.gz
HLINT_ARCHIVE_FILE := hlint-$(VERSION_HLINT)/hlint
HLINT_ARCHIVE_STRIP := 1
HLINT_ARCHIVE_URI := https://github.com/ndmitchell/hlint/releases/download/v$(VERSION_HLINT)/hlint-$(VERSION_HLINT)-x86_64-osx.tar.gz
WEEDER := $(BUILDDIR)/weeder
WEEDER_ARCHIVE := $(BUILDDIR)/weeder-$(VERSION_WEEDER)-x86_64-osx.tar.gz
WEEDER_ARCHIVE_FILE := weeder-$(VERSION_WEEDER)/weeder
WEEDER_ARCHIVE_STRIP := 1
WEEDER_ARCHIVE_URI := https://github.com/ndmitchell/weeder/releases/download/v$(VERSION_WEEDER)/weeder-$(VERSION_WEEDER)-x86_64-osx.tar.gz
else ifeq ($(OS),windows)
BAZEL := $(BUILDDIR)/bazel.exe
BAZEL_PURTY_BINARY := src/purty.exe
HLINT := $(BUILDDIR)/hlint.exe
HLINT_ARCHIVE := $(BUILDDIR)/hlint-$(VERSION_HLINT)-x86_64-windows.zip
HLINT_ARCHIVE_FILE := hlint-$(VERSION_HLINT)/hlint.exe
HLINT_ARCHIVE_URI := https://github.com/ndmitchell/hlint/releases/download/v$(VERSION_HLINT)/hlint-$(VERSION_HLINT)-x86_64-windows.zip
WEEDER := $(BUILDDIR)/weeder.exe
WEEDER_ARCHIVE := $(BUILDDIR)/weeder-$(VERSION_WEEDER)-x86_64-windows.zip
WEEDER_ARCHIVE_FILE := weeder-$(VERSION_WEEDER)/weeder.exe
WEEDER_ARCHIVE_URI := https://github.com/ndmitchell/weeder/releases/download/v$(VERSION_WEEDER)/weeder-$(VERSION_WEEDER)-x86_64-windows.zip
endif

.DEFAULT_GOAL := bootstrap

$(BAZEL): | $(BUILDDIR)
	$(info Downloading bazel binary)
ifeq ($(OS),linux)
	curl --location --output $@ https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-linux-x86_64
	@chmod 0755 $@
else ifeq ($(OS),osx)
	curl --location --output $@ https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-darwin-x86_64
	@chmod 0755 $@
else ifeq ($(OS),windows)
	curl --location --output $@ https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-windows-x86_64.exe
endif
	@touch $@
	$(BAZEL) version

$(BAZEL_BINDIR)/$(BAZEL_PURTY_BINARY): $(BAZEL)
	$(BAZEL) build //src:purty

$(BINDIR)/$(BINARY): $(BAZEL_BINDIR)/$(BAZEL_PURTY_BINARY)
	@$(CP) $< $@

$(BINDIR)/$(OS) $(BUILDDIR) $(BUILDDIR)/$(OS) $(LINTDIR_WEEDER):
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

$(FORMAT_HASKELL_FILES): $(FORMATDIR)/%: % $(ORMOLU)
	$(info Formatting $*)
	@$(ORMOLU) --mode inplace $*
	@mkdir -p $(basename $@)
	@touch $@

$(HLINT): $(HLINT_ARCHIVE) | $(BUILDDIR)
	$(info Extracting hlint binary)
ifeq ($(OS),linux)
	@tar --extract --file $< --directory $(BUILDDIR) --gzip --strip-components $(HLINT_ARCHIVE_STRIP) $(HLINT_ARCHIVE_FILE)
else ifeq ($(OS),osx)
	@tar --extract --file $< --directory $(BUILDDIR) --gzip --strip-components $(HLINT_ARCHIVE_STRIP) $(HLINT_ARCHIVE_FILE)
else ifeq ($(OS),windows)
	@7z e $< -o$(BUILDDIR) $(HLINT_ARCHIVE_FILE)
endif
	@touch $@
	$(HLINT) --version

$(HLINT_ARCHIVE): | $(BUILDDIR)
	$(info Downloading hlint binary)
	curl --location --output $(HLINT_ARCHIVE) $(HLINT_ARCHIVE_URI)

$(LINT_HASKELL_HLINT_FILES): $(LINTDIR_HLINT)/%: % $(HLINT)
	$(info Linting $* with hlint)
	@$(HLINT) $*
	@mkdir -p $(basename $@)
	@touch $@

$(LINT_HASKELL_ORMOLU_FILES): $(LINTDIR_ORMOLU)/%: % $(ORMOLU)
	$(info Linting $* with ormolu)
	@$(ORMOLU) --mode check $* || (echo $* is not formatted properly. Please run 'make format'.; exit 1)
	@mkdir -p $(basename $@)
	@touch $@

$(LINTDIR_WEEDER)/stack.yaml: $(BINDIR)/$(BINARY) $(WEEDER) | $(LINTDIR_WEEDER)
	$(info Linting with weeder)
	@$(WEEDER) .
	@touch $@

$(ORMOLU): stack.yaml
	$(STACK_BUILD) --copy-bins --local-bin-path $(BUILDDIR) ormolu

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

$(WEEDER): $(WEEDER_ARCHIVE) | $(BUILDDIR)
	$(info Extracting weeder binary)
ifeq ($(OS),linux)
	@tar --extract --file $< --directory $(BUILDDIR) --gzip --strip-components $(WEEDER_ARCHIVE_STRIP) $(WEEDER_ARCHIVE_FILE)
else ifeq ($(OS),osx)
	@tar --extract --file $< --directory $(BUILDDIR) --gzip --strip-components $(WEEDER_ARCHIVE_STRIP) $(WEEDER_ARCHIVE_FILE)
else ifeq ($(OS),windows)
	@7z e $< -o$(BUILDDIR) $(WEEDER_ARCHIVE_FILE)
endif
	@touch $@
	$(WEEDER) --version

$(WEEDER_ARCHIVE): | $(BUILDDIR)
	$(info Downloading weeder binary)
	curl --location --output $(WEEDER_ARCHIVE) $(WEEDER_ARCHIVE_URI)

.PHONY: bintray-artifacts
bintray-artifacts: $(BINTRAY_JSON) $(PURTY_TAR)

.PHONY: bootstrap
bootstrap: $(BAZEL)

.PHONY: clean
clean:
	$(info Removing bazel artifacts)
	@$(BAZEL) clean
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm $(PACKAGE_JSON)
	@$(GIT) clean -X --force $(BINDIR)/*

.PHONY: format
format: format-haskell

.PHONY: format-haskell
format-haskell: $(FORMAT_HASKELL_FILES)

.PHONY: lint
lint: lint-haskell

.PHONY: lint-haskell
lint-haskell: lint-haskell-hlint lint-haskell-ormolu lint-haskell-weeder

.PHONY: lint-haskell-hlint
lint-haskell-hlint: $(LINT_HASKELL_HLINT_FILES)

.PHONY: lint-haskell-ormolu
lint-haskell-ormolu: $(LINT_HASKELL_ORMOLU_FILES)

.PHONY: lint-haskell-weeder
lint-haskell-weeder: $(LINTDIR_WEEDER)/stack.yaml

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
test-golden: $(BAZEL)
	$(BAZEL) test //test/golden:purty

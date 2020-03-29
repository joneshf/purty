.SUFFIXES:
Makefile:;

BAZEL_BINDIR := bazel-bin
BINDIR := bin
BUILDDIR := .build
CIDIR := ci
CP := cp
GIT := git
MKDIR := mkdir
OS := linux
VERSIONDIR := version
VERSION_BAZEL := 2.2.0
VERSION_DHALL_HASKELL := 1.30.0
VERSION_DHALL_TO_JSON := 1.6.2
VERSION_IBAZEL := 0.12.3
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
BAZEL := $(BUILDDIR)/bazel
BINARY := purty
DHALL_TO_JSON_ARCHIVE_FILE := ./bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 2
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-linux.tar.bz2
IBAZEL := $(BUILDDIR)/ibazel
PURTY_BINARY := purty-binary
else ifeq ($(OS),osx)
BAZEL := $(BUILDDIR)/bazel
BINARY := purty
DHALL_TO_JSON_ARCHIVE_FILE := bin/dhall-to-json
DHALL_TO_JSON_ARCHIVE_STRIP := 1
DHALL_TO_JSON_URI := https://github.com/dhall-lang/dhall-haskell/releases/download/$(VERSION_DHALL_HASKELL)/dhall-json-$(VERSION_DHALL_TO_JSON)-x86_64-macos.tar.bz2
IBAZEL := $(BUILDDIR)/ibazel
PURTY_BINARY := purty-binary
else ifeq ($(OS),windows)
BAZEL := $(BUILDDIR)/bazel.exe
BINARY := purty.exe
IBAZEL := $(BUILDDIR)/ibazel.exe
PURTY_BINARY := purty-binary.exe
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

$(BAZEL_BINDIR)/$(PURTY_BINARY): $(BAZEL)
	$(BAZEL) build //:purty-binary

$(BINDIR)/$(BINARY): $(BAZEL_BINDIR)/$(PURTY_BINARY)
	@$(CP) $< $@
ifeq ($(OS),linux)
	@chmod 0755 $@
else ifeq ($(OS),osx)
	@chmod 0755 $@
endif

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

$(IBAZEL): | $(BUILDDIR)
	$(info Downloading ibazel binary)
ifeq ($(OS),linux)
	curl --location --output $@ https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_linux_amd64
	@chmod 0755 $@
else ifeq ($(OS),osx)
	curl --location --output $@ https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_darwin_amd64
	@chmod 0755 $@
else ifeq ($(OS),windows)
	curl --location --output $@ https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_windows_amd64.exe
endif
	@touch $@
	$(IBAZEL) version

$(PACKAGE_JSON): $(CONFIGURED_PACKAGE_DHALL) $(DHALL_TO_JSON)
	$(info Generating $@ file)
	@$(DHALL_TO_JSON) --file $< --output $@

$(PURTY_TAR): $(BINDIR)/$(OS)/$(BINARY) | $(BUILDDIR)/$(OS)
	$(info Creating $@ tarball)
	@tar --create --file $@ --directory $(BINDIR)/$(OS) --gzip $(BINARY)

.PHONY: $(RELEASE_DATE)
$(RELEASE_DATE): | $(BUILDDIR)
	$(info Capturing current date)
	@date '+%Y-%m-%d' > $@

.PHONY: bintray-artifacts
bintray-artifacts: $(BINTRAY_JSON) $(PURTY_TAR)

.PHONY: bootstrap
bootstrap: $(BAZEL) $(IBAZEL)

.PHONY: clean
clean:
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm -f $(PACKAGE_JSON)
	@$(GIT) clean -X --force $(BINDIR)/*

.PHONY: format
format: format-haskell

.PHONY: format-haskell
format-haskell: $(BAZEL)
	$(BAZEL) run //:format-ormolu

.PHONY: lint
lint: lint-haskell

.PHONY: lint-haskell
lint-haskell: lint-haskell-hlint lint-haskell-ormolu

.PHONY: lint-haskell-hlint
lint-haskell-hlint: $(BAZEL)
	$(BAZEL) test //:lint-hlint

.PHONY: lint-haskell-ormolu
lint-haskell-ormolu: $(BAZEL)
	$(BAZEL) test //:lint-ormolu

.PHONY: npm-publish
npm-publish: $(PACKAGE_JSON)
	$(info Publishing to npm)
	npm publish

.PHONY: test
test: test-acceptance test-golden

.PHONY: test-acceptance
test-acceptance: test-acceptance-binary test-acceptance-npm

.PHONY: test-acceptance-binary
test-acceptance-binary: $(BAZEL)
	$(info Testing binary interface)
	$(BAZEL) test //:test-acceptance-binary

.PHONY: test-acceptance-npm
test-acceptance-npm: $(BAZEL)
	$(info Testing npm interface)
	$(BAZEL) test //:test-acceptance-npm

.PHONY: test-golden
test-golden: $(BAZEL)
	$(BAZEL) test //:purty-golden

.PHONY: watch
watch: $(IBAZEL)
	$(IBAZEL) test //...

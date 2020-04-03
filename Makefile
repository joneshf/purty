.SUFFIXES:
Makefile:;

BINDIR := bin
BUILDDIR := .build
OS := linux
VERSION_BAZEL := 2.2.0
VERSION_IBAZEL := 0.12.3

PACKAGE_JSON := package.json

ifeq ($(OS),linux)
BAZEL := $(BUILDDIR)/bazel
IBAZEL := $(BUILDDIR)/ibazel
else ifeq ($(OS),osx)
BAZEL := $(BUILDDIR)/bazel
IBAZEL := $(BUILDDIR)/ibazel
else ifeq ($(OS),windows)
BAZEL := $(BUILDDIR)/bazel.exe
IBAZEL := $(BUILDDIR)/ibazel.exe
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

$(BUILDDIR):
	@mkdir -p $@

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

$(PACKAGE_JSON): $(BAZEL)
	$(info Generating $@ file)
	$(BAZEL) build //:package.json
	cp $(BAZEL_BINDIR)/ci/npm/package.json $@

.PHONY: bootstrap
bootstrap: $(BAZEL) $(IBAZEL)

.PHONY: clean
clean:
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm -f $(PACKAGE_JSON)
	@git clean -X --force $(BINDIR)/*

.PHONY: format
format: format-haskell

.PHONY: format-haskell
format-haskell: $(BAZEL)
	$(BAZEL) run //:format-ormolu

.PHONY: lint
lint: $(BAZEL)
	$(BAZEL) test //:lint

.PHONY: npm-publish
npm-publish: $(PACKAGE_JSON)
	$(info Publishing to npm)
	npm publish

.PHONY: test
test: $(BAZEL)
	$(BAZEL) test //...

.PHONY: watch
watch: $(IBAZEL)
	$(IBAZEL) test //...

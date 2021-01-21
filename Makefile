.SUFFIXES:
Makefile:;

BAZEL_BINDIR := bazel-bin
BAZEL_CONFIG :=
BINDIR := bin
BUILDDIR := .build
VERSION_BAZEL := 3.7.0
VERSION_IBAZEL := 0.14.0

PACKAGE_JSON := package.json

# Based on https://stackoverflow.com/a/12099167/1549047.
ifeq ($(OS),Windows_NT)
BAZEL := $(BUILDDIR)/bazel.exe
BAZEL_URI := https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-windows-x86_64.exe
IBAZEL := $(BUILDDIR)/ibazel.exe
IBAZEL_URI := https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_windows_amd64.exe
else ifeq ($(shell uname -s),Linux)
BAZEL := $(BUILDDIR)/bazel
BAZEL_URI := https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-linux-x86_64
IBAZEL := $(BUILDDIR)/ibazel
IBAZEL_URI := https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_linux_amd64
else ifeq ($(shell uname -s),Darwin)
BAZEL := $(BUILDDIR)/bazel
BAZEL_URI := https://github.com/bazelbuild/bazel/releases/download/$(VERSION_BAZEL)/bazel-$(VERSION_BAZEL)-darwin-x86_64
IBAZEL := $(BUILDDIR)/ibazel
IBAZEL_URI := https://github.com/bazelbuild/bazel-watcher/releases/download/v$(VERSION_IBAZEL)/ibazel_darwin_amd64
else
$(error Platform not supported. Only Linux, macOS, and Windows are supported)
endif

.DEFAULT_GOAL := test

$(BAZEL): | $(BUILDDIR)
	$(info Downloading bazel binary)
	curl --location --output $@ $(BAZEL_URI)
	@chmod 0755 $@
	@touch $@
	$(BAZEL) version

$(BUILDDIR):
	@mkdir -p $@

$(IBAZEL): $(BAZEL) | $(BUILDDIR)
	$(info Downloading ibazel binary)
	curl --location --output $@ $(IBAZEL_URI)
	@chmod 0755 $@
	@touch $@
	$(IBAZEL) -bazel_path $(BAZEL) version

$(PACKAGE_JSON): $(BAZEL)
	$(info Generating $@ file)
	$(BAZEL) build //ci/npm:package.json
	cp $(BAZEL_BINDIR)/ci/npm/package.json $@

.PHONY: clean
clean:
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm -f $(PACKAGE_JSON)
	@git clean -X --force $(BINDIR)/*

.PHONY: coverage
coverage: $(BAZEL)
	$(BAZEL) coverage $(BAZEL_CONFIG) //...

.PHONY: format
format: $(BAZEL)
	$(BAZEL) run $(BAZEL_CONFIG) //cmd/purs-format:format-ormolu
	$(BAZEL) run $(BAZEL_CONFIG) //internal/error:format-ormolu
	$(BAZEL) run $(BAZEL_CONFIG) //internal/format:format-ormolu
	$(BAZEL) run $(BAZEL_CONFIG) //internal/log:format-ormolu
	$(BAZEL) run $(BAZEL_CONFIG) //internal/version:format-ormolu
	$(BAZEL) run $(BAZEL_CONFIG) //pkg/cst:format-ormolu

.PHONY: lint
lint: $(BAZEL)
	$(BAZEL) test $(BAZEL_CONFIG) //:lint

.PHONY: npm-publish
npm-publish: $(PACKAGE_JSON)
	$(info Publishing to npm)
	npm publish

.PHONY: test
test: $(BAZEL)
	$(BAZEL) test $(BAZEL_CONFIG) //...

.PHONY: watch
watch: $(BAZEL) $(IBAZEL)
	$(IBAZEL) -bazel_path $(BAZEL) test $(BAZEL_CONFIG) //...

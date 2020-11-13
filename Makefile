.SUFFIXES:
Makefile:;

BAZEL_BINDIR := bazel-bin
BAZEL_CONFIG :=
BINDIR := bin
BUILDDIR := .build
VERSION_BAZEL := 2.2.0
VERSION_IBAZEL := 0.12.3

PACKAGE_JSON := package.json
STACKAGE_SNAPSHOT := snapshot.yaml
STACKAGE_SNAPSHOT_PINNED := stackage_snapshot.json
WORKSPACE := WORKSPACE.bazel

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

$(PACKAGE_JSON): $(BAZEL) $(STACKAGE_SNAPSHOT_PINNED)
	$(info Generating $@ file)
	$(BAZEL) build //ci/npm:package.json
	cp $(BAZEL_BINDIR)/ci/npm/package.json $@

$(STACKAGE_SNAPSHOT_PINNED): $(BAZEL) $(STACKAGE_SNAPSHOT) $(WORKSPACE)
	$(info Generating $@ file)
	$(BAZEL) run @stackage-unpinned//:pin

.PHONY: clean
clean:
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)
	$(info Removing $(PACKAGE_JSON))
	@rm -f $(PACKAGE_JSON)
	@git clean -X --force $(BINDIR)/*

.PHONY: coverage
coverage: $(BAZEL) $(STACKAGE_SNAPSHOT_PINNED)
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
lint: $(BAZEL) $(STACKAGE_SNAPSHOT_PINNED)
	$(BAZEL) test $(BAZEL_CONFIG) //:lint

.PHONY: npm-publish
npm-publish: $(PACKAGE_JSON)
	$(info Publishing to npm)
	npm publish

.PHONY: pin-stackage
pin-stackage: $(STACKAGE_SNAPSHOT_PINNED)

.PHONY: test
test: $(BAZEL) $(STACKAGE_SNAPSHOT_PINNED)
	$(BAZEL) test $(BAZEL_CONFIG) //...

.PHONY: watch
watch: $(BAZEL) $(IBAZEL) $(STACKAGE_SNAPSHOT_PINNED)
	$(IBAZEL) -bazel_path $(BAZEL) test $(BAZEL_CONFIG) //...

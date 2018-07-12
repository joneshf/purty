# Changelog

## 2.0.1

Internal changes to the code base. Feature-wise, everything should be the same.

### Additions

### Changes

* [Cleanup most of the logic](https://gitlab.com/joneshf/purty/merge_requests/64)
* [Switch over to extensible effects](https://gitlab.com/joneshf/purty/merge_requests/65)
* [Clean up effects more](https://gitlab.com/joneshf/purty/merge_requests/66)

### Deletions

## 2.0.0

Lots more bug fixes.

Thanks to everyone who reported them. Keep 'em coming!

### Additions

### Changes

* Fix [Quoted key names unquoted incorrectly](https://gitlab.com/joneshf/purty/issues/55)
* Fix [List pattern matching in case statements breaks](https://gitlab.com/joneshf/purty/issues/56)
* Fix [Syntax error with `if` expression in `do`](https://gitlab.com/joneshf/purty/issues/58)
* Fix [Infix expressions can lose their back-ticks](https://gitlab.com/joneshf/purty/issues/60)

### Deletions

## 1.0.1

Minor bug fix.

A fix for boolean literals.

### Additions

### Changes

* [fix printing of booleans](https://gitlab.com/joneshf/purty/issues/53)

### Deletions

## 1.0.0

Official release

It's finally here! The 1.0.0 release!

Let's talk about what 1.0.0 **doesn't** mean: `purty` is now "done," `purty` has now become "stable," `purty` is now "production ready," `purty` generates "nice looking code."
* `purty` will probably never be "done." A project like this is almost never "done." There will always be some improvement to make, a bug to fix, or a syntax to update. Just as languages continue to evolve, so do do styles of those languages.
* `purty` will not be stable for quite some time. There are many new features in the future and most of these will alter the way modules are formatted.
* `purty` has been "production ready" since about the 0.3.0 or 0.4.0 release. There are still bugs, there are still improvements to make for the formatting. But, running `purty` should not "eat your code", change the semantics of a module, or break in an otherwise bad way.
* Generating "nice looking code" is entirely subjective. There's probably a consensus around what most people would consider nice, but there's no way to put a stamp on things and say `purty` generates "nice looking code."

1.0.0 is only the beginning. `purty` does enough now to be "useful." It's no longer a proof of concept, it's a useful tool. It will still have bugs, they will still get fixed. It will still have improvements.

This version also supports PureScript 0.12.0. It _should_ be backwards compatible with PureScript 0.11.7, but there are no guarantees.

### Additions

### Changes

* [Support `where` clauses](https://gitlab.com/joneshf/purty/issues/20)

### Deletions

## 0.6.0

Tiny release

One small addition for configuration.

### Additions

* [Provide a default configuration](https://gitlab.com/joneshf/purty/issues/46)

### Changes

### Deletions

## 0.5.0

Small bug fixes

Not much happened in this release. There were a couple of bug fixes and a change to import sorting.

### Additions

### Changes

* [`Char`s are not printed properly](https://gitlab.com/joneshf/purty/issues/45)
* [Let bindings with type signatures print incorrectly](https://gitlab.com/joneshf/purty/issues/41)
* [Sort imported values in import lists](https://gitlab.com/joneshf/purty/issues/44)

### Deletions

## 0.4.0

Configuration and customization

This release had only a few additions, but they were big! 

The first allows selecting between dynamic and static printing. Dynamic printing is what most people think of when they think of a pretty printer. Static printing is similar to how `gofmt` and similar tools work.

The second allows configuring `purty` with a file instead of command line arguments. That should allow for more reproducability and ease of use in the future. We're using [Dhall](https://dhall-lang.org) as the configuration language because it should make working with the file easier in the long run.

### Additions

* [Support dynamic and static printing](https://gitlab.com/joneshf/purty/issues/10)
* [Support configuration via Dhall](https://gitlab.com/joneshf/purty/issues/18)

### Changes

* [Travis webhook busted](https://gitlab.com/joneshf/purty/issues/38)
* [Distinguish between infix and prefix versions of operators](https://gitlab.com/joneshf/purty/issues/36)

### Deletions

## 0.3.0

Bug fixes and small features

Most of what happened in this release were bug fixes after being used in a real project.

### Additions

* [Document how to install `purty`](https://gitlab.com/joneshf/purty/issues/34)

### Changes

* [Fix printing of open records](https://gitlab.com/joneshf/purty/issues/30)
* [Handle paths that contain `..`](https://gitlab.com/joneshf/purty/issues/33)
* [Sort import declarations](https://gitlab.com/joneshf/purty/issues/19)
* [Print record puns when possible](https://gitlab.com/joneshf/purty/issues/32)

### Deletions

## 0.2.0

Infrastructure changes

Most of this release was around making the infrastructure of creating `purty`. We had a few features and fixes, but mostly infrastructure.

### Additions

* [Run stylish-haskell on CI](https://gitlab.com/joneshf/purty/issues/9)
* [Pretty print files in place](https://gitlab.com/joneshf/purty/issues/12)
* [Run hlint on CI](https://gitlab.com/joneshf/purty/issues/15)
* [Release `purty` as an NPM package](https://gitlab.com/joneshf/purty/issues/22)

### Changes

* [Return non-zero exit code on parse errors](https://gitlab.com/joneshf/purty/issues/13)
* [Make acceptance tests easier to work with](https://gitlab.com/joneshf/purty/issues/16)
* [Run GitLab CI in one stage](https://gitlab.com/joneshf/purty/merge_requests/22)
* [Remove unnecessary where clause in type class and instance declarations](https://gitlab.com/joneshf/purty/issues/24)
* [Fix Travis CI reporting to pipeline](https://gitlab.com/joneshf/purty/issues/25)
* [Report Travis CI starting on pipeline](https://gitlab.com/joneshf/purty/merge_requests/28)

### Deletions

* [Stop released binaries generating coverage information](https://gitlab.com/joneshf/purty/issues/26)

## 0.1.0

Initial release

### Additions

* Pretty prints PureScript modules
* [Verbose mode](https://gitlab.com/joneshf/purty/merge_requests/7) prints debug logs
* Automatically creates [Linux binary](https://gitlab.com/joneshf/purty/issues/3)
* Automatically creates [OSX binary](https://gitlab.com/joneshf/purty/issues/4)
* Automatically creates [Windows binary](https://gitlab.com/joneshf/purty/issues/5)

### Changes

### Deletions

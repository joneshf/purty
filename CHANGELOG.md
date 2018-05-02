# Changelog

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

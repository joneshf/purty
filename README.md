# purty
[![pipeline status](https://gitlab.com/joneshf/purty/badges/master/pipeline.svg)](https://gitlab.com/joneshf/purty/commits/master)
[![Build Status](https://travis-ci.org/joneshf/purty.svg?branch=master)](https://travis-ci.org/joneshf/purty)
[![Build status](https://ci.appveyor.com/api/projects/status/x1882rn32ggamxuu?svg=true)](https://ci.appveyor.com/project/joneshf/purty)
[![coverage report](https://gitlab.com/joneshf/purty/badges/master/coverage.svg)](https://gitlab.com/joneshf/purty/commits/master)
[![Download](https://api.bintray.com/packages/joneshf/generic/purty/images/download.svg)](https://bintray.com/joneshf/generic/purty/_latestVersion)

## What is it?

A WIP pretty printer for PureScript modules.

It's not all that pretty just yet.

## Installation

### npm

You can install the [npm package](https://www.npmjs.com/package/purty)

```sh
npm install purty
```

### Precompiled Binary

Binaries are available from [Bintray](https://bintray.com/joneshf/generic/purty/_latestVersion#files)

## Usage

You can pretty print a file by invoking `purty` with the path to the file.

For instance, if you had a `Main` module in a `src` directory, you could say:

```sh
purty src/Main.purs
```

A listing of all available options can be shown with the `--help` option

```sh
purty --help
```

## Configuration

You can configure most options with a [Dhall][] file.

The file must be named `.purty.dhall` and it must be in the directory where you're invoking `purty`.

Since the configuration is a [Dhall][] file, all of the guarantees and power of [Dhall][] is available.
You can reference any other [Dhall][] file on the internet, you can compute the values, you can type the configuration, etc.

For more information about [Dhall][] and what it provides, see the [Dhall tutorial][].

## How does it print?

For examples of how `purty` prints, see the [golden tests][].

[Dhall]: https://dhall-lang.org
[Dhall tutorial]: https://hackage.haskell.org/package/dhall-1.13.1/docs/Dhall-Tutorial.html
[golden tests]: ./test/golden/files

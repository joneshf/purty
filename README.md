# purty
[![pipeline status](https://gitlab.com/joneshf/purty/badges/master/pipeline.svg)](https://gitlab.com/joneshf/purty/commits/master)
[![Build Status](https://travis-ci.org/joneshf/purty.svg?branch=master)](https://travis-ci.org/joneshf/purty)
[![Build status](https://ci.appveyor.com/api/projects/status/x1882rn32ggamxuu?svg=true)](https://ci.appveyor.com/project/joneshf/purty)
[![coverage report](https://gitlab.com/joneshf/purty/badges/master/coverage.svg)](https://gitlab.com/joneshf/purty/commits/master)
[![Download](https://api.bintray.com/packages/joneshf/generic/purty/images/download.svg)](https://bintray.com/joneshf/generic/purty/_latestVersion)

## What is it?

A source code formatter for PureScript.

## Installation

### npm

You can install the [npm package](https://www.npmjs.com/package/purty)

```sh
npm install purty
```

### Precompiled Binary

Binaries are available from [Bintray](https://bintray.com/joneshf/generic/purty/_latestVersion#files)

## Usage

You can format a file by invoking `purty` with the path to the file.

For instance, if you had a `Main` module in a `src` directory, you could say:

```sh
purty src/Main.purs
```

This will write the formatted module to STDOUT.
If you'd like to format the module and write it back to the same file, you can use the `--write` option:

```sh
purty --write src/Main.purs
```

This will write the formatted module to `src/Main.purs`.

A listing of all available options can be shown with the `--help` option

```sh
purty --help
```

## How does it format?

For examples of how `purty` formats, see the [golden tests][].

[golden tests]: ./test/golden/files

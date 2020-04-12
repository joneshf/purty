# purs-format

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

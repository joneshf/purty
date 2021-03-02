# Changelog

## Unreleased

### Additions

### Changes

### Deletions

## 7.0.0

This is the first release that supports the 0.14.0 compiler and its syntactical changes.

Overall, not much has changed.
The new syntactic constructs are kind signatures and roles.
Kind signatures are formatted like type signatures.
Roles are are all on a single line for now.

This release will likely still work for most 0.13.x code.
Though there's no explicit support for the 0.13.x series at the moment.
The breaking changes are for some edge cases that it doesn't seem like many would run into,
but they're breaking for a reason.

### Additions

### Changes

* [Support 0.14.0](https://gitlab.com/joneshf/purty/-/issues/234)
* [Use POSIX-compatible flags in bash scripts](https://gitlab.com/joneshf/purty/-/issues/235)

### Deletions

## 6.3.1

This is a small release with all internal-facing changes.

This is mostly updating dependencies and fixing some CI issues.
There should be no behavioral change from the previous version.

### Additions

### Changes

* [Fix AppVeyor image again](https://gitlab.com/joneshf/purty/-/issues/228)
* [Change formatting goals/non-goals to have links](https://gitlab.com/joneshf/purty/-/issues/227)
* [Update dependencies](https://gitlab.com/joneshf/purty/-/issues/218)
* [Update even more dependencies](https://gitlab.com/joneshf/purty/-/issues/229)

### Deletions

## 6.3.0

This is a small release to address the formatting of nested conditionals.

### Additions

### Changes

* [Fix Chained if statments indent every time](https://gitlab.com/joneshf/purty/-/issues/207)
* [Fix AppVeyor image](https://gitlab.com/joneshf/purty/-/issues/219)

### Deletions

## 6.2.1

This is a small release with internal project changes.

### Additions

### Changes

* [Use `ormolu` binary](https://gitlab.com/joneshf/purty/-/issues/216)

### Deletions

## 6.2.0

This is a bug fix release to address a long standing bug.

Trailing comments in a module have been formatted incorrectly since 4.0.0.
It was largely tangled up in all trailing comments being formatted incorrectly since 4.0.0.
We took some time to dig in further and found a way to address at least this one part.

### Additions

### Changes

* [Fix trailing module comments](https://gitlab.com/joneshf/purty/-/issues/143)

### Deletions

## 6.1.3

Another small release.
More internal project changes.

### Additions

### Changes

* [Fix `prepack.sh` script](https://gitlab.com/joneshf/purty/issues/191)
* [Split build into packages](https://gitlab.com/joneshf/purty/issues/192)

### Deletions

## 6.1.2

Another small release.
This should all be internal project changes.

### Additions

### Changes

* [Put version under `bazel`s control](https://gitlab.com/joneshf/purty/issues/184)
* [Move more tests to bazel](https://gitlab.com/joneshf/purty/issues/186)
* [Remove platform handling in `make`](https://gitlab.com/joneshf/purty/issues/187)
* [Fix `make package.json` rule](https://gitlab.com/joneshf/purty/issues/188)

### Deletions

## 6.1.1

Another small release.
This should all be internal project changes related to using `bazel` as the bulk of the build system.

### Additions

### Changes

* [Use bazel for majority of the build system](https://gitlab.com/joneshf/purty/issues/164)

### Deletions

## 6.1.0

This is a small release to fix an oddity of formatting.

### Additions

### Changes

* [Remove erroneous whitespace in empty open row](https://gitlab.com/joneshf/purty/issues/181)

### Deletions

## 6.0.2

This is a bugfix due to recent changes.
We were accidentally dropping code coverage information whenever `purty` was run.
That should be taken care of, now.

### Additions

### Changes

* [Stop dumping `purty.tix` files in the cwd](https://gitlab.com/joneshf/purty/issues/178)

### Deletions

## 6.0.1

This release is all internal project changes.

### Additions

### Changes

* [Start moving the build to bazel](https://gitlab.com/joneshf/purty/issues/164)
* [Move tests to bazel](https://gitlab.com/joneshf/purty/issues/164)
* [Fix Bintray deploy on AppVeyor](https://gitlab.com/joneshf/purty/issues/177)

### Deletions

## 6.0.0

This release smooths out some rough edges.

First, we added a `version` mode so you can know which version of `purty` you're using.
It's pretty impressive tha we went this long without it, but it wasn't intentional.
In any case, by default it outputs a verbose, human-readable version information.
There is also a `--numeric` flag if you need a more machine-friendly version.

Next, we added a `validate` mode so you can check whether a file is formatted correctly.
This mode is useful for using `purty` as a lint step.
We hope to expand this mode with more useful output (like a diff of what's unformatted).

Finally, we added a `format` mode so you can format a file or directory.
This works exactly like using `purty` without the `format` mode.
I.e. `purty foo` is exactly the same as `purty format foo`.

Because these two new modes–`validate` and `version`–act as commands on the `purty` binary, it's entirely possible that they might shadow existing files or directories that would have been formatted before.
E.g. if you had a directory named `version` with PureScript files in it, it would no longer be formatted when you said `purty version --write`.
To address this breaking change, we added a `format` mode to bring back the ability to format any file or directory shadowed by a mode the `purty` binary uses.
This even works if there's a file or directory named `format` you'd like to format: `purty format format`.

### Additions

* [Add a `version` mode](https://gitlab.com/joneshf/purty/issues/169)
* [Add a `validate` mode](https://gitlab.com/joneshf/purty/issues/17)

### Changes

* [Install `hlint` binary](https://gitlab.com/joneshf/purty/issues/173)

### Deletions

## 5.0.1

This is a smaller release.
All of the changes here should be internal to the project.

### Additions

### Changes

* [Switch from `stylish-haskell` to `ormolu`](https://gitlab.com/joneshf/purty/issues/157)
* [Update `hlint`](https://gitlab.com/joneshf/purty/issues/163)
* [Drop `path-io` in exchange for `pathwalk`](https://gitlab.com/joneshf/purty/issues/165)
* [Move language extensions to the files](https://gitlab.com/joneshf/purty/issues/166)
* [Clean up argument handling](https://gitlab.com/joneshf/purty/issues/170)
* [Validate with `ormolu`](https://gitlab.com/joneshf/purty/issues/171)

### Deletions

## 5.0.0

This is a big one! There are three important differences from previous verions of `purty:

1. Configuration files are no longer supported.
    The configuration never panned out how it was supposed to.
    It was never meant to be for altering formatting, but for configuring the CLI.
    It was also meant to be an example of using Dhall.

1. Binaries are statically linked.
    The binaries we were creating up until now have been dynamically linked.
    We were privvy to the same issues that the PureScript compiler runs into from time to time (runtime libraries not existing).
    We decide to link binaries statically, so we can mitigate runtime issues.
    We also can do more interesting things (like creating minimal Docker images) with statically linked binaries.

1. Directories are supported.
    If you've used `purty` on any non-trivial codebase, you've probably run into an issue with trying to format multiple files.
    Up until now, you've had to script your own way to run all of these files.
    Depending on how you run `purty`, this could be a slow and tedious process.
    Now we support formatting all `.purs` recursively in a directory.
    This should make it easier to use `purty` in larger projects.

There were some other minor changes for dependencies in this release as well.

### Additions

* [Support directories](https://gitlab.com/joneshf/purty/issues/11)

### Changes

* [Update `purescript` to 0.13.6](https://gitlab.com/joneshf/purty/issues/153)
* [Depend on exact `purescript` modules](https://gitlab.com/joneshf/purty/issues/154)
* [Use minimal PureScript parser package](https://gitlab.com/joneshf/purty/issues/156)
* [Update `rio`](https://gitlab.com/joneshf/purty/issues/158)
* [Compile static binaries](https://gitlab.com/joneshf/purty/issues/80)
* [Fix Bintray deploy on TravisCI](https://gitlab.com/joneshf/purty/issues/161)
* [Fix uploaded filename for Bintray deployment](https://gitlab.com/joneshf/purty/issues/162)

### Deletions

* [Drop configuration](https://gitlab.com/joneshf/purty/issues/159)

## 4.6.0

There's been a process change.
We're not accepting external merge requests for the time being.

This decision has been a long time coming, and it's almost entirely an internally motivated change.
This long and short is that I'm burned out from open source work, and would like to try a different approach to developing this project.
There's more details in the [issue](https://gitlab.com/joneshf/purty/issues/151).

In other news, we've got a couple of small fixes to formatting.

### Additions

### Changes

* [Fix row kind formatting](https://gitlab.com/joneshf/purty/issues/139)
* [Fix indentation in record values](https://gitlab.com/joneshf/purty/issues/142)
* [Disable external merge requests for now](https://gitlab.com/joneshf/purty/issues/151)

### Deletions

## 4.5.2

We've started working on the emacs integration.
So far, there's a elisp file that allows formatting on save.
In the coming releases, we hope to make installing and using it easier.

We're not officially supporting the emacs integration, but we're starting work on it!

### Additions

* [Add `purty-format` Emacs minor-mode](https://gitlab.com/joneshf/purty/merge_requests/124)
* [Run CI for !124](https://gitlab.com/joneshf/purty/merge_requests/132)

### Changes

### Deletions

## 4.5.1

Mostly infrastructure changes.
There should be no breaking changes or formatting changes in this version

### Additions

* [Create a Makefile](https://gitlab.com/joneshf/purty/issues/90)

### Changes

* [Update copy in the README](https://gitlab.com/joneshf/purty/merge_requests/129)

### Deletions

## 4.5.0

A couple of formatting changes and a bit of documentation.
The formatting changes should make the output a bit more compatible with compilers older than 0.13.0.

### Additions

* [Document the factors behind formatting](https://gitlab.com/joneshf/purty/issues/113)

### Changes

* [Fix conditional formatting](https://gitlab.com/joneshf/purty/issues/136)
* [Fix instance head formating](https://gitlab.com/joneshf/purty/merge_requests/126)

### Deletions

## 4.4.2

A bug fix for the formatting of unicode and escape characters.

### Additions

### Changes

* [Fix emoji and escaped characters](https://gitlab.com/joneshf/purty/issues/118)

### Deletions

## 4.4.1

A bug fix for the formatting of multi-line renamed imports.

### Additions

### Changes

* [Fix formatting of multiline renamed imports](https://gitlab.com/joneshf/purty/issues/131)

### Deletions

## 4.4.0

A fix to the formatting of type annotations.

### Additions

### Changes

* [Fix spaces in type annotations](https://gitlab.com/joneshf/purty/issues/128)

### Deletions

## 4.3.0

A fix to the formatting of parenthesized expressions.

### Additions

### Changes

* [Use `install-stack.sh` in GitLab CI](https://gitlab.com/joneshf/purty/merge_requests/115)
* [Indent multiline parenthesized expressions properly](https://gitlab.com/joneshf/purty/issues/125)

### Deletions

## 4.2.0

Small change to display the file name when reporting errors.

### Additions

* [Display file name in errors](https://gitlab.com/joneshf/purty/issues/121)

### Changes

* [Lock down version of `stack` for GitLab CI](https://gitlab.com/joneshf/purty/merge_requests/113)

### Deletions

## 4.1.1

Quick bug fix to get STDIN working over the npm interface.

### Additions

### Changes

* [Fix STDIN for npm package](https://gitlab.com/joneshf/purty/issues/115)

### Deletions

## 4.1.0

Some cleanup to formatting.
Thanks for all the feedback from the last big release.

We also add support for reading over STDIN.
If the binary is invoked with `-`, it will read data over STDIN instead of expecting a file.
E.g. `purty -` will read input over STDIN.
Hopefully, this is useful for other tools and integrations to use.

### Additions

* [Support STDIN](https://gitlab.com/joneshf/purty/issues/43)

### Changes

* [Fix record indentation](https://gitlab.com/joneshf/purty/issues/104)
* [Cleanup the program runners](https://gitlab.com/joneshf/purty/merge_requests/102)
* [Preserve span in application of expressions](https://gitlab.com/joneshf/purty/issues/106)
* [Insert spaces around all arrays and records](https://gitlab.com/joneshf/purty/issues/109)
* [Revert changes to conditional formatting](https://gitlab.com/joneshf/purty/issues/108)

### Deletions

## 4.0.1

A quick bug fix for formatting string literals.

### Additions

### Changes

* [Fix newlines in string literals](https://gitlab.com/joneshf/purty/issues/101)

### Deletions

## 4.0.0

First release in a while. We've moved over to interactive formatting!

We're now based off of the 0.13.x version of the `purescript` package.
That means we've got a parser that gives us as close to source code as possible.
Using the newer version, we can format a module much more carefully.

Interactive formatting is something that `elm-format` does.
It's not clear if there's an official name for it.
It could also be called VonderHaar-style formatting (after the author of `elm-format`).
The way it works is by taking hints from the structure of the file to direct the formatting.
If you have some syntactic construct that could be put on one line,
and there are no line breaks in the construct, `purty` will format it on one line.
If you have some syntactic construct that could be put on one line,
and there is at least one line break in the construct,
`purty` will format the entire construct as multiple lines.
The formatting does not reach beneath the construct but may reach above it.

For example:
```PureScript
foo = [[1, 2, 3], [4,
  5, 6, 7]]
```

would be formatted as:
```PureScript
foo =
  [ [1, 2, 3]
  , [ 4
    , 5
    , 6
    , 7
    ]
  ]
```

The array `[1, 2, 3]` was formatted on a single line,
since it was initially all on one line.
The array `[4, 5, 6, 7]` was formatted over multiple lines,
as it had at least one line break.
Finally, the overall array `[[1, 2, 3], [4, 5, 6, 7]]` was formatted over multiple lines,
as it also had at least one line break.

The long and short of the changes here are that the control of formatting is given more to the author of the code.
This should still keep with the idea of formatting that works nicely,
but it should also give a bit of freedom to people to structure the code a little bit differently if need be.

Bear in mind, almost all of the code is re-written so it's likely to contain a new set of bugs.
Please feel free to report any bugs you find.

### Additions

* [Support interactive formatting](https://gitlab.com/joneshf/purty/issues/39)

### Changes

* [Update stack to 1.9.3](https://gitlab.com/joneshf/purty/merge_requests/93)
* [Remove duplicate comments in most expressions](https://gitlab.com/joneshf/purty/issues/61)
* [Format multi-line guards better](https://gitlab.com/joneshf/purty/issues/78)
* [Retain comments at end of file](https://gitlab.com/joneshf/purty/issues/94)
* [Retail lines over infix values](https://gitlab.com/joneshf/purty/issues/95)
* [Retain comments in data declarations](https://gitlab.com/joneshf/purty/issues/96)
* [Fix file path handling](https://gitlab.com/joneshf/purty/issues/98)
* [Fix derived instance formatting](https://gitlab.com/joneshf/purty/issues/99)
* [Add some more tests](https://gitlab.com/joneshf/purty/merge_requests/94)

### Deletions

* [Drop support for dynamic/static formatting](https://gitlab.com/joneshf/purty/issues/39)

## 3.0.7

Re-publishing to npm. There was a problem with the previous version.

There shouldn't be any changes with `purty` itself.

### Additions

### Changes

### Deletions

## 3.0.6

Internal code cleanup. There shouldn't be any changes with `purty` itself.

### Additions

### Changes

* [Prefer `Member` over `Members`](https://gitlab.com/joneshf/purty/merge_requests/86)
* [Cleanup golden test suite](https://gitlab.com/joneshf/purty/merge_requests/87)

### Deletions

## 3.0.5

Patch release with a bug fix.

### Additions

### Changes

* [ado might not have statements](https://gitlab.com/joneshf/purty/issues/83)

### Deletions

## 3.0.4

Patch release with bug fixes.

### Additions

### Changes

* [Functional dependency indexes are wrong](https://gitlab.com/joneshf/purty/issues/84)
* [Allow building just the dependencies only in Travis](https://gitlab.com/joneshf/purty/merge_requests/82)

### Deletions

## 3.0.3

Patch release with CI changes.

### Additions

### Changes

* [Publish Bintray artifact immediately from AppVeyor](https://gitlab.com/joneshf/purty/issues/79)

### Deletions

## 3.0.2

Squashing another boug from 3.0.0 changes.

This one was record puns not being formatted correctly.
There were also some infrastructure changes.

### Additions

### Changes

* [Automate more stuff in CI](https://gitlab.com/joneshf/purty/issues/65)
* [Re-add support for record literal puns in expressions](https://gitlab.com/joneshf/purty/issues/72)
* [Update release documentation](https://gitlab.com/joneshf/purty/merge_requests/76)

### Deletions

## 3.0.1

Those huge changes from 3.0.0 contained a few bugs :).

This is a quick patch release to address them. If there are more, please report them!

### Additions

### Changes

* [Add missing npm file](https://gitlab.com/joneshf/purty/issues/66)
* [Fix printing of functions in parentheses](https://gitlab.com/joneshf/purty/issues/67)
* [Support bound value declarations](https://gitlab.com/joneshf/purty/issues/68)
* [Print top level comments](https://gitlab.com/joneshf/purty/issues/69)

### Deletions

## 3.0.0

Huge changes to the internals. We've got our own AST! Now, we can format things a bit purty-er.

It's already made some things easier and removed some bugs with the formatting. It should make other formatting easier and also will make implementing [interactive formatting](https://gitlab.com/joneshf/purty/issues/39) much easier.

### Additions

### Changes

* [Print nested lambdas with a single lambda expression](https://gitlab.com/joneshf/purty/issues/31)
* [Make a pretty print AST](https://gitlab.com/joneshf/purty/issues/35)
* [Remove extra newlines in let bindings](https://gitlab.com/joneshf/purty/issues/49)
* Fix [Trailing spaces in output](https://gitlab.com/joneshf/purty/issues/54)

### Deletions

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

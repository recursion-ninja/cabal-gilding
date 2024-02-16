# Gild

[![Workflow](https://github.com/tfausak/cabal-gild/actions/workflows/workflow.yaml/badge.svg)](https://github.com/tfausak/cabal-gild/actions/workflows/workflow.yaml)
[![Hackage](https://img.shields.io/hackage/v/cabal-gild)](https://hackage.haskell.org/package/cabal-gild)
[![Stackage](https://www.stackage.org/package/cabal-gild/badge/nightly?label=stackage)](https://www.stackage.org/package/cabal-gild)

Gild is an opinionated command line utility that formats Haskell package
descriptions, which are also known as `*.cabal` files.

To run Gild in CI, consider using [cabal-gild-setup-action][].

[cabal-gild-setup-action]: https://github.com/tfausak/cabal-gild-setup-action

Gild started as a fork of [phadej/cabal-fmt][] by, but is now totally separate.

[phadej/cabal-fmt]: https://github.com/phadej/cabal-fmt

## Summary

Given a package description like this:

``` cabal
CABAL-VERSION : 3.0
name          : example
version       : 0.0.0.0

library
  build-depends: mtl>=2.3, base
  ghc-options:-Wall
  if impl(ghc>=9.8)
    ghc-options: -Wmissing-role-annotations
```

Gild will produce output like this:

``` cabal
cabal-version: 3.0
name: example
version: 0.0.0.0

library
  build-depends:
    base,
    mtl >=2.3,

  ghc-options: -Wall

  if impl(ghc >= 9.8)
    ghc-options: -Wmissing-role-annotations
```

See the [installation][] section for how to get Gild and the [usage][] section
for how to use it.

[installation]: #installation
[usage]: #usage

## Goals

- There should be no configuration options. As long as the output format is
  reasonable, the specifics should not matter too much. This means the amount
  of indentation cannot be changed, for example.

- The output should be diff friendly. This means things generally go on their
  own line, trailing commas are used when possible, and elements are sorted
  where it makes sense.

- The output should be semantically the same as the input. This means no
  normalization or canonicalization. For example, separate `build-depends`
  fields are not merged together.

- It should be possible to format other files that use the same format as
  package descriptions. This means `cabal.project` files can be formatted as
  well.

- The focus is on formatting only. There should be no effort made to validate
  input or provide any checks or lints.

- The command line utility should be fast enough to run on every save. It
  should not need network access.

## Installation

Go to the [latest release][] page and download the binary for your platform.

[latest release]: https://github.com/tfausak/cabal-gild/releases/latest

### From Source

In general you should prefer downloading the appropriate binary for you
platform. However it is possible to build Gild from source. It supports Linux,
macOS, and Windows along with the three most recent versions of GHC. Any other
configurations are unsupported.

With Cabal:

``` sh
$ cabal install cabal-gild
```

With Stack:

``` sh
$ stack install cabal-gild
```

## Usage

Gild is a command line utility named `cabal-gild`. By default it reads from
standard input (STDIN) and writes to standard output (STDOUT). Its behavior can
be modified with command line options, which are described below.

### Options

Run `cabal-gild --help` to see the options that Gild supports. They are:

- `--help`: Prints the help message to STDOUT then exits successfully.

- `--version`: Prints the version number to STDOUT then exits successfully.

- `--input=FILE`: Uses `FILE` as the input. If this is `-` (which is the
  default), then the input will be read from STDIN.

- `--mode=MODE`: Sets the mode to `MODE`, which must be either `format` (the
  default) or `check`. When the mode is `format`, Gild will output the
  formatted package description. When the mode is `check`, Gild will exit
  successfully if the input is already formatted, otherwise it will exit
  unsuccessfully.

- `--output=FILE`: Uses `FILE` as the output. If this is `-` (which is the
  default), then the output will be written to STDOUT. To modify a file in
  place, use the same file as both input and output. For example:

  ``` sh
  $ cabal-gild --input p.cabal --output p.cabal
  ```

  This option is ignored when the mode is `check`.

- `--stdin=FILE`: When reading input from STDIN, use `FILE` as the effective
  input file. This is useful when a file's contents are already available, like
  in an editor. For example:

  ``` sh
  $ cabal-gild --stdin p.cabal < p.cabal
  ```

  This option is ignored when the input is not `-`.

### Pragmas

Gild supports special comments in package descriptions that act as pragmas.
Each pragma starts with `-- cabal-gild:`. Pragmas must be the last comment
before a field.

- `-- cabal-gild: discover DIRECTORY`: This pragma will discover any Haskell
  files (`*.hs`) in the given directory and use those to populate the list of
  modules. For example, given this input:

  ``` cabal
  library
    -- cabal-gild: discover source/library
    exposed-modules: ...
  ```

  Assuming there is a single Haskell file at `source/library/M.hs`, Gild will
  produce this output:

  ``` cabal
  library
    -- cabal-gild: discover source/library
    exposed-modules: M
  ```

  This pragma only works with the `exposed-modules` and `other-modules` fields.
  It will be ignored on all other fields.

  Any existing modules in the list will be ignored. The entire field will be
  replaced. This means adding, removing, and renaming modules should be handled
  automatically.

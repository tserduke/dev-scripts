# dev-scripts
Scripts for support of Haskell projects development


## Requirements
* [Mercurial] as version control system. I use the [Hg-Git] plugin for seamless integration with Git.
* [Stack]
* [Stackage] >= 7.0 for [`hackage-docs`](#hackage-docs)


## Installation
```shell
hg clone https://github.com/tserduke/dev-scripts.git
cd dev-scripts
stack install
```


## Usage:
### Executable
```shell
cd some-project
# Given ~/.local/bin is on the PATH
dev lint # run lint on current project
```

### Library
```shell
cd some-project
# Given dev-scripts and some-project are using the same Stack resolver
stack ghci --no-package-hiding
```
```haskell
import Development.Scripts
lint -- run lint on current project
```


## Commands
### lint
Lints all Haskell sources it could find.

1. Looks for all packages listed in `stack.yaml` in simple format.
2. Creates a directory list from all `hs-source-dirs` fields in found cabal files.
3. Calls [HLint] via stack on constructed directory list.

### build
Builds all projects, tests, benchmarks and documentation failing on warnings and runs tests.
```shell
stack clean
stack build --test --bench --ghc-options "-Werror" --no-run-benchmarks
# Build documentation for the root package only without going into full rebuild:
stack build --haddock --ghc-options "-Werror" package-name
```
First build with `--haddock` may take some time as it goes through all installed packages!

### check-changelog
Checks `changelog.md`.

1. Checks the latest version number equals the one in the root cabal file.
2. Checks the date of the latest version is current.

### hackage-docs
Uploads locally generated documentation to Hackage for the root package. Port of the following scripts - [1][hackage-docs-1] and [2][hackage-docs-2].

### publish
If all is good publishes and tags the release.

1. Checks if there are no uncommitted changes. Add `/.shake` to `.gitignore`.
1. Runs [`lint`](#lint), [`build`](#check-build) and [`check-changelog`](#check-changelog).
1. `hg clean src` - cleanup after Haddock
1. `stack sdist`
1. `stack upload .`
1. `hg tag vx.y.z` - tags the repository with current version number
1. Runs [`hackage-docs`](#hackage-docs).


[Mercurial]: https://www.mercurial-scm.org
[Hg-Git]: https://hg-git.github.io
[Stack]: https://www.haskellstack.org
[Stackage]: https://www.stackage.org/lts-7
[HLint]: https://github.com/ndmitchell/hlint#readme
[hackage-docs-1]: https://github.com/ekmett/lens/blob/67ac5db4ee24364c435e6e9fbe29fe429bce8d0c/scripts/hackage-docs.sh
[hackage-docs-2]: https://github.com/phadej/binary-orphans/blob/3f106567260c1a9bb3063d49948201675876ad12/hackage-docs.sh

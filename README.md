# dev-scripts
Scripts for support of Haskell projects development

## Requirements
The scripts assume [Mercurial] is used as version control system. I use the [Hg-Git] plugin for seamless integration with Git.

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

### publish
Checks the project has no warnings, builds fine, passes its tests and has an up-to-date changelog. If all is good publishes and tags the release.

1. Runs [lint](#lint).
1. Checks that latest version number in `changelog.md` equals the one in root cabal file.
1. Checks the date of latest version in `changelog.md`.
1. Checks there are no uncommitted files.
1. `stack clean`
1. `stack build --test --bench --haddock --ghc-options "-Werror" --no-run-benchmarks`
1. `stack dist`
1. `stack publish`
1. `hg tag vx.y.z` - tags the repository with current version number.

[Mercurial]: https://www.mercurial-scm.org
[Hg-Git]: https://hg-git.github.io
[HLint]: https://github.com/ndmitchell/hlint#readme

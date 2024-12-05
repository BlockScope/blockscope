---
title: 'Cabal Tooling Contributions'
---
A list of contributions I've made to `haskell`[**/cabal**](https://github.com/haskell/cabal):

- **Functionality**; feature additions, bug fixes, make, continuous integration
    - Render short option with arg,
      [#9043](https://github.com/haskell/cabal/pull/9043)
    - Only move code to `Simple/GHC/Build*`,
      [#9409](https://github.com/haskell/cabal/pull/9409)
    - Warn early overwrite expanded,
      [#9506](https://github.com/haskell/cabal/pull/9506)
    - Add a format rejections function,
      [#9560](https://github.com/haskell/cabal/pull/9560)
    - Add Cabal-described and cabal-install to doctests,
      [#9572](https://github.com/haskell/cabal/pull/9572)
    - Generate `doc/buildinfo-fields-reference.rst`,
      [#9573](https://github.com/haskell/cabal/pull/9573)
    - Show import tree provenance
      [#9578](https://github.com/haskell/cabal/pull/9578)
    - Show abbreviated mixed versions with suffix
      [#9824](https://github.com/haskell/cabal/pull/9824)
    - Render project configs as clean list in provenance message
      [#9985](https://github.com/haskell/cabal/pull/9985)

- **Projects**
    - Rename projects to have `.project` extension
      [#9958](https://github.com/haskell/cabal/pull/9958)
    - Use package groups
      [#9565](https://github.com/haskell/cabal/pull/9565)

- **Tests and CI**
    - Add test cases that reproduce `sdist --project-file`,
      [#8226](https://github.com/haskell/cabal/pull/8226)
    - Add reinstall test to `LinkerOptions/NonignoredConfigs`,
      [#9377](https://github.com/haskell/cabal/pull/9377)
    - Double banger test names,
      [#9526](https://github.com/haskell/cabal/pull/9526)
    - Add further tests of cyclical project imports,
      [#9665](https://github.com/haskell/cabal/pull/9665)
    - Add tests for project imports and constraint version conflicts,
      [#9680](https://github.com/haskell/cabal/pull/9680)
    - Download stackage.org/.../cabal.config locally
      [#9739](https://github.com/haskell/cabal/pull/9739)
    - Remove skipIfGhcVersion "== 9.6.3"
      [#9749](https://github.com/haskell/cabal/pull/9749)
    - Remove stale see 'withSourceCopyDir'
      [#9750](https://github.com/haskell/cabal/pull/9750)
    - Add Y-forking import project
      [#10508](https://github.com/haskell/cabal/pull/10508)
    - Add a --dry-run build check of cabal.project.release
      [#9610](https://github.com/haskell/cabal/pull/9610)
    - Text file, newlines at end of files
      [#9804](https://github.com/haskell/cabal/pull/9804)

- **Linting and Fixing Warnings**
    - Add `HLint` configuraton,
      [#9041](https://github.com/haskell/cabal/pull/9041)
    - Follow hlint suggestion: redundant list comprehension,
      [#9111](https://github.com/haskell/cabal/pull/9111)
    - Follow hlint suggestion: move brackets to avoid `$`,
      [#9112](https://github.com/haskell/cabal/pull/9112)
    - Follow hlint suggestion: redundant where (dead code),
      [#9128](https://github.com/haskell/cabal/pull/9128)
    - Have hlint ignore `CmmSourcesExe Demo`,
      [#9189](https://github.com/haskell/cabal/pull/9189)
    - Satisfy `-Werror=unused-top-binds`,
      [#9488](https://github.com/haskell/cabal/pull/9488)
    - Build `cabal-dev-scripts` with `ghc-9.8.1`,
      [#9600](https://github.com/haskell/cabal/pull/9600)
    - Don't use redundant alias Prelude as Prelude
      [#10582](https://github.com/haskell/cabal/pull/10582)
    - Bump to hlint-3.8
      [#9959](https://github.com/haskell/cabal/pull/9959)
    - Remove unused packages
      [#9855](https://github.com/haskell/cabal/pull/9855)

- **Documentation: Contributing**
    - Recommend using latest cabal-install
      [#10013](https://github.com/haskell/cabal/pull/10013)
    - Add a note for contributors installing fourmolu
      [#10473](https://github.com/haskell/cabal/pull/10473)
    - Define a user-visible change
      [#9946](https://github.com/haskell/cabal/pull/9946)

- **Documentation: Users Guide**
    - Move target forms to its own section, warning about install all,
      [#7000](https://github.com/haskell/cabal/pull/7000)
    - Warn about needing to opt in to `overwrite-policy` for install,
      [#7823](https://github.com/haskell/cabal/pull/7823)
    - Add a doc section on `cabal list-bin`,
      [#7964](https://github.com/haskell/cabal/pull/7964)
    - Describe a gitpod workflow for docs,
      [#7976](https://github.com/haskell/cabal/pull/7976)
    - Bring the command help up to date,
      [#9105](https://github.com/haskell/cabal/pull/9105)
    - Add back reference from constraints to flags,
      [#9264](https://github.com/haskell/cabal/pull/9264)
    - Note how to do "not equal" with constraints,
      [#9380](https://github.com/haskell/cabal/pull/9380)
    - Warn about project conditional blocks,
      [#9515](https://github.com/haskell/cabal/pull/9515)
    - Use `knownLanguages` for describing Language,
      [#9580](https://github.com/haskell/cabal/pull/9580)
    - Regen cabal help after #9583
      [#10545](https://github.com/haskell/cabal/pull/10545)
    - Duplicate VCS content
      [#10553](https://github.com/haskell/cabal/pull/10553)
    - Use alice as username in cabal path examples
      [#9931](https://github.com/haskell/cabal/pull/9931)
    - Hackage as a proper noun, capitalized in docs
      [#9970](https://github.com/haskell/cabal/pull/9970)
    - Add "no command line variant" for `source-repository-package`.
      [#9975](https://github.com/haskell/cabal/pull/9975)
    - Add warning and note to cabal test [TARGETS]
      [#9945](https://github.com/haskell/cabal/pull/9945)
    - Change packages default to empty
      [#10099](https://github.com/haskell/cabal/pull/10099)
    - Add warning and note about cabal init and script
      [#10327](https://github.com/haskell/cabal/pull/10327)
    - source-repository versus source-repository-package
      [#9701](https://github.com/haskell/cabal/pull/9701)
    - Warn that scripts cannot be package executables
      [#10326](https://github.com/haskell/cabal/pull/10326)

- **Ignores**
    - Ignore generated `cabal.out` and `cabal.out''`,
      [#9594](https://github.com/haskell/cabal/pull/9594)
    - Add `cabal-testsuite` git ignores for validate dangling files,
      [#9633](https://github.com/haskell/cabal/pull/9633)
    - Ignore `IntegrationTests2/config/default-config`,
      [#9644](https://github.com/haskell/cabal/pull/9644)
    - Ignore test `T7339` output, `libhello.so`,
      [#9699](https://github.com/haskell/cabal/pull/9699)
    - Ignore testdb/intree generated files
      [#9847](https://github.com/haskell/cabal/pull/9847)

- **Trivial**
    - Consistent starting capital letter, ending period in help text,
      [#7980](https://github.com/haskell/cabal/pull/7980)
    - Get clones the source repository,
      [#8088](https://github.com/haskell/cabal/pull/8088)
    - Typo missing closing parenthesis on license,
      [#8146](https://github.com/haskell/cabal/pull/8146)
    - Simplify to avoid a typo,
      [#9315](https://github.com/haskell/cabal/pull/9315)
    - Avoid double space in "Executing install plan ...",
      [#9376](https://github.com/haskell/cabal/pull/9376)
    - Use the newer haskell-actions organisation,
      [#9381](https://github.com/haskell/cabal/pull/9381)
    - Change the test expectation md5 hash,
      [#9487](https://github.com/haskell/cabal/pull/9487)
    - Delete `cabal.project.coverage`,
      [#9574](https://github.com/haskell/cabal/pull/9574)

- **Typos**
    - Typo, Warning not Waring
      [#10513](https://github.com/haskell/cabal/pull/10513)
    - Typo respositories
      [#10592](https://github.com/haskell/cabal/pull/10592)
    - Typo prexif, reseved and unmatched (
      [#10593](https://github.com/haskell/cabal/pull/10593)
    - Typo depency
      [#10599](https://github.com/haskell/cabal/pull/10599)
    - Add typo checking for *.rst and *.md files
      [#10603](https://github.com/haskell/cabal/pull/10603)

- **Makefile**
    - Add PHONY rules for lint and lint-json
      [#10584](https://github.com/haskell/cabal/pull/10584)
    - Add help target to the makefile
      [#10608](https://github.com/haskell/cabal/pull/10608)

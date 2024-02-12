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
- **Tests**
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
- **Linting** and fixing warnings
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
- **Documentation**
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
    - Ignore generated `cabal.out` and `cabal.out''`,
      [#9594](https://github.com/haskell/cabal/pull/9594)
    - Add `cabal-testsuite` git ignores for validate dangling files,
      [#9633](https://github.com/haskell/cabal/pull/9633)
    - Ignore `IntegrationTests2/config/default-config`,
      [#9644](https://github.com/haskell/cabal/pull/9644)
    - Ignore test `T7339` output, `libhello.so`,
      [#9699](https://github.com/haskell/cabal/pull/9699)
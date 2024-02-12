---
title: 'Tooling'
subtitle: A little help can go a long way.
---
What is the use of a programming language if it is a huge pain to resolve
dependencies with or to build. That's an area where a little help can go a long
way to ease the pain.

#### Build Tools
* `haskell`[**/cabal**](https://github.com/haskell/cabal)  
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
* `commercialhaskell`[**/stack**](https://docs.haskellstack.org)  
    - **Functionality**; feature additions, bug fixes, continuous integration
        - Help text allows multiple use for `--package`,
          [#5775](https://github.com/commercialhaskell/stack/pull/5775)
        - Use original `YAML` to preserve top-level order,
          [#5813](https://github.com/commercialhaskell/stack/pull/5813)
        - Add stan gh-action and ignored triggered checks,
          [#5811](https://github.com/commercialhaskell/stack/pull/5811)
        - Adds a `cabal.project` file,
          [#5860](https://github.com/commercialhaskell/stack/pull/5860)
        - Check if target might be a project-level configuration file, not a
          missing directory,
          [#6033](https://github.com/commercialhaskell/stack/pull/6033)
        - Add ide target component types,
          [#6126](https://github.com/commercialhaskell/stack/pull/6126)
        - Build _ with `ghc-x.y.z`,
          [#6344](https://github.com/commercialhaskell/stack/pull/6344)
    - **Documentation**
        - Document how to suppress the ``-nopie`` warnings on macOS,
          [#4392](https://github.com/commercialhaskell/stack/pull/4392/files)
        - Link to unit tests and integration tests,
          [#5809](https://github.com/commercialhaskell/stack/pull/5809)
    - **Trivial**
        - Ignore `dist-newstyle` folder,
          [#5776](https://github.com/commercialhaskell/stack/pull/5776)
        - Lint `YAML` especially `.hlint.yaml`,
          [#5797](https://github.com/commercialhaskell/stack/pull/5797)
        - Typo close parenthesis for work directory default,
          [#5812](https://github.com/commercialhaskell/stack/pull/5812)
        - Bump the cabal project to match `lts-20.0` in `stack.yaml`,
          [#6031](https://github.com/commercialhaskell/stack/pull/6031)
* `cabalism`[**/hpack-dhall**](http://hackage.haskell.org/package/hpack-dhall)  
I started contributing and ended up
[owner and maintainer](https://github.com/cabalism/hpack-dhall/issues/3) of
this way to setup Haskell projects.
* `judah`[**/pier**](https://github.com/judah/pier)  
I added [test-suite](https://github.com/judah/pier/issues/50) running to this
experimental way of building Haskell projects.
* `tweag`[**/rules_haskell**](https://haskell.build/)  
I helped
[document](https://github.com/tweag/rules_haskell/commits?author=philderbeast)
how to build Haskell fast with Google Bazel and rules_haskell.
* `nmattia`[**/snack**](https://github.com/nmattia/snack/commits?author=philderbeast)  
An incremental build tool for Haskell projects.

#### Packages and Dependencies
* `fsprojects`[**/paket**](https://fsprojects.github.io/Paket/)  
I helped document how to convert an existing solution to use paket,
[explaining](https://github.com/fsprojects/Paket/commit/3db8c5b8701adf345c8cf4c1b3cfcb8d4bc11fca)
some assumptions in the way the project files need to be organized on disk.
This is now
a [tutorial](https://fsprojects.github.io/Paket/convert-from-nuget-tutorial.html).
* `NixOS`[**/cabal2nix**](https://github.com/NixOS/cabal2nix)  
I made a [pull request](https://github.com/NixOS/cabal2nix/pull/375) adding
hpack-dhall packages that we agreed should not be merged as cabal2nix should
focus on `.cabal` files and drop support for other package file formats.
* `typeable`[**/stackage2nix**](https://github.com/typeable/stackage2nix)  
[Redirect](https://github.com/typeable/stackage2nix/commit/9bf94e1ded1d52feddbdbd560ecd4f9a70aa6c34)
stale links to `all-cabal-hashes` and `lts-haskell`.
* `input-output-hk`[**/stack2nix**](https://github.com/input-output-hk/stack2nix)  
[Parse](https://github.com/input-output-hk/stack2nix/commit/e01483c14ec288eeeef586c9aa31c737042bda55)
the mac operating system as `osx` or `darwin`.

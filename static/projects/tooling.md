---
title: 'Tooling'
subtitle: A little help can go a long way.
---
What is the use of a programming language if it is a huge pain to resolve
dependencies with or to build. That's an area where a little help can go a long
way to ease the pain.

#### Build Tools
* `haskell`[**/cabal**](https://github.com/haskell/cabal)  
Raised
[issues](https://github.com/haskell/cabal/issues?q=is%3Aissue+author%3Aphilderbeast)
and supplied [pull requests](https://github.com/haskell/cabal/pulls?q=author%3Aphilderbeast), such as filling gaps in the documentation for commands like
`cabal install all:exes` and `cabal list-bin`.
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

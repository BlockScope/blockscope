---
title: 'Supporting Acts'
subtitle: Contributions to projects that I use or want to succeed.
---
I donate time to open source and am setup to [receive
donations](https://github.com/sponsors/philderbeast). I'm pushing for units of
measure and functional programming.

#### Units of Measure
* `adamgundry`[**/units-parser**](https://github.com/adamgundry/units-parser)  
[Bump](https://github.com/adamgundry/units-parser/commit/9db2652bfbeea5d69f590ce15c171d7b9801bb60)
expected test output for later ghc versions.
* `adamgundry`[**/uom-plugin**](http://hackage.haskell.org/package/uom-plugin)  
A compiler plugin for units of measure.
* `bgamari`[**/the-thoralf-plugin**](https://cs.brynmawr.edu/~rae/papers/2018/thoralf/thoralf.pdf)  
After seeing a presentation on this compiler plugin at the Haskell symposium
2018,
I [fixed](https://github.com/bgamari/the-thoralf-plugin/commits?author=philderbeast)
some build warnings and got it compiling with ghc-8.6.1.
* `BlockScope`[**/plugins-for-blobs**](https://github.com/BlockScope/plugins-for-blobs)  

    > 1 blob is equal to 1 lbf⋅s2/in, or 12 [slugs][slug].

    This is an attempt to refactor the [thoralf-plugin][thoralf-plugin] and the
    [uom-plugin][uom-plugin]. Under the hood, the `thoralf-plugin` uses Z3 to solve
    equational theories. The `uom-plugin` solves trivial equations, rewrites and
    simplifies constraints and makes substitutions.

    [slug]: https://en.wikipedia.org/wiki/Slug_(unit)
    [uom-plugin]: https://github.com/adamgundry/uom-plugin
    [thoralf-plugin]: https://github.com/bgamari/the-thoralf-plugin
    [ghc-tcplugins-extra]: https://github.com/BlockScope/ghc-tcplugins-extra
    [units-parser]: https://github.com/adamgundry/units-parser

    The `uom-plugin` depends on [ghc-tcplugins-extra][ghc-tcplugins-extra] and
    [units-parser][units-parser]. It defines a quasiquoter for writing units with
    measures, such as `[u| 9.8 m/s^2 |]`.

    The `thoralf-plugin` ships with one small units example, calculating distance
    in `m` from velocity in `m/s` and time in `s`. The `uom-plugin` has a large
    number of unit tests.

    <h5>**Goals**</h5>
    * Use the unit quasiquoter with the `thoralf-plugin` and get it to pass all of
      the units tests of the `uom-plugin`.
    * Identify, refactor and extract commonality in both unit plugins.

    <h5 style="margin-top: 1em">**Progress**</h5>
    * Added `ghc-corroborate`, a new package exposing a flattened subset of GHC's
      API needed for typechecking plugins as a single API across multiple GHC
      versions. It uses cabal conditionals and mixins and avoids use of the `CPP`
      language extension and predefined macros for switching between GHC versions.
    * Forked `ghc-tcplugins-extra` to use `ghc-corroborate` and to remove its use
      of `CPP`.
    * Moved the tracing of the `thoralf-plugin` to `ghc-tcplugins-trace`.
    * Moved the quasiquoter of the `uom-plugin` to `uom-th`.
    * Moved the units of measure (UoM) theory from the `thoralf-plugin` and much of
      the `uom-plugin` internals to `uom-quantity`.
    * Pulled unit definitions out of `uom-plugin` and put these into
      `uom-plugin-defs`.
    * Rearranged the modules of each plugin for similarity between both.

#### Compilers
I've coded up solutions to the direct and indirect problems of geodetics in
Haskell, F#, Unison and Koka and in so doing have been able to find problems
with some of these compilers.

* `koka-lang`[**/koka**](http://koka-lang.org/)  
Found
[issues](https://github.com/koka-lang/koka/issues/created_by/philderbeast)
compiling
`flight-earth`[**/coriolis-effect**](https://github.com/flight-earth/coriolis-effect).
* `unisonweb`[**/unison**](https://www.unisonweb.org/)  
Found
[issues](https://github.com/unisonweb/unison/issues/created_by/philderbeast)
compiling
`flat-earth`[**/flat-earth**](https://github.com/flight-earth/flat-earth).
* `ghc`[**/ghc**](https://www.haskell.org/ghc/)  
For the newcomer labelled bug
[12441](https://gitlab.haskell.org/ghc/ghc/issues/12441), I provided a fix for
pretty printing [explicit
quantifiers](https://github.com/ghc/ghc/commit/33140f41b931fb81bf2e5aa28603fe757bb3779d).
I've reported some
[issues](https://gitlab.haskell.org/ghc/ghc/-/issues?scope=all&state=all&author_username=philderbeast)
in ghc.

#### Build Tools
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
* `commercialhaskell`[**/stack**](https://docs.haskellstack.org)  
[Document](https://github.com/commercialhaskell/stack/pull/4392/files) how to
suppress the ``-nopie`` warnings on macOS.
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

#### Backend
* `fsprojects`[**/FSharp.Linq.ComposableQuery**](http://fsprojects.github.io/FSharp.Linq.ComposableQuery/)  
[Tweak](https://github.com/fsprojects/FSharp.Linq.ComposableQuery/commit/1706939d7573fdef3e017637a264871cff1c5fcc)
the database creation script for an example and [find
a query](https://github.com/fsprojects/FSharp.Linq.ComposableQuery/issues/13)
that fails. Composable queries are now part of the
[SQLProvider](https://fsprojects.github.io/SQLProvider/core/composable.html).

#### Frontend
* `purescript-contrib`[**/purescript-unicode**](https://github.com/purescript-contrib/purescript-unicode)  
[Help](https://github.com/purescript-contrib/purescript-unicode/commit/c161145fe403e520e505e7631ce9d77b9a2de9df)
document the behaviour of `digitToInt` with examples.

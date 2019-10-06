---
title: 'Projects'
---

My activity on github is nicely summarised by <a
href="https://ghuser.io/philderbeast"
target="_blank">ghuser.io/philderbeast</a>.

### Flying High and Flying Far

I have but one major project for scoring in the sky and another for saying how
that is done.

* [**flare-timing**](http://www.flaretiming.com)  
A suite of command line apps for scoring hang gliding and paragliding
competitions and a website for publishing competitions scored this way with
comparisons to the official scores.
* [**CIVL-GAP**](https://github.com/BlockScope/CIVL-GAP)  
A mirror of the FAI-CIVL official scoring rules for hang gliding and
paragliding. Marked up in LaTeX and stored in a git repo, this is a place for
myself and others to raise questions about the rules, correct obvious mistakes,
add clarifying appendices and to suggest changes.

### Supporting Acts

I contribute to projects that I use or want to succeed.

* [**paket**](https://fsprojects.github.io/Paket/)  
I helped document how to convert an existing solution to use paket,
[explaining](https://github.com/fsprojects/Paket/commit/3db8c5b8701adf345c8cf4c1b3cfcb8d4bc11fca)
some assumptions in the way the project files need to be organized on disk.
This is now
a [tutorial](https://fsprojects.github.io/Paket/convert-from-nuget-tutorial.html).
* [**FSharp.Linq.ComposableQuery**](http://fsprojects.github.io/FSharp.Linq.ComposableQuery/)  
[Tweak](https://github.com/fsprojects/FSharp.Linq.ComposableQuery/commit/1706939d7573fdef3e017637a264871cff1c5fcc)
the database creation script for an example and [find
a query](https://github.com/fsprojects/FSharp.Linq.ComposableQuery/issues/13)
that fails. Composable queries are now part of the
[SQLProvider](https://fsprojects.github.io/SQLProvider/core/composable.html).
* [**ghc**](https://www.haskell.org/ghc/)  
For the newcomer labelled bug
[12441](https://gitlab.haskell.org/ghc/ghc/issues/12441), I provided a fix for
pretty printing [explicit
quantifiers](https://github.com/ghc/ghc/commit/33140f41b931fb81bf2e5aa28603fe757bb3779d).
* [**hpack-dhall**](http://hackage.haskell.org/package/hpack-dhall)  
I started contributing and ended up
[owner and maintainer](https://github.com/BlockScope/hpack-dhall/issues/3) of
this way to setup Haskell projects.
* [**uom-plugin**](http://hackage.haskell.org/package/uom-plugin)  
A compiler plugin for units of measure.
* [**rules_haskell**](https://haskell.build/)  
I helped
[document](https://github.com/tweag/rules_haskell/commits?author=philderbeast)
how to build Haskell fast with Google Bazel and rules_haskell.
* [**stack**](https://docs.haskellstack.org)  
[Document](https://github.com/commercialhaskell/stack/pull/4392/files) how to
suppress the ``-nopie`` warnings on macOS.
* [**pier**](https://github.com/judah/pier)  
I added [test-suite](https://github.com/judah/pier/issues/50) running to this
experimental way of building Haskell projects.
* [**snack**](https://github.com/nmattia/snack/commits?author=philderbeast)  
An incremental build tool for Haskell projects.
* [**Thoralf**](https://cs.brynmawr.edu/~rae/papers/2018/thoralf/thoralf.pdf)  
After seeing a presentation on this compiler plugin at the Haskell symposium
2018,
I [fixed](https://github.com/bgamari/the-thoralf-plugin/commits?author=philderbeast)
some build warnings and got it compiling with ghc-8.6.1.
* [**cabal2nix**](https://github.com/NixOS/cabal2nix)  
I made a [pull request](https://github.com/NixOS/cabal2nix/pull/375) adding
hpack-dhall packages that we agreed should not be merged as cabal2nix should
focus on `.cabal` files and drop support for other package file formats.
* [**stackage2nix**](https://github.com/typeable/stackage2nix)  
[Redirect](https://github.com/typeable/stackage2nix/commit/9bf94e1ded1d52feddbdbd560ecd4f9a70aa6c34)
stale links to `all-cabal-hashes` and `lts-haskell`.
* [**stack2nix**](https://github.com/input-output-hk/stack2nix)  
[Parse](https://github.com/input-output-hk/stack2nix/commit/e01483c14ec288eeeef586c9aa31c737042bda55)
the mac operating system as `osx` or `darwin`.


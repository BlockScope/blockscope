---
title: Conditional Coding.
subtitle: >-
    Struggle to read C preprocessor interleaved <em>iffy</em> Haskell code?<br>
    Forget <code>CPP</code>. With cabal conditionals, get to do clean file diffs again!
tags: haskell, tcplugins
---

.. raw:: html

    <div id="ifdef">
    <pre class="pre">
    {-# LANGUAGE CPP #-}

    module GHC.TcPluginM.Extra

    #if __GLASGOW_HASKELL__ < 711
    #endif
    #if __GLASGOW_HASKELL__ < 711
    #endif
    #if MIN_VERSION_ghc(9,2,0)
    #elif MIN_VERSION_ghc(9,0,0)
    #endif
    #if MIN_VERSION_ghc(9,0,0)
    #else
    #if __GLASGOW_HASKELL__ < 711
    #endif
    #if MIN_VERSION_ghc(8,5,0)
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 711
    #else
    #if __GLASGOW_HASKELL__ < 809
    #else
    #endif
    #endif
    #if __GLASGOW_HASKELL__ < 802
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 809
    #if __GLASGOW_HASKELL__ >= 806
    #endif
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 809
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 711
    #else
    #endif
    #endif
    #if __GLASGOW_HASKELL__ < 802
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #endif
    #if __GLASGOW_HASKELL__ < 711
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if MIN_VERSION_ghc(8,5,0)
    #elif __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if MIN_VERSION_ghc(8,5,0)
    #else
    #if __GLASGOW_HASKELL__ < 711
    #endif
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 711
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ >= 711
    #else
    #endif
    #if __GLASGOW_HASKELL__ < 802
    #else
    #endif
    #if MIN_VERSION_ghc(8,4,0)
    #elif MIN_VERSION_ghc(8,0,0)
    #else
    #endif
    #if MIN_VERSION_ghc(9,2,0)
    #else
    #endif
    #if MIN_VERSION_ghc(8,6,0)
    #endif
    #if __GLASGOW_HASKELL__ >= 900
    #elif __GLASGOW_HASKELL__ >= 809
    #elif __GLASGOW_HASKELL__ >= 802
    #elif __GLASGOW_HASKELL__ < 711
    #endif
    #if __GLASGOW_HASKELL__ > 711
    #endif
    </pre>
    </div>

I have a dependency on ghc-tcplugins-extra_. The panel on the right shows the
#ifdefs of its one module, ``GHC.TcPluginM.Extra``. I'm happy with this
package and don't help maintain it so why am I making a disruptive pull request with 63
changed files, 1,902 additions and 458 deletions?

CPP Hell, No!
-------------

I don't much like ``CPP`` and find nested conditional blocks hard to
disentangle. One or two is fine but when they're nested and the conditions range
over many GHC versions I find it hard to take in the whole at a glance let alone
see the difference between one GHC version and the next. What is more #ifdefs
are noise in the source file.

We can can stop or reduce ``{-# LANGUAGE CPP #-}`` pragma use even when we need
to switch code between GHC versions. I'll show you how using ghc-tcplugins-extra
as an example.

One Internal Indirection
------------------------

I gut ``src/GHC.TcPluginM.Extra``[#]_ and defer to ``import Internal`` for the
implementation.

.. code-block:: haskell 

    module GHC.TcPluginM.Extra
        ( -- * Create new constraints
        newWanted
        , newGiven
        , newDerived
        -- * Creating evidence
        , evByFiat
        -- * Lookup
        , lookupModule
        , lookupName
        -- * Trace state of the plugin
        , tracePlugin
        -- * Substitutions
        , flattenGivens
        , mkSubst
        , mkSubst'
        , substType
        , substCt
        ) where

    import Internal

I thought this might screw around with the haddocks but they look good, the
internal module is invisible and the module tree is unchanged.

.. raw:: html

    <div id="module-list"><p class="caption">Modules</p><ul><li><span class="module details-toggle-control details-toggle collapser" data-details-id="n.1">GHC</span><details id="n.1" open="open"><summary class="hide-when-js-enabled">Submodules</summary><ul><li><span class="module details-toggle-control details-toggle collapser" data-details-id="n.1.1">TcPluginM</span><details id="n.1.1" open="open"><summary class="hide-when-js-enabled">Submodules</summary><ul><li><span class="module"><span class="noexpander">&nbsp;</span><a href="https://hackage.haskell.org/package/ghc-tcplugins-extra-0.4.2/docs/GHC-TcPluginM-Extra.html">GHC.TcPluginM.Extra</a></span></li></ul></details></li></ul></details></li></ul></div>

In the implementation I have two module hierarchies, ``GhcApi.*`` and
``Internal.*``.

Cabal conditionals
------------------

In the cabal file with ``impl(ghc?)`` conditonals we can pick which files to
compile.

.. code-block:: yaml

    library
        exposed-modules:
            GHC.TcPluginM.Extra
        other-modules:
            Internal
        hs-source-dirs:
            src
        if impl(ghc >= 9.2) && impl(ghc < 9.4)
            hs-source-dirs:
                src-ghc-tree
                src-ghc-9.2
        if impl(ghc >= 9.0) && impl(ghc < 9.2)
            hs-source-dirs:
                src-ghc-tree
                src-ghc-9.0
        if impl(ghc >= 8.10) && impl(ghc < 9.0)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.10
        if impl(ghc >= 8.8) && impl(ghc < 8.10)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.8
        if impl(ghc >= 8.6) && impl(ghc < 8.8)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.6
        if impl(ghc >= 8.4) && impl(ghc < 8.6)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.4
        if impl(ghc >= 8.2) && impl(ghc < 8.4)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.2
        if impl(ghc >= 8.0) && impl(ghc < 8.2)
            hs-source-dirs:
                src-ghc-flat
                src-ghc-8.0
        if impl(ghc >= 7.10) && impl(ghc < 8.0)
            hs-source-dirs:
                src-ghc-cpp

When some things stay the same but others change between GHC versions we can
group modules into different ``hs-source-dirs`` directories.

When ``8.0 <= ghc < 9.0`` I include modules in ``src-tree-flat`` so named
because the GHC module hierarchy at this time was flat. With ``ghc >= 9.0``
GHC's module hierarchy was moved to a deeper tree structure so I include modules
from ``src-tree-tree`` instead.

I put modules that change between versions into version-specific directories
like ``src-ghc-9.0`` and ``src-ghc-9.2``.

We're trading duplicating modules for ease of diffing. We can now see
differences between GHC
*dot-even-numbered* releases and see what needed to change when GHC deepened
its module hierarchy.

File diffing
------------

One change between ``src-ghc-9.0`` and ``src-ghc-9.2``.

.. code-block:: diff

     --- src-ghc-9.0/GhcApi/Constraint.hs
     +++ src-ghc-9.2/GhcApi/Constraint.hs
        module GhcApi.Constraint
            ( Ct(..
            , CtEvidence(..)
            , CtLoc
     +      , CanEqLHS(..)
            , ctLoc
            , ctEvId
            , mkNonCanonical
            ) where

        import GHC.Tc.Types.Constraint
     -      ( Ct(..), CtEvidence(..), CtLoc
     +      ( Ct(..), CtEvidence(..), CanEqLHS(..), CtLoc
            , ctLoc, ctEvId, mkNonCanonical
            )

An example of reacting to GHC's change to a deeper module hierarchy.

.. code-block:: diff

     --- src-ghc-flat/GhcApi/Predicate.hs
     +++ src-ghc-tree/GhcApi/Predicate.hs
        module GhcApi.Predicate (mkPrimEqPred) where

     -  import Predicate (mkPrimEqPred)
     +  import GHC.Core.Coercion (mkPrimEqPred)

Cabal Mixins
------------

With mixins_ we can rename modules [#]_. I've done that to pick names I want to
use that stay the same even when GHC moves things around.

.. code-block:: yaml

    mixins:
        ghc hiding ()
      , ghc (TcRnTypes as Constraint)
      , ghc (Type as Predicate)

I could have avoided using mixins like this but it helped insulate me from GHC
API changes.

Simpler with dhall
------------------

Tweaking the cabal file with all these conditionals and mixins is a bit too much
repetitive work but with hpack-dhall_ we can simplify this chore.

.. code-block:: dhall

    λ(low : Text) →
    λ(high : Text) →
    λ(srcs : List Text) →
    λ(ghc : { name : Text, mixin : List Text }) →
    λ(mods : List Text) →
      { condition = "impl(ghc >= ${low}) && impl(ghc < ${high})"
      , source-dirs =
          Prelude/List/map Text Text (λ(x : Text) → "src-ghc-${x}") srcs
      , dependencies = [ ghc ⫽ { version = ">=${low} && <${high}" } ]
      , other-modules = mods
      }

Why do this?
------------

I feel that copying code files this way is better than using CPP. We can single
thread our thoughts looking at plain Haskell code uninterrupted by C prepocessor
#ifdefs and deal with only one GHC version at a time when getting the code to
#compile against a newer GHC version or when debugging a problem.

Backporting changes is simpler too because of the diffing but may require more
edits if ranging back over multiple GHC versions.  If we don't care about
backporting then the set of modules for an older GHC version can be left alone
as we don't need to touch them with CPP #ifdefs.

.. _ghc-tcplugins-extra: https://github.com/clash-lang/ghc-tcplugins-extra#readme
.. _ghc-tcplugins-extra-undef: https://github.com/BlockScope/ghc-tcplugins-extra-undef#readme
.. _mixins: https://cabal.readthedocs.io/en/3.6/cabal-package.html?highlight=mixins#pkg-field-mixins
.. _hpack-dhall: https://github.com/cabalism/hpack-dhall#readme

.. [#] Except for ``ghc < 8.0`` where I have left the original CPP-heavy module alone untouched.
.. [#] Mixins are a cabal 2.0 feature and requires ``impl(ghc >= 8.2)``. That's
    true if I use stack but with cabal-install I can get ``impl(ghc >= 7.10.3) &&
    impl(ghc < 8.0)`` to compile. With the cabal github action in 
    ``.github/workflows/cabal.yml`` cabal can build against GHC versions ``[ 7.10.3,
    8.0.2, 8.2.2, 8.4.4, 8.6.5, 8.8.4, 8.10.7, 9.0.1, 9.2.1 ]``

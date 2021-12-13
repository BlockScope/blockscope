---
title: Import Reduction 
subtitle: 'No more iffy differences, dropping #ifdef for cabal mixins.'
slug: Import reduction for type checker plugins.
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

I don't much like ``CPP`` and find nested conditional blocks hard to
disentangle. I use the uom-plugin_ type checker plugin for units of measure and
it has a dependency on ghc-tcplugins-extra_. The panel on the right shows the
#ifdefs of module ``GHC.TcPluginM.Extra``.

A big change I made to the uom-plugin was getting it to compile with later
versions of GHC. This involved moving GHC imports to one place, behind a
``GhcApi`` module hierarchy. This involved a fair bit of ``CPP`` and shimming.

.. code-block:: haskell

    {-# LANGUAGE CPP #-}
    #if __GLASGOW_HASKELL__ > 710
    {-# LANGUAGE PatternSynonyms #-}
    #endif

    module GhcApi.Shim where

    import GhcApi

    #if __GLASGOW_HASKELL__ > 710
    tyVarsOfType :: Type -> TyCoVarSet
    tyVarsOfType = tyCoVarsOfType

    tyVarsOfTypes :: [Type] -> TyCoVarSet
    tyVarsOfTypes = tyCoVarsOfTypes

    promoteTyCon :: TyCon -> TyCon
    promoteTyCon = id
    #endif

    #if __GLASGOW_HASKELL__ >= 800

    #if __GLASGOW_HASKELL__ < 802
    pattern FunTy :: Type -> Type -> Type
    pattern FunTy t v = ForAllTy (Anon t) v
    #endif

    mkEqPred :: Type -> Type -> Type
    mkEqPred = mkPrimEqPred

    mkHEqPred :: Type -> Type -> Type
    mkHEqPred t1 t2 =
      TyConApp heqTyCon [typeKind t1, typeKind t2, t1, t2]
    #endif

With ``GHC >= 8.4`` something changed with GHC so that the uom-plugin is unable
to solve unit equations it was good at before. Using ``git bisect`` I found the
commmit in GHC that broke the plugin but haven't yet figured out the problem. 

    "The GHC API does not make allowances for easy migrations -- it's just too
    hard. Not sure where it needs to be mentioned. ... But anyone using the GHC
    API, including plugin authors, should expect breakage at every release."

    .. raw:: html

        <footer>
            <a
                href="https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3583#note_285243"
                target="_blank">
                Richard Eisenberg
            </a>
        </footer>

The Haskell Language Server is fantastic.  Beneath it all is ghc-lib_, an API
for GHC decoupled from GHC versions. I thought I could do something similar to
help with authoring GHC type checker plugins and created ghc-corrobarate_.

As a first attempt I tried using ghc-lib only to find out that this is not an
alternative API over the full GHC API. You can compile to it but not run on it.

    "While ghc-lib provides the full GHC API, it doesn't contain a runtime
    system, nor does it create a package database. That means you can't run code
    produced by ghc-lib (no runtime), and compiling off-the-shelf code is very
    hard (no package database containing the base library)."

    .. raw:: html

        <footer>
            <a
                href="http://neilmitchell.blogspot.com/2019/02/announcing-ghc-lib.html"
                target="_blank">
                Neil Mitchell
            </a>
        </footer>

That effort was a failure but it wasn't wasted. I could reshape another API that
pulls together the various GHC imports needed for typechecker plugins, by
importing and re-exporting to flatten the API. I would be able to decouple this
from GHC versions too and it would be similar to the work I'd previously done
for the uom-plugin.

I saw too that I could use cabal mixins to avoid use of the ``CPP`` pragma to
create a flatter API into the guts of GHC for those writing type checker plugins
and created ghc-corroborate_.

.. _ghc-lib: https://hackage.haskell.org/package/ghc-lib
.. _ghc-corroborate: https://github.com/BlockScope/ghc-corroborate#readme
.. _ghc-tcplugins-extra: https://github.com/clash-lang/ghc-tcplugins-extra#readme
.. _ghc-tcplugins-extra-undef: https://github.com/BlockScope/ghc-tcplugins-extra-undef#readme
.. _uom-plugin: https://github.com/adamgundry/uom-plugin#readme

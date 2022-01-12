---
title: Unit Definitions.
subtitle: How do unit definitions like <code>[u| s, min = 60 s, Hz = s^-1 |]</code> work?
tags: haskell, tcplugins
---

Quasiquotes for Units
---------------------

In F# units are builtin, declared with special syntax and erased at runtime.
They're more like Haskell newtypes than single case discriminated unions because
of this erasure.

Shipped with the `uom-plugin`, is the u quasiquoter. Units need not be declared
this way to use the plugin but this is convenient and looks good. There are base
units, derived units and convertible units.

.. code:: Haskell

    declareBaseUnit "m"
    declareDerivedUnit "N" "kg m / s^2"
    declareConvertibleUnit "kilobyte" 1024 "byte"

With the quasiquoter these unit definitions can be written as:

.. code:: Haskell

    [u| m |]
    [u| N = kg m / s^2 |]
    [u| kilobyte = 1024 byte |]

Declarations Unpacked
---------------------

The following code snippet was successfully declaring byte as a unit but then
made to fail by not giving the plugin as a GHC option. Error messages are
shorter by including some strictly unnecessary imports.

.. code:: Haskell

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE QuasiQuotes #-}
    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE TypeFamilies #-}
    {-# LANGUAGE UndecidableInstances #-}
    {-# LANGUAGE PackageImports #-}

    --{-# OPTIONS_GHC -fplugin Plugins.UoM #-}
    {-# OPTIONS_GHC -fno-warn-orphans #-}

    module Defs where

    -- NOTE: The only import needed for the declaration.
    import "uom-th" Data.UnitsOfMeasure.TH

    -- NOTE: Imports added to drop prefixes in the error messages. 
    import "uom-quantity" Data.Theory.UoM
    import "uom-quantity" Data.UnitsOfMeasure.Syntax
    import "uom-th" Data.UnitsOfMeasure.Canonical

    declareBaseUnit "byte"

.. code:: pre

    test-suite-defs/Defs.hs:23:1: error:
        • Could not deduce: IsCanonical (Unpack (Base "byte"))
            arising from the superclasses of an instance declaration
        • In the instance declaration for ‘HasCanonicalBaseUnit "byte"’
       |
    23 | declareBaseUnit "byte"
       | ^^^^^^^^^^^^^^^^^^^^^^

    test-suite-defs/Defs.hs:23:1: error:
        • Couldn't match type ‘One’ with ‘Base "byte" /: Base "byte"’
            arising from a use of ‘Data.UnitsOfMeasure.Canonical.$dmconversionBase’
        • In the expression:
            Data.UnitsOfMeasure.Canonical.$dmconversionBase @("byte")
        In an equation for ‘conversionBase’:
            conversionBase
                = Data.UnitsOfMeasure.Canonical.$dmconversionBase @("byte")
        In the instance declaration for ‘HasCanonicalBaseUnit "byte"’
       |
    23 | declareBaseUnit "byte"
       | ^^^^^^^^^^^^^^^^^^^^^^

How is the plugin helping GHC to:

    * deduce ``IsCanonical (Unpack (Base "byte"))``
    * match ``One`` with ``Base "byte" /: Base "byte"``

It looks for unpacks and then lets GHC know that two types are equivalent, the
unpacked one and another it creates on the fly using unit syntax.

.. code:: Haskell

    lookForUnpacks :: UnitDefs -> [Ct] -> [Ct] -> TcPluginM [Ct]
    lookForUnpacks uds givens wanteds = mapM unpackCt unpacks where
        unpacks = concatMap collectCt $ givens ++ wanteds

        collectCt ct = collectType uds ct $ ctEvPred $ ctEvidence ct

        unpackCt (ct,a,xs) =
            newGivenCt loc (mkEqPred ty1 ty2) (evByFiat "units" ty1 ty2)
            where
                ty1 = TyConApp (unpackTyCon uds) [a]
                ty2 = mkTyConApp (unitSyntaxPromotedDataCon uds)
                    [ typeSymbolKind
                    , foldr promoter nil ys
                    , foldr promoter nil zs ]
                loc = ctLoc ct

                ys = concatMap (\ (s, i) -> if i > 0 then genericReplicate i s else []) xs
                zs = concatMap (\ (s, i) -> if i < 0 then genericReplicate (abs i) s else []) xs

        nil = mkTyConApp (promoteDataCon nilDataCon) [typeSymbolKind]

        promoter x t = mkTyConApp cons_tycon [typeSymbolKind, mkStrLitTy x, t]
        cons_tycon = promoteDataCon consDataCon

The state of this plugin is ``UnitDefs``, a record of type families.

.. code:: Haskell

    lookupUnitDefs :: ModuleName -> ModuleName -> FastString -> TcPluginM UnitDefs
    lookupUnitDefs theory syntax pkgName = do
        mT <- lookupModule theory pkgName
        mS <- lookupModule syntax pkgName
        let f = divulgeTyCon mT
        let g = divulgeTyCon mS
        u <- f "Unit"
        b <- f "Base"
        o <- f "One"
        m <- f "*:"
        d <- f "/:"
        e <- f "^:"
        x <- g "Unpack"
        i <- g "UnitSyntax"
        c <- g "~~"
        return
            UnitDefs
                { unitKindCon = u
                , unitBaseTyCon = b
                , unitOneTyCon = o
                , mulTyCon = m
                , divTyCon = d
                , expTyCon = e
                , unpackTyCon = x
                , unitSyntaxTyCon = i
                , unitSyntaxPromotedDataCon = getDataCon i ":/"
                , equivTyCon = c
                }
        where
            getDataCon u s =
                case [ dc
                     | dc <- tyConDataCons u
                     , occNameFS (occName (dataConName dc)) == fsLit s
                     ] of
                     [d] -> promoteDataCon d
                     _ -> error $ "lookupUnitDefs/getDataCon: missing " ++ s

Debugging Time
--------------

Tracing added to typechecker plugins will only fire when compilation of a module
in triggered, for instance when reloading the module in the REPL. Once
compilation is done, tracing stops. Running the program will be quiet.

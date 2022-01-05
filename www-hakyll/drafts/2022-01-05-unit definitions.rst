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

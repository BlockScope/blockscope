---
title: Units, Measured Up
tags: fsharp, haskell, uom
---
I'm very interested in ways I can use units of measure in application code
and have the correctness of calculations with respect to units checked at
compile time. The only language I've used with this capability baked in is
F#. I've done geodesic calculations at least twice, with F# and Haskell. For
Haskell, I took a dependency on the uom-plugin, a units of measure type
checker plugin that works with the compiler, solving type equalities and
conversions that GHC can't solve.

A pair of monomorphic functions for converting between degrees and radians.
  
.. code-block:: fsharp

    open System
    open FSharp.Data.UnitSystems.SI.UnitSymbols
    open Flight.Units

    let convertDegToRad (x : float<deg>) : float<rad> = x * Math.PI / 180.0<deg/rad>
    let convertRadToDeg (x : float<rad>) : float<deg> = x / Math.PI * 180.0<deg/rad> 

I can convert between units related in their definition.

.. code-block:: haskell

    import Data.UnitsOfMeasure (Quantity, u, convert)
    import Data.UnitsOfMeasure.Convert (Convertible)

    [u| rad |]
    [u| deg = (5030569068109113 % 288230376151711744) rad |]

    convert :: forall a u v . (Fractional a, Convertible u v) => Quantity a u -> Quantity a v

The development of GHC moves fast, type checker plugins are not part of its
test suite and from ghc-8.4 something has changed so that the uom-plugin
fails to resolve equalities it was once able to resolve. Without really
wanting to I've had to look at the innards of GHC typechecker plugins to see
what the upset is between GHC and this plugin.

Another plugin I've looked at is the thoralf-plugin. I attended the talk
about this at ICFP 2018. I thought it looked very cool, wiring up an SMT
solver to do equality reasoning. The authors of the paper presented at the
conference suggest that it could subsume the uom-plugin. I dabbled at bit
with this plugin. It requires adding more constraints than needed with the
uom-plugin so that we can avoid unification variables because unification
with SMT solvers is difficult.
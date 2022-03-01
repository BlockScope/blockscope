---
title: Units
subtitle: How do we measure up when static typing?
slug: Units. How do we measure up?
tags: fsharp, haskell, uom
---
I much prefer static type checking and strong types. If a compiler is going to
catch more of my errors, sign me up. I worked on a crop model where the model
did its calculating in units customarily used in my home country, close but not
exactly the same as international units. Results however were stored in SI
units. For users from any country, we had to be able to display quantities such
as irrigation, fertilization and yields in all possible unit combinations. One
user might want to view yield in tons_ or tonnes_ per hectare and another view it
in hundredweight_ per acre.

I'm disappointed and surprised there's a gaping type hole in most programming
languages surrounding units of measure. It's a trap to fall in and there's no
help from the compiler digging ourselves out.  A small oversight of taking
a quantity in the wrong units or combining quantities of different unit without
first doing unit conversions and calcualtion results will be off and it may
well be hard to track down the error without domain knowledge, without already
knowing what are sensible ranges for values.

The only language I've used with this capability baked in is F# but there's
a compiler type checker plugin for Haskell that gives us similar capabilities.
The uom-plugin_ works with the compiler, solving unit type equalities and
conversions that the GHC compiler can't solve without outside help.

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

Another plugin I've looked at is the thoralf-plugin. I attended the talk about
the thoralf-paper_ at ICFP 2018. I thought it looked very cool, wiring up an
SMT solver to do equality reasoning. The authors of the paper presented at the
conference suggest that it could subsume the uom-plugin. I dabbled at bit with
this plugin. It requires adding more constraints than needed with the
uom-plugin so that we can avoid unification variables because unification with
SMT solvers is difficult.

.. _uom-plugin: https://github.com/adamgundry/uom-plugin
.. _thoralf-paper: https://icfp18.sigplan.org/details/haskellsymp-2018-papers/12/The-Thoralf-Plugin-For-Your-Fancy-Type-Needs
.. _hundredweight: https://en.wikipedia.org/wiki/Hundredweight
.. _tons: https://en.wikipedia.org/wiki/Ton
.. _tonnes: https://en.wikipedia.org/wiki/Tonne

---
title: Absolute Units
subtitle: are defined in terms of the fundamental units of a system
slug: Unit absolutism
tags: fsharp, haskell, uom
---
I like static type checking. If a compiler is going to catch some of my errors
before a program runs, sign me up. Strong types are cool too. They help me think
about how to fit things together to make a solution. If I'm missing a piece of
the puzzle, the compiler could suggest what fits and will show me the shape of
the hole.

So what's the big deal about units of measure as types?

Motivation
----------

    "Units-of-measure are to science what types are to programming"[#]_

Professionally, I develop software applications. When I most needed better types
for units I didn't know that types could help with checking unit conformance and
conversion. At the time I joined a project[#]_ to commercialise a potato crop
model it was coded in Borland Builder C++ and built one executable. This Windows
GUI desktop app, agronomists could use to setup and run the model. It had
pickers for input files and a presentation layer for the results of the model
run.  I didn't need the GUI elements as we would be delivering these via the web
in the commercial product.  The model itself was not a lot of code and within a
couple of days I was able to port it to standard C++ and then to C#. There were
comments in places that mentioned units, mostly on numeric properties of
classes.  Even with those comments, as a developer without prior experience in
this domain, I couldn't step through the code and know what a sensible range
would be for any variable I'd drag into the watch pane. When I ran the model I
was getting different outputs than expected.  Familiar with the model and
sensible ranges of some key variables, the research programmer at the institute
pinpointed the problem quickly.  Turned out to be a unit conversion error. A
needle in a haystack problem for me, not a domain insider.

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

Comparing Units
---------------

A pair of monomorphic functions for explicitly converting between degrees and radians.

.. hint::
    This is hint text.

.. tip::
    This is tip text.

.. note::
    This is note text.

.. warning::
    This is warning text.

.. code-block:: fsharp
    :caption: conversion.fs

    open System
    open FSharp.Data.UnitSystems.SI.UnitSymbols

    [<Measure>] type rad
    [<Measure>] type deg

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

.. _uom-plugin: https://github.com/adamgundry/uom-plugin
.. _absolute_unit: https://www.lexico.com/definition/absolute_unit

.. [#] Types for Units-of-Measure: Theory and Practice by Andrew Kennedy
.. [#] Crop Logic, a now defunct startup spun out of a research institute.

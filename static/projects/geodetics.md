---
title: 'Geodetics'
subtitle: Where on Earth are we?
---
I've coded up solutions to the direct and indirect problems of geodetics in
Haskell, F#, Unison and Koka and in so doing have been able to find problems
with some of these compilers.

## The Direct or Forward Geodetics Problem

* {x} The departure point on the ellipsoid.
* {α₁} The azimuth from the departure point.
* {s} The distance to the arrival point.

Given the above inputs, find:

* {y} The arrival point.
* {α₂} The azimuth at the arrival point.

## The Inverse or Reverse Geodetics Problem

* {x} The departure point.
* {y} The arrival point.

Given the above inputs, find:

* {s} The distance between departure and arrival points.
* {α₁} The azimuth at the departure point.
* {α₂} The azimuth at the arrival point.

## Geodetics Solutions

At the `github`/[flight-earth](https://github.com/flight-earth) organisation
you'll find a set of solutions to geodesy problems in various programming
languages.

* `flight-earth`/[meridian-arc](https://github.com/flight-earth/meridian-arc) for F#. Also published on nuget as [meridian-arc](https://www.nuget.org/packages/meridian-arc/). Interestingly, the math doesn't work quite the same on all platforms (Windows, Mac and Ubuntu) I tested with.
* `glide-angle`/[flight-earth](https://github.com/GlideAngle/flare-timing/tree/main/lang-haskell/earth) for Haskell. I'm waiting on a fix for the `uom-plugin` dependency to land before I move the package between organisations, from `glide-angle` to `flight-earth`. 
* `flight-earth`/[flat-earth](https://github.com/flight-earth/flat-earth) for Unison. Also published on unison share but I'm not finding it.
* `flight-earth`/[coriolis-effect](https://github.com/flight-earth/coriolis-effect) for Koka.

## Issues Found Incidentally

* `koka-lang`[**/koka**](http://koka-lang.org/)  
Found
[issues](https://github.com/koka-lang/koka/issues/created_by/philderbeast)
compiling
`flight-earth`[**/coriolis-effect**](https://github.com/flight-earth/coriolis-effect).
* `unisonweb`[**/unison**](https://www.unisonweb.org/)  
Found
[issues](https://github.com/unisonweb/unison/issues/created_by/philderbeast)
compiling
`flight-earth`[**/flat-earth**](https://github.com/flight-earth/flat-earth).

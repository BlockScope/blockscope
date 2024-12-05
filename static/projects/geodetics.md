---
title: 'Geodetics'
subtitle: Where on Earth are we?
---
I'm a lifelong learner and like to try out interesting or novel programming
languages.  I've coded up solutions to the direct and indirect problems of
geodetics in `Haskell`, `F#`, `Unison`, `Koka`, and `Rust`.  Occasionally I
find problems with newer compilers but this is to be expected.

The Direct or Forward Geodetics Problem

:   * {x} The departure point on the ellipsoid.
    * {α₁} The azimuth from the departure point.
    * {s} The distance to the arrival point.

    Given the above inputs, find:

    * {y} The arrival point.
    * {α₂} The azimuth at the arrival point.

The Inverse or Reverse Geodetics Problem

:   * {x} The departure point.
    * {y} The arrival point.

    Given the above inputs, find:

    * {s} The distance between departure and arrival points.
    * {α₁} The azimuth at the departure point.
    * {α₂} The azimuth at the arrival point.

## Geodetics Solutions

At the `github`/[flight-earth](https://github.com/flight-earth) organisation
you'll find a set of solutions to geodesy problems in various programming
languages (listed chronologically).

[flight-earth](https://github.com/GlideAngle/flare-timing/tree/main/lang-haskell/earth) for Haskell
: Uses the `uom-plugin` for units of measure.

[meridian-arc](https://github.com/flight-earth/meridian-arc) for F#
: Also published on nuget as
[meridian-arc](https://www.nuget.org/packages/meridian-arc/). Interestingly, the
math doesn't work quite the same on all platforms (Windows, Mac and Ubuntu) I
tested with.

[flat-earth](https://github.com/flight-earth/flat-earth) for Unison
: Also published on unison share.

[coriolis-effect](https://github.com/flight-earth/coriolis-effect) for Koka
: The docs for this language are nice, succinct and helpful.

[oblate-spheroid](https://github.com/flight-earth/oblate-spheroid) for Lean
: Doing inline doctest-like testing is nice in this language:

    ```lean4
    /-- info: 359° -/
    #guard_msgs in #eval normalizeDeg $ Deg.mk (-1.0)
    ```

[auxillary-sphere](https://github.com/flight-earth/coriolis-effect) for Rust
: Half converted by hand and half with the help of GitHub Copilot from the Lean4 solution.

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

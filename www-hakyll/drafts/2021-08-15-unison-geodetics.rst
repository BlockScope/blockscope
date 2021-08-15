---
title: Unison Geodetics
subtitle: A developer experience report.
slug: Scratching the Earth's surface with unison.
tags: unison, build
---

Before scoring can start for a flying competition we need to establish some
ground rules. One of these is, what model of the Earth are we going to use; the
FAI sphere or the WGS84 ellipsoid? Once that's decided we can work out distances
flown by pilots in the comp.

The FAI's official scoring program FS rolls its own geodetics code in C#. Its
nominated succesor, FAI-Airscore_, pulls in python geodetic libraries to do the
same. I've written geodetic packages in Haskell as part of flare-timing_ and
ported a subset of this to F#, published as meridian-arc_. I've been interested
in unison for some time and so to get to know the language and its development
environment that is somewhat special, I decided to port these geodetics again to
unison.

.. _flare-timing: https://github.com/BlockScope/flare-timing#readme
.. _meridian-arc: https://github.com/BlockScope/meridian-arc#readme
.. _FAI-Airscore: https://github.com/FAI-CIVL/FAI-Airscore

---
title: Unison Geodetics
subtitle: A developer experience report.
slug: Scratching the Earth's surface with unison.
tags: unison, build
---

Before scoring can start for a flying competition we need to establish some
ground rules like what model of the Earth to use, the FAI sphere or the WGS84
ellipsoid? Once that's decided we can work out the shortest distance around the
turnpoint cylinders of the task and distance along the course for each fix of
each pilot's track log.

The FAI's official scoring program FS rolls its own geodetics code in C# as does
its nominated succesor, FAI-Airscore_ [#]_. I've written geodetic packages in
Haskell and F#, flare-timing_ and meridian-arc_. What better way to get to try unison
than have a go again than contributing a geodetics package.

The development environment is special.

.. [#] FAI-Airscore implements the Andoyer_ method for solving geodesic distance
    on the ellipsoid but it can get distances by using package haversine_ for the
    sphere and package geopy_ for the ellipsoid.

.. _flare-timing: https://github.com/BlockScope/flare-timing#readme
.. _meridian-arc: https://github.com/BlockScope/meridian-arc#readme
.. _FAI-Airscore: https://github.com/FAI-CIVL/FAI-Airscore
.. _haversine: https://github.com/mapado/haversine
.. _geopy: https://geopy.readthedocs.io/
.. _Andoyer: https://en.wikipedia.org/wiki/Marie_Henri_Andoyer

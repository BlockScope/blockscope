---
title: Splitting Packages
subtitle: Ease the pain of moving modules around with package imports.
slug: Splitting packages with package imports.
tags: haskell, build
---

There's this one large haskell project, flare-timing_, that I regularly wait
around 360 seconds to build from scratch. What am I doing with my time? Could
splitting the large ``gap``[#]_ package into smaller packages speed up the
compile?

This package, with many reverse dependencies, implements the simpler parts of
GAP_, setting down the rules for scoring free flight competitions.  Flights are
scored for distance, effort, leading, speed and arrival at goal.  Depending on
key factors like how many pilots made goal, the weight and allocation of points
varies as does the validity of each task in a competition.  Penalties and
stopped tasks are also dealt with here. All these aspects of scoring are mostly
independant of each other so we have an opportunity to break up this package.

Smaller modules compile faster[#]_. If I broke up this big package would
compile times improve? I hoped so because not every one of its dependants
depends on the same set of imports. For instance one exe only deals with effort
and another one only deals with leading.

The package exposes one mega-module, ``Flight.Score``[#]_.  For the breakup,
I created new packages but kept ``gap`` around so I could defer updating
dependant's dependencies.  For the moment they could stay the same with their
imports. Here is how those imports would have looked before the breakup if
I had been using the PackageImports_ extension.

.. code-block:: haskell

   import "flight-gap" Flight.Score (...)

I ended up adding seven ``gap-*`` packages[#]_ after regrouping modules.

.. code-block:: pre

    .
    ├── gap
    ├── gap-allot
    ├── gap-effort
    ├── gap-lead
    ├── gap-math
    ├── gap-stop
    ├── gap-valid
    └── gap-weight

The original module imports all the others for re-export.

.. code-block:: haskell

    module Flight.Score
        ( -- explicit exports go here but are not shown
        ) where

    import "flight-gap-allot" Flight.Score
    import "flight-gap-effort" Flight.Score
    import "flight-gap-lead" Flight.Score
    import "flight-gap-math" Flight.Score
    import "flight-gap-stop" Flight.Score
    import "flight-gap-valid" Flight.Score
    import "flight-gap-weight" Flight.Score

After making changes so that packages don't depend on ``flight-gap`` but on the
finer grained ``flight-gap-*`` packages, the build took 315 seconds, an
improvement of 12.5%.

To change package dependencies, I swapped packages in the cabal file and added
the package name to imports, like with this diff;

.. code-block:: diff

    -- import qualified Flight.Score as Gap (ReachToggle(..))
    -- import Flight.Score (ArrivalFraction(..))
    ++ import qualified "flight-gap-valid" Flight.Score as Gap (ReachToggle(..))
    ++ import "flight-gap-allot" Flight.Score (ArrivalFraction(..))

As a finickity last step I could change the exported module names but haven't
bothered yet.

.. code-block:: diff

    -- import qualified "flight-gap-valid" Flight.Score as Gap (ReachToggle(..))
    -- import "flight-gap-allot" Flight.Score (ArrivalFraction(..))
    ++ import qualified Flight.Score.Valid as Gap (ReachToggle(..))
    ++ import Flight.Score.Allot (ArrivalFraction(..))

Conclusion
----------
Putting related modules together and keeping unrelated modules apart by using
finer grained packaging was worth it. The code base is better organised. It was
easier to reanimate bit-rotten test-suites and have them pass continuous
integration when the test surface is smaller. The compile times got a little
better.

.. [#] Most packages in flare-timing have ``flight-`` prefixes to their name.
   I don't show that here.
.. [#] From `Keeping Compilation Fast <https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html>`_ splitting up large modules will help compile times.
.. [#] Actually one other module declaring one data type is exposed.
.. [#] On disk I don't bother with adding a ``flight-`` prefix as most packages
   have this.
.. _flare-timing: https://github.com/BlockScope/flare-timing#readme
.. _GAP: https://github.com/BlockScope/CIVL-GAP/releases
.. _PackageImports: https://ghc.readthedocs.io/en/latest/glasgow_exts.html?highlight=packageimports#extension-PackageImports

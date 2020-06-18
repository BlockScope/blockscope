---
title: Splitting Modules
subtitle: Worth doing even if the compile times are no better.
slug: Splitting modules for no better compile times.
tags: haskell, build
---
It is taking what seems a long time to build flare-timing, nearly 9 minutes.

.. code-block:: bash

    > time stack install
    - align-time
    - area-step
    - build-flare-timing
    - comp-serve
    - cross-zone
    - discard-further
    - extract-input
    - fs-arrival
    - fs-effort
    - fs-filter
    - fs-route
    - fs-score
    - gap-point
    - land-out
    - mask-track
    - peg-frame
    - tag-zone
    - task-length
    - test-fsdb-parser
    - test-igc-parser
    - test-kml-parser
    - unpack-track

    ________________________________________________________
    Executed in  531.64 secs   fish           external
       usr time  754.24 secs  125.00 micros  754.24 secs
       sys time  122.15 secs  744.00 micros  122.15 secs

    > time stack install
    ...
    ________________________________________________________
    Executed in  557.76 secs   fish           external
       usr time  795.14 secs  105.00 micros  795.14 secs
       sys time  138.00 secs  557.00 micros  138.00 secs

    > time stack build
    ...
    ________________________________________________________
    Executed in  454.11 secs   fish           external
       usr time  644.16 secs   38.39 millis  644.12 secs
       sys time  101.74 secs    5.19 millis  101.73 secs

    > time stack build
    ...
    ________________________________________________________
    Executed in  587.15 secs   fish           external
       usr time  874.27 secs  119.00 micros  874.27 secs
       sys time  142.36 secs  728.00 micros  142.36 secs

    > time stack build flare-timing
    ...
    ________________________________________________________
    Executed in  458.09 secs   fish           external
       usr time  590.11 secs  127.00 micros  590.11 secs
       sys time   99.45 secs  750.00 micros   99.45 secs

    > time stack build app-serve
    ________________________________________________________
    Executed in  341.31 secs   fish           external
       usr time  451.67 secs  110.00 micros  451.67 secs
       sys time   55.36 secs  642.00 micros   55.36 secs

I have three small console apps that test the parsers I wrote for `*.igc`,
`*.kml` and `*.fsdb` files. They're occassionally useful but I don't need them
on every build so lets flag those to only be build sometimes.

.. code-block:: dhall

    { version =
        "0.1.0"
    , author =
        "Phil de Joux"
    , maintainer =
        "phil.dejoux@blockscope.com"
    , copyright =
        "\u00A9 2017-2019 Phil de Joux, \u00A9 2017-2019 Block Scope Limited"
    , license =
        "MPL-2.0"
    , license-file =
        "LICENSE.md"
    , tested-with =
        "GHC == 8.2.2"
    , extra-source-files =
        [ "package.dhall", "changelog.md", "README.md" ]
    , ghc-options =
        [ "-Wall"
        , "-Werror"
        , "-Wincomplete-uni-patterns"
        , "-Wcompat"
        , "-Widentities"
        , "-Wredundant-constraints"
        , "-fhide-source-paths"
        ]
    , default-extensions =
        [ "PackageImports" ]
    , dependencies =
        [ "base >=4.10.1.0 && <5" ]
    , flags =
        { suppress-failing-tests =
            { manual = False, default = True }
        , suppress-test-parsers =
            { manual = False, default = True }
        }
    }

.. code-block:: bash

    > time stack build
    ...
    ________________________________________________________
    Executed in  358.81 secs   fish           external
       usr time  513.48 secs  126.00 micros  513.48 secs
       sys time   79.14 secs  594.00 micros   79.14 secs

    > time stack build
    ...
    ________________________________________________________
    Executed in  372.52 secs   fish           external
       usr time  539.01 secs   96.00 micros  539.01 secs
       sys time   83.87 secs  463.00 micros   83.87 secs

That's got the time down a bit.

I had read that one way to speed up the build time is to decrease the size of
packages. I have one module `flight-gap`, core to the whole project and depended on by many,
that I could split up. Furthermore, each executable in flare-timing only
depends on some subset its features.

.. code-block:: bash

    > cabal v2-clean
    > cabal v2-build flare-timing
    ...
    ________________________________________________________
    Executed in  283.23 secs   fish           external
       usr time  557.16 secs  126.00 micros  557.16 secs
       sys time  115.67 secs  753.00 micros  115.66 secs

    > cabal v2-clean
    > cabal v2-build flare-timing
    ...

    ________________________________________________________
    Executed in  283.36 secs   fish           external
       usr time  551.23 secs  122.00 micros  551.23 secs
       sys time  116.59 secs  710.00 micros  116.58 secs

    > cabal v2-clean
    > cabal v2-build all
    ...
    ________________________________________________________
    Executed in  346.27 secs   fish           external
       usr time  1010.65 secs  125.00 micros  1010.65 secs
       sys time  200.69 secs  780.00 micros  200.69 secs

    > cabal v2-clean
    > time cabal v2-build flare-timing
    ...
    ________________________________________________________
    Executed in  294.54 secs   fish           external
       usr time  574.61 secs  131.00 micros  574.61 secs
       sys time  124.42 secs  773.00 micros  124.42 secs


In `flight-gap` I'd exposed one main module, `Flight.Score`. I broke this
package up but retained it so that dependent packages did not need to
change right away. In directory `gap` I have retained package `flight-gap` and
added packages for the alloction, effort, leading, point and penalty math,
stopped tasks, task validity and weighting between aspects of flights scored,
all in packages with a name prefix of `flight-gap-` in directories with a name
prefix of **`gap-`**.

.. code-block:: bash

    .
    ├── app-serve
    ├── app-view
    ├── build
    ├── clip
    ├── cmd
    ├── comp
    ├── detour-via-sci
    ├── detour-via-uom
    ├── earth
    ├── flare-timing
    ├── fsdb
    ├── gap
    ├── gap-allot
    ├── gap-effort
    ├── gap-lead
    ├── gap-math
    ├── gap-stop
    ├── gap-valid
    ├── gap-weight
    ├── hcoord
    ├── igc
    ├── kml
    ├── latlng
    ├── lookup
    ├── mask
    ├── route
    ├── scribe
    ├── siggy-chardust
    ├── span
    ├── task
    ├── tasty-compare
    ├── time
    ├── track
    ├── units
    ├── vernix
    └── zone

Modules that depended on flight-gap


.. code-block:: bash

    .
    ├── app-serve
    ├── comp
    ├── flare-timing
    ├── fsdb
    ├── lookup
    ├── mask
    ├── scribe

After making changes so that packages don't depend on `flight-gap` but on the
finer grained `flight-gap-*` packages:

.. code-block:: bash

    > stack clean
    > time stack install
    ...
    ________________________________________________________
    Executed in  396.05 secs   fish           external
       usr time  568.94 secs  138.00 micros  568.94 secs
       sys time   90.22 secs  774.00 micros   90.22 secs

    > stack clean
    > time stack build
    ...
    ________________________________________________________
    Executed in  384.55 secs   fish           external
       usr time  560.48 secs  128.00 micros  560.47 secs
       sys time   89.41 secs  672.00 micros   89.41 secs

    > cabal v2-clean
    > cabal v2-build flare-timing
    ...
    ________________________________________________________
    Executed in  318.86 secs   fish           external
       usr time  517.85 secs  111.00 micros  517.85 secs
       sys time  119.29 secs  699.00 micros  119.29 secs

    > cabal v2-clean
    > cabal v2-build all
    ...
    ________________________________________________________
    Executed in  372.21 secs   fish           external
       usr time  936.68 secs  116.00 micros  936.68 secs
       sys time  213.25 secs  690.00 micros  213.25 secs

    > cabal v2-clean
    > time cabal v2-build flare-timing
    ...
    ________________________________________________________
    Executed in  345.91 secs   fish           external
       usr time  684.72 secs  126.00 micros  684.71 secs
       sys time  149.00 secs  741.00 micros  149.00 secs

    > pier clean-all
    > pier build
    (from shared cache: Downloading lts-11.22.yaml)
    (from shared cache: Downloading stack-setup-2.yaml)
    (from shared cache: Downloading ghc-8.2.2-x86_64-apple-darwin.tar.bz2)
    (from shared cache: Unpacking GHC)
    (from shared cache: Installing GHC locally)
    (from shared cache: Building core package database)
    (from shared cache: Downloading tasty-hunit-0.10.0.1.tar.gz)
    ...
    (from shared cache: flight-time-0.1.0: building library)
    (from shared cache: flare-timing-0.1.0: building executable align-time)
    (from shared cache: flare-timing-0.1.0: building executable area-step)
    (from shared cache: flare-timing-0.1.0: building executable cross-zone)
    (from shared cache: flare-timing-0.1.0: building executable discard-further)
    (from shared cache: flare-timing-0.1.0: building executable extract-input)
    (from shared cache: flare-timing-0.1.0: building executable fs-arrival)
    (from shared cache: flare-timing-0.1.0: building executable fs-effort)
    (from shared cache: flare-timing-0.1.0: building executable fs-filter)
    (from shared cache: flare-timing-0.1.0: building executable fs-route)
    (from shared cache: flare-timing-0.1.0: building executable fs-score)
    (from shared cache: flare-timing-0.1.0: building executable gap-point)
    (from shared cache: flare-timing-0.1.0: building executable land-out)
    (from shared cache: flare-timing-0.1.0: building executable mask-track)
    (from shared cache: flare-timing-0.1.0: building executable peg-frame)
    (from shared cache: flare-timing-0.1.0: building executable tag-zone)
    (from shared cache: flare-timing-0.1.0: building executable task-length)
    (from shared cache: flare-timing-0.1.0: building executable unpack-track)
    Build completed in 0:31m
    ________________________________________________________
    Executed in   30.37 secs   fish           external
       usr time  123.58 secs  118.00 micros  123.58 secs
       sys time   27.54 secs  902.00 micros   27.54 secs

    > pier clean
    > time pier build --no-shared-cache
    (from cache: Downloading lts-11.22.yaml)
    (from cache: Downloading stack-setup-2.yaml)
    (from cache: Downloading ghc-8.2.2-x86_64-apple-darwin.tar.bz2)
    (from cache: Unpacking GHC)
    (from cache: Installing GHC locally)
    (from cache: Building core package database)
    (from cache: Downloading uom-plugin-0.3.0.0.tar.gz)
    (from cache: Downloading QuickCheck-2.10.1.tar.gz)
    ...
    (from cache: app-serve-0.1.0: building executable comp-serve)
    (from cache: flight-time-0.1.0: building library)
    (from cache: flare-timing-0.1.0: building executable align-time)
    (from cache: flare-timing-0.1.0: building executable area-step)
    (from cache: flare-timing-0.1.0: building executable cross-zone)
    (from cache: flare-timing-0.1.0: building executable discard-further)
    (from cache: flare-timing-0.1.0: building executable extract-input)
    (from cache: flare-timing-0.1.0: building executable fs-arrival)
    (from cache: flare-timing-0.1.0: building executable fs-effort)
    (from cache: flare-timing-0.1.0: building executable fs-filter)
    (from cache: flare-timing-0.1.0: building executable fs-route)
    (from cache: flare-timing-0.1.0: building executable fs-score)
    (from cache: flare-timing-0.1.0: building executable gap-point)
    (from cache: flare-timing-0.1.0: building executable land-out)
    (from cache: flare-timing-0.1.0: building executable mask-track)
    (from cache: flare-timing-0.1.0: building executable peg-frame)
    (from cache: flare-timing-0.1.0: building executable tag-zone)
    (from cache: flare-timing-0.1.0: building executable task-length)
    (from cache: flare-timing-0.1.0: building executable unpack-track)
    Build completed in 0:20m
    ________________________________________________________
    Executed in   19.17 secs   fish           external
       usr time   99.04 secs  146.00 micros   99.04 secs
       sys time   13.84 secs  888.00 micros   13.84 secs



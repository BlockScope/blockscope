---
title: What on Earth is Unison?
subtitle: A developer experience report
slug: Scratching the Earth's surface with unison.
tags: unison, build
---

Unison_ is interesting.  I coded up solutions to the direct and indirect
problems of geodesy with it.  I got to know the language a bit, was able to
contribute some `bug reports`_ and share my initial impressions of the
developer experience.

Geodesic Problems
-----------------

1. The direct or forward problem:

  * ``x`` The departure point on the ellipsoid.
  * ``α₁`` The azimuth from the departure point.
  * ``s `` The distance to the arrival point.

  Given the above inputs, find:

  * ``y`` The arrival point.
  * ``α₂`` The azimuth at the arrival point.

2. The indirect, inverse or reverse problem:

  * ``x`` The departure point.
  * ``y`` The arrival point.

  Given the above inputs, find:

  * ``s`` The distance between departure and arrival points.
  * ``α₁`` The azimuth at the departure point.
  * ``α₂`` The azimuth at the arrival point.

I've been working on a reference implementation for scoring free flight
competitions. For this I need to be able to find the distance between two
points on the Earth, the ``s`` of the indirect solution.

Flying Geodesics
----------------

Pilots flying cross country competitions in hang gliders or paragliders carry
flight computers with sensors for position and air pressure. The vario_ beeps
vary in tone, rapidity and volume as we climb or sink.  Sound really is the
best way of taking in this information as the pilot is often too busy in active
air to be glancing at or tapping a smallish screen.  It is great to be climbing
but worrying to be sinking and tones emitted by the vario match those moods.

Except for respecting airspace, height is pretty much ignored when scoring. It
is however used for stopped tasks to fairly reward pilots for being higher than
others. A pilot that could have converted height to distance if time had
allowed for the glide will get a bonus distance.  Scoring is not simple but
boils down to initially finding out when and where each pilot crossed the
start, turnpoint cylinders and goal of the tasked course for the day and from
there finding out how far a pilot flew and how much ahead of other pilots were
they during the race.

A comp can be scored against a sphere or an ellipsiod. The FAI sphere and the
WGS84 ellipsoid are the only two models of the Earth sanctioned by the FAI for
scoring. For the sphere we'd likely calculate distance using haversines formula
and for the ellipsoid we might use Vincenty's formula or other faster but less
accurate solutions. Solutions for the ellipsoid are iterative and do not work
near the poles, not that we fly competitions in those regions.  It is cold
enough at cloudbase when scorching hot on the ground.

Familiar Territory
------------------

FS_ is the official free flight competition scoring program of the world
governing body for air sports (the FAI - Fédération Aéronautique
Internationale). It will likely be retired and succeeded by Airscore_ [#]_.
I've helped a little on both projects but have spent most of my energy on
flare-timing_. FS is a C# winforms Windows-only desktop app and Airscore is
a python flask web app.  Both roll some of their own geodetics code and I have
too, in Haskell for flare-timing and in F#.

Unison's Development Environment
--------------------------------

The development environment is special. The code is stored in a database, parsed
as an abstract syntax tree with nodes identified by hash and metadata attaching
names to those hashes. What this means in practice is that neither you
developing your code or someone browsing your repo will see anything that looks
like code.

In practice this is no big problem. There are ways to discover and browse the
code. I'm used to working in a REPL with little more than syntax highlighting as
a language specific aid in the editor. Unison's code base manager watches for
changes in a scratch file I'm editing and on start up provides a link where the
code base can be browsed.

.. code-block:: pre

    > unison

    The Unison Codebase UI is running at
    http://127.0.0.1:61119/lUbPTPoxXjHHcTWH9MwOoaKzhJiYyQts/ui

    Now starting the Unison Codebase Manager...
    Welcome to Unison!
    You are running version: release/M2g
    I'm currently watching for changes to .u files under ~/.../flat-earth
    Type help to get help. 😎

When code I'm working on in the scratch file compiles I can add it to or update
it in the code base. Like git, I can choose to push my changes to a repo with
builtin commands. The whole experience is quite sensitive to current namespace
selected. We can move around namespaces with the ``namespace`` or ``cd``
commands and use ``ls`` to see what's there:

.. code-block:: pre

    > unison

    .> cd flight
    .flight> ls

    1. Earth/        (46 definitions)
    2. Geodesy/      (69 definitions)
    3. LatLng/       (15 definitions)
    4. Units/        (43 definitions)
    5. Zone/         (3 definitions)
    6. licenseTypes/ (1 definition)
    7. metadata/     (7 definitions)
    8. patch         (patch)

Using commands to navigate namespaces and definitions that have the same names
as commands we're used to for navigating the file system is great and could be
taken further. Sadly a ``cd ..`` command doesn't go up the namespace tree.

To work on an existing definition, find it and select it for editing:

.. code-block:: pre

    .> find haversines

    1. flight.Geodesy.Math.EarthMath.Haversines : EarthMath

    .> edit 1
    ☝️
    I added these definitions to the top of ~/.../flat-earth/scratch.u

        unique type flight.Geodesy.Math.EarthMath
        = Pythagorus
        | Haversines
        | Vincenty
        | AndoyerLambert
        | ForsytheAndoyerLambert
        | FsAndoyer

    You can edit them there, then do `update` to replace the definitions currently in this
    namespace.

To edit this definition, I'm better off navigating to its namespace first to
avoid long namespace qualified names and to avoid a bug where fully qualified
names cannot be added or updated if the current prompt is at ``.>``, the root
namespace.

.. code-block:: pre

    .> cd flight.Geodesy.Math
    .flight.Geodesy.Math> edit EarthMath
    ☝️
    I added these definitions to the top of ~/.../flat-earth/scratch.u

        unique type EarthMath
        = Pythagorus
        | Haversines
        | Vincenty
        | AndoyerLambert
        | ForsytheAndoyerLambert
        | FsAndoyer

Teething Problems
-----------------

The pretty printing and parsing doesn't roundtrip. Printed constructor
parentheses were missed and indentation was offside. This was an inconvenience
but I pretty quickly recognized the edits I'd need to make to dumped definitions
to get them to compile again.

.. code-block:: diff

    aOfHaversine : LatLng -> LatLng -> Rad
    aOfHaversine x y =
        use Float * +
        use Lat Lat
        use Lng Lng
        LatLng (Lat xLatF) (Lng xLngF) = x
        LatLng (Lat yLatF) (Lng yLngF) = y
        (dLatF, dLngF) =
            use Float -
            (yLatF - xLatF, yLngF - xLngF)
    --  Rad hLatF = haversine (Rad dLatF)
    --  Rad hLngF = haversine (Rad dLngF)
    ++  (Rad hLatF) = haversine (Rad dLatF)
    ++  (Rad hLngF) = haversine (Rad dLngF)
        Rad (hLatF + (cos xLatF * cos yLatF * hLngF))

It was easy to make updates that resulted in names coming unstuck from hashes
especially when renaming things.

.. code-block:: pre

    .flight.Geodesy> find InverseSolution

    1.  unique type InverseSolution s α
    2.  InverseSolution.InverseSolution : s -> α -> Optional α -> InverseSolution s α
    3.  InverseSolution.doc : Doc
    4.  InverseSolution.s : #7l8qisp5pk s α -> s
    5.  InverseSolution.s.modify : (i ->{g} o) -> #7l8qisp5pk i α ->{g} #7l8qisp5pk o α
    6.  InverseSolution.s.set : s1 -> #7l8qisp5pk s α -> #7l8qisp5pk s1 α
    7.  InverseSolution.α₁ : #7l8qisp5pk s α -> α
    8.  InverseSolution.α₁.modify : (o ->{g} o) -> #7l8qisp5pk s o ->{g} #7l8qisp5pk s o
    9.  InverseSolution.α₁.set : α₁1 -> #7l8qisp5pk s α₁1 -> #7l8qisp5pk s α₁1
    10. InverseSolution.α₂ : #7l8qisp5pk s α -> () α
    11. InverseSolution.α₂.modify : (() α ->{g} () α) -> #7l8qisp5pk s α ->{g} #7l8qisp5pk s α
    12. InverseSolution.α₂.set : () α -> #7l8qisp5pk s α -> #7l8qisp5pk s α

Some very ordinary float functions are missing from the base library such as
``Float.isNaN`` and related predicates for testing infinity. I also encountered
a bug in float comparison:

.. code-block:: pre

    Now evaluating any watch expressions (lines starting with `>`)... Ctrl+C cancels.

    1 | > 0.0 < 0.0
        ⧩
        false

    2 | > 0.0 < 1.0
        ⧩
        true

    3 | > 1.0 < 2.0
        ⧩
        true

    4 | > +0.0 < +1.0
        ⧩
        true

    5 | > +1.0 < +2.0
        ⧩
        true

    6 | > -1.0 < 0.0
        ⧩
        true

    7 | > -2.0 < -1.0
        ⧩
        false

    8 | > -1.0 < -2.0
        ⧩
        true

There's no pattern matching or type deconstruction in arguments to
functions. I have that in Haskell and F# and miss it.

Some of the property tests I'd like to have added were not possible without float
generators that are not yet included.

I couldn't get the code I wanted to write to compile with the trunk branch and
ended up using the latest ``release/M2g`` branch but even there I had to
backport an interpreter fix to prevent a ``missing integral case`` exception
when using
*less than* when comparing floats.

Overall Impression
------------------

Unison is a new language with a distinctive and unusal development environment
yet I was able to get what I wanted to do done, helped along by good
documentation, excellent talks and quick feedback in the slack channel.

The builtin ``find`` command and code base browsing web app are great but I
still think I'd like to be able browse a subset of the codebase on disk as files
in the appropriate branch of a namespace tree. Once I saw I could dump a lot of
definitions to the scratch file then move them beneath the fold so that they
were only visible to me I was happier.

I really like transcripts.

.. code-block:: pre

    > unison transcript.fork Haversine.md

    Transcript will be run on a copy of the codebase at:
        /Users/pdejoux
    Running the provided transcript file...
    ⚙️   Processing stanza 3 of 3.
    💾  Wrote ~/.../flat-earth/Haversine.output.md

    > unison transcript.fork Vincenty.md
    ⚙️   Processing stanza 5 of 5.
    💾  Wrote ~/.../flat-earth/Vincenty.output.md

I used transcripts to document what the code does for both the `haversine
solution`_ and `Vincenty solution`_ to the geodetic inverse problem. Shown below
is a snippet of the output of the transcript for the Vincenty solution:

.. code-block:: pre

    ✅
    
    ellipsoids.u changed.

    Now evaluating any watch expressions (lines starting with
    `>`)... Ctrl+C cancels.

    1 | > bessel
            ⧩
            Ellipsoid (Radius 6377397.155) 299.1528128

    2 | > hayford
            ⧩
            Ellipsoid (Radius 6378388.0) 297.0

The code base manager works well as one tool with a command shell, a REPL and
git-like code base actions.

The cached tests and definitions seem to hold a lot of promise to save developer
time. I can't say I noticed but isn't that the point!

The task I was solving didn't require anything fancy so I can't say much about
the unison language itself other than it is similar enough to Haskell or F# that
it felt familiar already except I suspect I don't quite understand when to use
``let``.

I enjoyed trying out unison and contributing a package [#]_.

.. [#] FAI-Airscore implements the Andoyer_ method for solving geodesic distance
    on the ellipsoid but it can get distances by using package haversine_ for the
    sphere and package geopy_ for the ellipsoid.

.. [#] The code in the `blockscope/flat-earth`_ repo can be found at
    ``contrib/pdejoux`` in unison share, the common code base, where we can link
    directly to definitions such as this one for InverseSolution_.

.. _flare-timing: https://github.com/BlockScope/flare-timing#readme
.. _meridian-arc: https://github.com/BlockScope/meridian-arc#readme
.. _FS: http://fs.fai.org
.. _Airscore: https://github.com/FAI-CIVL/FAI-Airscore
.. _haversine: https://github.com/mapado/haversine
.. _geopy: https://geopy.readthedocs.io/
.. _Andoyer: https://en.wikipedia.org/wiki/Marie_Henri_Andoyer
.. _haversine solution: https://github.com/BlockScope/flat-earth/blob/main/Haversine.output.md
.. _Vincenty solution: https://github.com/BlockScope/flat-earth/blob/main/Vincenty.output.md
.. _InverseSolution: https://share.unison-lang.org/latest/types/@gtl0lqo99gd558dvadhpv2d4vsl0bei7kdern03h6jml2jmjo8pffrk3d5nt95q1ft3ui79aats93pfabmjbttl9pd4ljd07r482ut0
.. _blockscope/flat-earth: https://github.com/BlockScope/flat-earth#readme
.. _unison: https://www.unisonweb.org/
.. _vario: https://en.wikipedia.org/wiki/Variometer
.. _bug reports: https://github.com/unisonweb/unison/issues/created_by/philderbeast

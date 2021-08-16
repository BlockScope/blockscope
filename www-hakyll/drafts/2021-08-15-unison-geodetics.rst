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
than have a go at contributing a geodetics package.

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
changes in a scratch file I'm editing and provides a link where the code base
can be browsed.

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

To work on an existing definition, find it and select it for editing.

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

Problems
--------

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
ended up using the latest ``release/M2g`` but even there I had to backport an
interpreter fix to prevent a ``missing integral case`` exception when using
*less than* when comparing floats.

Overall Impression
------------------

Unison is a new language with distinctive and unusal development environment. I
was able to get what I wanted to do done. I was helped along by good
documentation, excellent talks and quick feedback in the slack channel.

The builtin ``find`` command and code base browsing web app are great but I
still think I'd like to be able browse a subset of the codebase on disk as files
in the appropriate branch of a namespace tree. Once I saw I could dump a lot of
definitions to the scratch file then move them beneath the fold so that they
were only visible to me I was happier.  We can dump more than one item from a
``find`` command's result list.

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
solution`_ and `vincenty solution`_ to the geodetic inverse problem. Shown below
is a snippet of the output of the transcript for the vincenty solution:

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
git.

.. [#] FAI-Airscore implements the Andoyer_ method for solving geodesic distance
    on the ellipsoid but it can get distances by using package haversine_ for the
    sphere and package geopy_ for the ellipsoid.

.. _flare-timing: https://github.com/BlockScope/flare-timing#readme
.. _meridian-arc: https://github.com/BlockScope/meridian-arc#readme
.. _FAI-Airscore: https://github.com/FAI-CIVL/FAI-Airscore
.. _haversine: https://github.com/mapado/haversine
.. _geopy: https://geopy.readthedocs.io/
.. _Andoyer: https://en.wikipedia.org/wiki/Marie_Henri_Andoyer
.. _haversine solution: https://github.com/BlockScope/flat-earth/blob/main/Haversine.output.md
.. _vincenty solution: https://github.com/BlockScope/flat-earth/blob/main/Vincenty.output.md

---
title: Swapping Nose Cones
subtitle: Comparing reflex-frp with rescript, svelte, feliz and sutil.
tags: haskell, rescript, fsharp, react, svelte
---

Initially when I set out to write a reference implementation of the scoring for
hang gliding and paragliding competitions, I'd expected that it would be enough
to read in files and write out files. No user interface needed. As I got closer
to completing the project, finding and correcting scoring bugs was taking longer
than I'd like. I'd be scanning through YAML and CSV files looking at a wall of
numbers for latitudes, longitudes, times and points so I started on a user
interface to help with the development but in the end it turned out to be a
great way to visualise how comps are scored too, useful for the pilots. It
really brought the static formulae, diagrams and graphs of the rules document to
life.

With a map, I can see at a glance if a flight line crosses the circle of a
turpoint and so answer the question, did the pilot make the turnpoint. With
turnpoint crossings labelled with times, I could quickly verify if scoring had
picked the right crossing for a pilot after the start gun.  With tables drilling
down to the parts of scoring and comparing the reference and official scores for
a comp side-by-side, I could see at a glance where the big differences were. The
more things I added to the visualization app, the faster development became and
I was able to quickly find bugs in the official scoring program.

When I first wrote the front end to flare-timing I wanted to learn something
new.  I'd been keen to try functional reactive ever since Stephen Blackheath
explained the fundamentals of it in a short talk he'd given at a Haskell
Hackathon in Wellington, New Zealand.  Apart from the effort of getting setup,
developing this in Reflex-FRP_ was really intuitive for me with a printed copy
of `cheat sheet`__ at hand.

I'd previously developed product user interfaces for desktop and the web using
many different frameworks and paradigms:

* Classic TUI [#]_ with `Turbo Vision`_ and Turbo Pascal on 486 PCs with turbo buttons.
    |turbo buttons|
* Windows GUI [#]_ with MFC [#]_ in C++.
* ASP.NET Web Forms in C# and Razor.
* Adding dynamic content to server rendered pages with jQuery and Knockout.js.
* Early React in Typescript without JSX.
* Elm when it had signals.

.. [#] Text User Interface (TUI)
.. [#] Graphical User Interface (GUI)
.. [#] Microsof Foundation Classes (MFC)
.. _Turbo Vision: https://en.wikipedia.org/wiki/Turbo_Vision
.. |turbo buttons| image:: https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Casebuttons.jpg/330px-Casebuttons.jpg

Recently I was external reviewer for Compositional IT's `SAFE stack course`__.
I'd used F# in the backend before but with React and Elm up front.  Reviewing
this training material was a great way to get up to speed quickly with that part
of the SAFE stack I was unfamiliar with, Fable_. Thanks Isaac for picking me to
do the review.


After the review I wanted to give some front end frameworks a go and compare
them.  Instead of something like the TODO app, how about Flare Timing? I'd split
the Reflex-FRP front end of that project off as Nose Cone. Obviously doing the
whole user interface again would be way too much work but some the first few
views would be representative enough of read-only front end development work.
I'd be fetching and presenting data in an interactive user interface but I
wouldn't be showing any forms, doing validation or posting data back to a
server.

.. raw:: html

    <pre><code>+-----------+-------+--------------------------+-------------------+
    | Framework | Hours |          Demo            |      Source       |
    |===========|=======|==========================|===================|
    | rescript  | 11.75 | <a href="http://rescript.flaretiming.com/">rescript.flaretiming.com</a> |     <a href="https://github.com/NoseCone/dive-stick">dive-stick</a>    |
    +-----------+-------+--------------------------+-------------------+
    | svelte    |  8.5  |   <a href="http://svelte.flaretiming.com/">svelte.flaretiming.com</a> | <a href="https://github.com/NoseCone/variable-geometry">variable-geometry</a> |
    +-----------+-------+--------------------------+-------------------+
    | feliz     | 13.0  |    <a href="http://feliz.flaretiming.com/">feliz.flaretiming.com</a> |  <a href="https://github.com/NoseCone/leading-edge">leading-edge</a>     |
    +-----------+-------+--------------------------+-------------------+
    | sutil     |  6.75 |    <a href="http://sutil.flaretiming.com/">sutil.flaretiming.com</a> |   <a href="https://github.com/NoseCone/aspect-ratio">aspect-ratio</a>    |
    +-----------+-------+--------------------------+-------------------+</code></pre>

Note that the Sutil solution borrows the Thoth JSON decoding and views from
Feliz solution.

.. _Fable: https://fable.io/
.. _Reflex-FRP: https://reflex-frp.org/
.. __cheat sheet: https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
.. __safe stack course: https://www.compositional-it.com/training-coaching/functional-web-programming/
.. __demo: https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
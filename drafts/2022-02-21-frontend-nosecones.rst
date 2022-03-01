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

When I first wrote the front end flare-timing I wanted to learn something new.
Apart from the effort of getting setup, developing in reflex-frp__ was really
intuitive for me with a printed copy of `cheat sheet`__ at hand. I've developed
product user interfaces for desktop and the web using many different frameworks
and paradigms:

* ASP.NET Web Forms in C# and Razor
* Adding some dynamic content to server rendered pages with jQuery and Knockout.js.
* Early React in Typescript without JSX
* Elm when it had signals

Recently I was external reviewer for Compositional IT's `SAFE stack course`__.
I'd used F# in the backend before but with React and Elm up in front.  Reviewing
this training material was a great way to get up to speed quickly. Thanks Isaac
for picking me to do the review.

I wanted to compare a few front end frameworks and instead of something like the
TODO app, I chose to reimplement a few pages of the front end of flare-timing
that I then split off from the main project and renamed nose cone::

    +----------------+-------+
    | Framework      | Hours |
    +================+=======+
    | rescript react |       |
    +----------------+-------+
    | svelte         |       |
    +----------------+-------+
    | feliz          |       |
    +----------------+-------+
    | sutil          |       |
    +----------------+-------+


.. __reflex-frp: https://reflex-frp.org/
.. __cheat sheet: https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
.. __safe stack course: https://www.compositional-it.com/training-coaching/functional-web-programming/
.. __demo: https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
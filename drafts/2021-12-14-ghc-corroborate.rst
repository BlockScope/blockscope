---
title: GHC Corroborate.
subtitle: A package for import aggregation when writing typechecker plugins.
tags: haskell, tcplugins
---

A Few Good Imports
------------------

It can be hard compiling a plugin when you don't know what imports you need
across GHC versions. What has been renamed? What has been added? To help with
the import problem, I wrote ghc-corroborate_ as a simpler, flatter API into the
guts of GHC for those writing type checker plugins.

Backstory
---------

The uom-plugin is wonderful but stuck at ``ghc < 8.4``.  Something changed with
GHC so that this plugin can no longer work with GHC to solve unit equations it
was good at before. Using ``git bisect`` I found the commit in GHC that broke
the plugin but haven't yet figured out the problem.

    "The GHC API does not make allowances for easy migrations -- it's just too
    hard. Not sure where it needs to be mentioned. ... But anyone using the GHC
    API, including plugin authors, should expect breakage at every release."

    .. raw:: html

        <footer>
            <a
                href="https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3583#note_285243"
                target="_blank">
                Richard Eisenberg
            </a>
        </footer>

The Haskell Language Server is fantastic.  Beneath it all is ghc-lib_, an API
for GHC decoupled from GHC versions. I thought I could do something with
ghc-corrobarate_.  As a first attempt I tried using ghc-lib only to find out
that this is not an alternative API over the full GHC API. You can compile to it
but not run on it. Bummer I can't use ghc-lib to insulate me from GHC changes.

    "While ghc-lib provides the full GHC API, it doesn't contain a runtime
    system, nor does it create a package database. That means you can't run code
    produced by ghc-lib (no runtime), and compiling off-the-shelf code is very
    hard (no package database containing the base library)."

    .. raw:: html

        <footer>
            <a
                href="http://neilmitchell.blogspot.com/2019/02/announcing-ghc-lib.html"
                target="_blank">
                Neil Mitchell
            </a>
        </footer>

The typechecker plugins I've seen only use a subset of GHC's API so I tried
reshaping the needed imports, going with a ``GhcApi.*`` module hierarchy stable
across GHC versions [#]_.

.. _ghc-lib: https://hackage.haskell.org/package/ghc-lib
.. _ghc-corroborate: https://github.com/BlockScope/ghc-corroborate#readme
.. _uom-plugin: https://github.com/adamgundry/uom-plugin#readme

.. [#] Similar to the GHC version decoupling work I'd previously done for the
    uom-plugin plugin and the ghc-tcplugin-extra library for plugin authors.

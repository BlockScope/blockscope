---
title: Hpack Dhall Tricks
subtitle: Subtleties for generating cabal files.
slug: Tweak cabal file generation.
tags: cabal, hpack-dhall
---

If you put tests in a map they will come out in any order but you can fix the
order explicitly.

.. code:: dhall

    , tests.e2e-test.source-dirs = "e2e-test"
    , tests.waspc-test.source-dirs = "test"
    , tests.cli-test.source-dirs = "cli/test"

Hpack orders `build-depends`` alphabetically.

Expands version bounds `aeson ^>= 1.5.6` to `aeson >=1.5.6 && <1.6`.
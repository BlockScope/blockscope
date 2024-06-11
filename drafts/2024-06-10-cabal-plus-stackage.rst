---
title: Cabal + Stackage
subtitle: Using a Stackage resolver with Cabal
tags: haskell, updo
---

Prior to Conversion
===================

I've included Copilot in the Updo Examples. Before conversion it had a
``cabal.project`` the same as the default project.

.. note::

    When there is no explicit ``cabal.project``, `local packages
    <project-packages_>`_ defaults to ``./*.cabal``.
    

.. _project-packages: https://cabal.readthedocs.io/en/latest/cabal-project-description-file.html#specifying-the-local-packages
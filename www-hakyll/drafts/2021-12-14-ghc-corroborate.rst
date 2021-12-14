---
title: Annoucing GHC Corroborate.
subtitle: A package deal to imports writing typechecker plugins.
tags: haskell, tcplugins
---

With something changed with GHC so that The uom-plugin is unable help ``GHC >=
8.4`` to solve unit equations it was good at before. Using ``git bisect`` I
found the commit in GHC that broke the plugin but haven't yet figured out the
problem. 

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
for GHC decoupled from GHC versions. I thought I could do something similar to
help with authoring GHC type checker plugins and created ghc-corrobarate_.

As a first attempt I tried using ghc-lib only to find out that this is not an
alternative API over the full GHC API. You can compile to it but not run on it.

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

That effort was a failure but it wasn't wasted. I could reshape another API that
pulls together the various GHC imports needed for typechecker plugins, by
importing and re-exporting to flatten the API. I would be able to decouple this
from GHC versions too and it would be similar to the work I'd previously done
for the uom-plugin.

With combining mixins_ with conditonals in the cabal file, we can rename modules
and pick which files to compile. We can the ``CPP`` language pragma,  files.

I wrote ghc-corroborate_ as a flatter API into the guts of GHC for those writing
type checker plugins 

.. _ghc-lib: https://hackage.haskell.org/package/ghc-lib
.. _ghc-corroborate: https://github.com/BlockScope/ghc-corroborate#readme
.. _ghc-tcplugins-extra: https://github.com/clash-lang/ghc-tcplugins-extra#readme
.. _ghc-tcplugins-extra-undef: https://github.com/BlockScope/ghc-tcplugins-extra-undef#readme
.. _uom-plugin: https://github.com/adamgundry/uom-plugin#readme
.. _mixins: https://cabal.readthedocs.io/en/3.6/cabal-package.html?highlight=mixins#pkg-field-mixins

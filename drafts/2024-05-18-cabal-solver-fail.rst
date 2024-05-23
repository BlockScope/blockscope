---
title: Cabal solver fail
subtitle: What did it take to convert ``hledger`` to use Updo for project generation?
tags: haskell, updo
---

Prior to Conversion
===================

A long time on a `short list of projects
<updo-shortlist_>`_ to convert to Updo, ``hledger`` is well-known project that
uses a GHC version naming scheme with its many Stack projects.

.. code-block:: pre

    $ tree -P '*.project|stack*.yaml' --prune -L 1
    .
    ├── cabal.project    ▨ ghc-x.y.z
    ├── stack8.10.yaml   ▨ ghc-8.10.7
    ├── stack9.0.yaml    ▨ ghc-9.0.2
    ├── stack9.2.yaml    ▨ ghc-9.2.8
    ├── stack9.4.yaml    ▨ ghc-9.4.8
    ├── stack9.6.yaml    ▨ ghc-9.6.4
    └── stack.yaml       ▨ ghc-9.8.2

There's an asymmetry in having many Stack projects but only one Cabal project.

.. warning::

    The one ``cabal.project`` isn't good enough to cover all versions of GHC we want to
    build with. While Cabal's solver can come up with a build plan for each of the
    compiler versions, one of them doesn't build [#]_.

Conversion
==========

The conversion, `up-do/hledger <updo-hledger_>`_, adds configuration for GHC
versions; ``9.0.2``, ``9.2.8``, ``9.4.8`` and ``9.8.2`` but not ``8.10.7`` as
this didn't build as-is.  The configuration for the conversion can be found in
``project-stackage`` and ``project-dhall`` folders. Updo can import cabal
configurations directly from Stackage but not if there are any version conflicts
as there always were. With ``hledger`` itself published to Stackage, the version
in development is always ahead and a conflicting version.

.. code-block:: pre

    $ tree project-stackage
    project-stackage
    ├── lts-19.33.config
    ├── lts-20.26.config
    ├── lts-21.25.config
    ├── lts-22.22.config
    └── nightly-2024-05-18.config

Each of the ``project-dhall/ghc-x.y.z`` folders are very similar to each other
and mostly vary in the version equality constraints, in ``constraints.dhall``.

.. code-block:: pre

    $ tree project-dhall
    project-dhall
    ├── ghc-9.0.2
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.2.8
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.4.8
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.6.5
    │   ├── constraints.dhall
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       └── stack-snippet.dhall
    ├── ghc-9.8.2
    │   └── text-templates
    │       ├── cabal-snippet.dhall
    │       ├── dhall2cabal.dhall
    │       ├── dhall2stack.dhall
    │       ├── stack-snippet-debug.dhall
    │       └── stack-snippet.dhall
    ├── pkg-groups.dhall
    ├── pkgs
    │   └── hledger.dhall
    ├── pkgs-upgrade-todo.dhall
    └── stack-snippet-nix-comments.dhall


Generated Projects
==================

I developed Updo for a large project that had one current GHC version and one
(or two) prospective GHC upgrade versions. In that situation we worked with a
pair of current projects and a pair of upgrade projects, one project of each for
Cabal and Stack.

.. code-block:: pre

    $ tree -P '*.project|*.yaml' --prune -L 1
    .
    ├── cabal.project
    ├── cabal.upgrade.project
    ├── stack.upgrade.yaml
    └── stack.yaml

The above files are actually generated in Updo by way of intermediates:

.. code-block:: makefile

    # To make stack.yaml or cabal.project and no other, mark the file we copy from
    # as intermediate. This is all we want when not doing a GHC upgrade.
    #
    # Comment out these .INTERMEDIATE targets to allow these files to be kept.
    .INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
    .INTERMEDIATE: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
    .INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
    .INTERMEDIATE: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml

What I've done then when converting ``hledger`` to use Updo is keep the
intermediate files:

.. code-block:: pre

    tree -P 'ghc-*.dhall2stack.yaml|ghc-*.dhall2cabal.project' --prune -L 1
    .
    ├── ghc-9.0.2.dhall2cabal.project
    ├── ghc-9.0.2.dhall2stack.yaml
    ├── ghc-9.2.8.dhall2cabal.project
    ├── ghc-9.2.8.dhall2stack.yaml
    ├── ghc-9.4.8.dhall2cabal.project
    ├── ghc-9.4.8.dhall2stack.yaml
    ├── ghc-9.6.5.dhall2cabal.project
    ├── ghc-9.6.5.dhall2stack.yaml
    ├── ghc-9.8.2.dhall2cabal.project
    └── ghc-9.8.2.dhall2stack.yaml

Each of these ``.project`` files should have pinned dependencies that closely
match the versions used in the matching ``.yaml`` file.

These can be compared with:

.. code-block:: pre

    $ stack --stack-yaml=ghc-9.8.2.dhall2stack.yaml \
        ls dependencies cabal \
        > ghc-9.8.2.dhall2stack.yaml.freeze
    $ cabal freeze --project-file=ghc-9.8.2.dhall2cabal.project

It is a little annoying that the formatting differs between the two outputs.

.. code-block:: diff

    $ diff ghc-9.8.2.dhall2stack.yaml.freeze ghc-9.8.2.dhall2cabal.project.freeze 
    1,276c1,361
    < constraints:
    <   , Cabal ==3.10.3.0
    <   , Cabal-syntax ==3.10.2.0
    <   , Decimal ==0.5.2
    ...
    > active-repositories: hackage.haskell.org:merge
    > constraints: any.Cabal ==3.10.2.0,
    >              any.Cabal-syntax ==3.10.2.0,
    >              any.Decimal ==0.5.2,

I've added Stack lock files too so that these can be compared (and saw that they
were ignored in `.gitignore`).

.. code-block:: diff

    $ diff ghc-9.6.5.dhall2stack.yaml.lock stack9.6.yaml.lock --unified
    --- ghc-9.6.5.dhall2stack.yaml.lock	2024-05-18 12:19:38.978371883 -0400
    +++ stack9.6.yaml.lock	2024-05-18 12:19:38.982371994 -0400
    @@ -5,15 +5,15 @@
    
    packages:
    - completed:
    -    hackage: vty-windows-0.2.0.2@sha256:add74928c695b83f651116a73171b097524c7989d2b08c7c6e2816c982047cab,2815
    +    hackage: vty-windows-0.2.0.1@sha256:6c75230057a708168dbc420975572511ad3ec09956bf73c3b6f4be03324e8b13,2815
        pantry-tree:
    -      sha256: 348b3ce7af7bc386b8ecdf469c31b251eebf3a59945cd9de58f8cc8667fdcd7d
    +      sha256: 0de977580639c2a90958ef68ec03128d0362ff13657c66c4f25cedc12ba3259e
        size: 2160
    original:
    -    hackage: vty-windows-0.2.0.2
    +    hackage: vty-windows-0.2.0.1
    snapshots:
    - completed:
    -    sha256: 4be1ca5d31689b524a7f0f17a439bbe9136465213edc498e9a395899a670f2aa
    -    size: 718486
    -    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/22.yaml
    -  original: lts-22.22
    +    sha256: 629fdd46125079fa6d355106005b2ab88bd39332169dd416fda06d5c0aaa63e2
    +    size: 713332
    +    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/17.yaml
    +  original: lts-22.17

I went with ``vty-windows-0.2.0.2`` for all configurations. I don't know why
``stack9.6.yaml`` is the only one to specify ``vty-windows-0.2.0.1``.

On versions, in ``project-versions.mk`` I've left ``ghc-9.0.2`` as the current
version and ``ghc-9.8.2`` as the upgrade version, just because that was the
order I tackled them in. As for using Updo, here are some commands I used during
the conversion:

.. code-block:: pre

    $ make -f project-files.mk
    $ make -f project-files.mk upgrade-projects
    $ GHC_UPGRADE=9.6.5 STACKAGE_UPGRADE=lts-22.22 make -f project-files.mk upgrade-projects
    $ make -f project-files.mk clean

Probably the biggest hurdle to using Updo right now is being able to use it
without needing to have
[nix-prefetch-git](https://github.com/cabalism/updo/issues/5) available.

.. [#] The ``cabal.project`` doesn't build with ``ghc-9.2.8`` using dependencies
    chosen by Cabal's solver:

    .. code-block:: pre

        $ cabal build all --enable-tests --enable-benchmarks
        Resolving dependencies...
        Build profile: -w ghc-9.2.8 -O1
        ...
        Failed to build text-ansi-0.3.0.1.
        Configuring library for text-ansi-0.3.0.1..
        Preprocessing library for text-ansi-0.3.0.1..
        Building library for text-ansi-0.3.0.1..
        [1 of 4] Compiling Text.Builder.ANSI
        ghc: panic! (the 'impossible' happened)
        (GHC version 9.2.8:
        refineFromInScope
        InScope {wild_00 x_X1 x_X3 x_X6 x_Xa x_Xe x_Xi x_Xm x_Xq x_Xu x_Xy
                x_XC x_XG x_XK x_XO x_XS x_XW x_X10 x_X13 x_X17 x_X1b x_X1f x_X1j
                x_X1n x_X1r x_X1v x_X1z x_X1D x_X1H x_X1L x_X1P x_X1T x_X1X x_X21
                a_a13O b_a13P c_a13Q d_a13R s_a13S x_a1M6 x_a1N3 b_a1Si black red
                green yellow blue magenta cyan white brightBlack brightRed
                brightGreen brightYellow brightBlue brightMagenta brightCyan
                brightWhite rgb blackBg redBg greenBg yellowBg blueBg magentaBg
                cyanBg whiteBg brightBlackBg brightRedBg brightGreenBg
                brightYellowBg brightBlueBg brightMagentaBg brightCyanBg
                brightWhiteBg rgbBg bold faint italic underline doubleUnderline
                strikethrough frame encircle overline surround isatty $trModule
                esc_s1RL $trModule_s1RY $trModule_s1RZ $trModule_s1S0
                $trModule_s1S1 ds1_s1So overline_s1Sv overline_s1Sw encircle_s1Sx
                encircle_s1Sy frame_s1Sz frame_s1SA strikethrough_s1SB
                strikethrough_s1SC doubleUnderline_s1SD doubleUnderline_s1SE
                underline_s1SF underline_s1SG italic_s1SH italic_s1SI faint_s1SJ
                faint_s1SK bold_s1SL bold_s1SM s_s1Tg s_s1Ti s_s1Tk s_s1Tm s_s1To
                s_s1Tq s_s1Ts s_s1Tu s_s1Tw s_s1Ty s_s1TA s_s1TC s_s1TE s_s1TG
                s_s1TI s_s1TK s_s1U4 s_s1U6 s_s1U8 s_s1Ua s_s1Uc s_s1Ue s_s1Ug
                s_s1Ui s_s1Uk s_s1Um s_s1Uo s_s1Uq s_s1Us s_s1Uu s_s1Uw s_s1Uy}
        semi_s1RX
        Call stack:
            CallStack (from HasCallStack):
                callStackDoc, called at compiler/GHC/Utils/Panic.hs:181:37 in ghc:GHC.Utils.Panic
                pprPanic, called at compiler/GHC/Core/Opt/Simplify/Env.hs:706:30 in ghc:GHC.Core.Opt.Simplify.Env

        Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

        Error: cabal: Failed to build text-ansi-0.3.0.1 (which is required by
        test:unittest from hledger-lib-1.33.99, test:doctest from hledger-lib-1.33.99
        and others). See the build log above for details.

.. _updo-shortlist: https://github.com/up-do#proposed
.. _updo-hledger: https://github.com/up-do/hledger
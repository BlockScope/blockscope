---
title: Updo Pandoc
subtitle: Walkthrough of converting Pandoc to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting Pandoc
=================

Before the conversion[#]_, the Cabal project builds. The Stack project didn't
build first try, something failed with a copy of ``alex``. Trying again worked.

    .. code-block:: pre

        $ stack build --test --no-run-tests --bench --no-run-benchmarks
        ...
        Error: [S-7282]
               Stack failed to execute the build plan.
                
               While executing the build plan, Stack encountered the error:
                
               [S-7011]
               While building package alex-3.3.0.0 (scroll up to its section to see the error) using:
               /.../.stack/setup-exe-cache/x86_64-linux/Cabal-simple_6HauvNHV_3.8.1.0_ghc-9.4.5 --verbose=1 --builddir=.stack-work/dist/x86_64-linux/ghc-9.4.5 copy
               Process exited with code: ExitFailure 1 

We can't use the resolver from stackage as-is because of the version conflict
with pandoc itself.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] next goal: pandoc (user goal)
      [__0] rejecting: pandoc-3.1.9 (constraint from project config https://stackage.org/lts-21.7/cabal.config requires ==3.0.1)
      [__0] rejecting: pandoc-3.1.8, ..., pandoc-1.14 (constraint from user target requires ==3.1.9)
      [__0] fail (backjumping, conflict set: pandoc)
      After searching the rest of the dependency tree exhaustively, these were the goals I've had most trouble fulfilling: pandoc

The workaround for this is to download the stackage resolver and switch our
templates to use ``StackageLocal`` instead of ``StackageWeb`` and then remove
this conflict and others by commenting out those lines where the stackage
version equality constraint conflicts.

    .. code-block:: pre

      $ mkdir project-stackage
      $ curl https://www.stackage.org/lts-21.7/cabal.config > project-stackage/lts-21.7.config

    .. code-block:: diff

      $ git diff project-stackage/lts-21.7.config
      -             commonmark ==0.2.3,
      -             commonmark-extensions ==0.2.3.5,
      +          -- commonmark ==0.2.3,
      +          -- commonmark-extensions ==0.2.3.5,
      -             http-client-tls ==0.3.6.1,
      +          -- http-client-tls ==0.3.6.1,
      -             lpeg ==1.0.4,
      +          -- lpeg ==1.0.4,
      -             pandoc ==3.0.1,
      +          -- pandoc ==3.0.1,
      -             skylighting ==0.13.4.1,
      -             skylighting-core ==0.13.4.1,
      +          -- skylighting ==0.13.4.1,
      +          -- skylighting-core ==0.13.4.1,
      -             skylighting-format-blaze-html ==0.1.1,
      +          -- skylighting-format-blaze-html ==0.1.1,
      -             texmath ==0.12.8,
      +          -- texmath ==0.12.8,
      -             tls ==1.6.0,
      +          -- tls ==1.6.0,
      -             typst-symbols ==0.1.2,
      +          -- typst-symbols ==0.1.2,

The version conflicts will be encountered one-by-one. This step is a bit
repetitive but very easy. As the project imports
``project-stackage/lts-21.7/cabal.config`` directly, we don't need to remake
each time after commenting out a line. Here's the abbreviated sequence of
failing builds:

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] next goal: pandoc (user goal)
      [__0] rejecting: pandoc-3.1.9
            (constraint from project config https://stackage.org/lts-21.7/cabal.config requires ==3.0.1)
      [__0] rejecting: pandoc-3.1.8, ..., pandoc-1.14
            (constraint from user target requires ==3.1.9)
      [__0] fail (backjumping, conflict set: pandoc)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc

Download ``cabal.config`` for the stackage resolver and comment out ``pandoc ==3.0.1``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: texmath (dependency of pandoc)
      [__1] rejecting: texmath-0.12.8.4
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.12.8)
      [__1] rejecting: texmath-0.12.8.3, ..., texmath-0.6.5.1
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.12.8.4)
      [__1] fail (backjumping, conflict set: pandoc, texmath)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: texmath, pandoc

Comment out ``texmath ==0.12.8``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] trying: texmath-0.12.8.4 (dependency of pandoc)
      [__2] next goal: typst-symbols (dependency of texmath)
      [__2] rejecting: typst-symbols-0.1.5
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.1.4)
      [__2] rejecting: typst-symbols-0.1.4
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.1.2)
      [__2] rejecting: typst-symbols-0.1.3.1, ..., typst-symbols-0.1.3
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.1.4)
      [__2] fail (backjumping, conflict set: texmath, typst-symbols)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: texmath, pandoc, typst-symbols

Comment out ``typst-symbols ==0.1.2``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: skylighting-core (dependency of pandoc)
      [__1] rejecting: skylighting-core-0.14
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.13.4.1)
      [__1] rejecting: skylighting-core-0.13.4.1, ..., skylighting-core-0.7
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.14)
      [__1] fail (backjumping, conflict set: pandoc, skylighting-core)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, skylighting-core

Comment out ``skylighting-core ==0.13.4.1``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: skylighting (dependency of pandoc)
      [__1] rejecting: skylighting-0.14
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.13.4.1)
      [__1] rejecting: skylighting-0.13.4.1, ..., skylighting-0.3.5
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.14)
      [__1] fail (backjumping, conflict set: pandoc, skylighting)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, skylighting

Comment out ``skylighting ==0.13.4.1``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] trying: skylighting-0.14 (dependency of pandoc)
      [__2] next goal: skylighting-format-blaze-html (dependency of skylighting)
      [__2] rejecting: skylighting-format-blaze-html-0.1.1.1
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.1.1)
      [__2] rejecting: skylighting-format-blaze-html-0.1.1, skylighting-format-blaze-html-0.1
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.1.1.1)
      [__2] fail (backjumping, conflict set: skylighting, skylighting-format-blaze-html)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, skylighting,
      skylighting-format-blaze-html

Commenting out ``skylighting-format-blaze-html ==0.1.1``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: http-client-tls (dependency of pandoc)
      [__1] rejecting: http-client-tls-0.3.6.3
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.3.6.1)
      [__1] rejecting: http-client-tls-0.3.6.2, ..., http-client-tls-0.2.0.0
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.3.6.3)
      [__1] fail (backjumping, conflict set: http-client-tls, pandoc)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, http-client-tls

Commenting out ``http-client-tls ==0.3.6.1``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] trying: http-client-tls-0.3.6.3 (dependency of pandoc)
      [__2] next goal: tls (dependency of http-client-tls)
      [__2] rejecting: tls-1.9.0
            (constraint from project config ./project-stackage/lts-21.7.config requires ==1.6.0)
      [__2] rejecting: tls-1.8.0, ..., tls-0.1
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==1.9.0)
      [__2] fail (backjumping, conflict set: http-client-tls, tls)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: tls, pandoc, http-client-tls

Commenting out ``tls ==1.6.0``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: commonmark-extensions (dependency of pandoc)
      [__1] rejecting: commonmark-extensions-0.2.4
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.2.3.5)
      [__1] rejecting: commonmark-extensions-0.2.3.6, ..., commonmark-extensions-0.1.0.0
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.2.4)
      [__1] fail (backjumping, conflict set: commonmark-extensions, pandoc)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, commonmark-extensions

Commenting out ``commonmark-extensions ==0.2.3.5``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-3.1.9 (user goal)
      [__1] next goal: commonmark (dependency of pandoc)
      [__1] rejecting: commonmark-0.2.4
            (constraint from project config ./project-stackage/lts-21.7.config requires ==0.2.3)
      [__1] rejecting: commonmark-0.2.3, ..., commonmark-0.1.0.0
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==0.2.4)
      [__1] fail (backjumping, conflict set: commonmark, pandoc)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: pandoc, commonmark

Commenting out ``commonmark ==0.2.3``.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: pandoc-lua-engine-0.2.1.2 (user goal)
      [__1] next goal: lpeg (dependency of pandoc-lua-engine)
      [__1] rejecting: lpeg-1.1.0
            (constraint from project config ./project-stackage/lts-21.7.config requires ==1.0.4)
      [__1] rejecting: lpeg-1.0.4, lpeg-1.0.3, lpeg-1.0.2, lpeg-1.0.1, lpeg-1.0.0
            (constraint from project config ./project-cabal/ghc-9.4.5/constraints.config requires ==1.1.0)
      [__1] fail (backjumping, conflict set: lpeg, pandoc-lua-engine)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: lpeg, pandoc-lua-engine

Commenting out ``lpeg ==1.0.4``. After commenting out this final conflicting
equality version constraint, the build succeeds.

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Build profile: -w ghc-9.4.5 -O1
      ...
      $ cabal build all --enable-tests --enable-benchmarks
      Up to date


Converting Unison
=================

After conversion we can build Unison with Stack as before and now we can build
with Cabal too:

.. code-block:: pre

    $ cabal clean
    $ cabal build all --enable-tests --enable-benchmarks
    Resolving dependencies...
    Build profile: -w ghc-9.4.7 -O1
    ...
    $ cabal build all --enable-tests --enable-benchmarks
    Up to date

    $ stack purge
    $ stack build --test --no-run-tests --bench --no-run-benchmarks
    ...
    Completed 17 action(s).

Conversion Steps
================

The steps of converting a project to Updo, using conversion of Cabal for example, are:

#. Ignores
    Ignore the working (``.updo``) and bootstrap (``updo``) folders in ``.gitignore``:

    .. code-block:: diff

        +.updo
        +updo

#. Versions
    Put stackage resolver and GHC version into ``project-versions.mk``[#]_, not
    bothering with separate upgrade versions for now. The process for adding an
    upgrade version is the same as for adding an initial current version.

    .. code-block:: makefile

        GHC_VERSION ?= 9.4.7
        STACKAGE_VERSION ?= lts-21.19
        GHC_UPGRADE ?= 9.4.7
        STACKAGE_UPGRADE ?= lts-21.19

#. Stackage Config
    Download a `cabal.config <stackage-cabal-config_>`_ file from stackage
    matching the resolver version and save it to
    ``project-stackage/${STACKAGE-VERSION}.config``.  This likely won't work
    as-is. No worries, we'll comment out version constraints that clash later.

    .. code-block:: bash

        $ mkdir -p project-stackage
        $ curl -sSL https://www.stackage.org/lts-21.19/cabal.config > project-stackage/lts-21.19.config

#. Group Packages
    Add configuration under ``project-dhall/ghc-${GHC-VERSION}``.  We'll break
    the packages up into groups and as we're not yet upgrading we'll use an
    empty list for upgrades yet to do.

    .. code-block:: dhall

        -- project-dhall/pkg-groups.dhall
        [ "benchmarks", "hackage", "tests" ]

        -- project-dhall/pkgs/benchmarks.dhall
        [ "cabal-benchmarks", "solver-benchmarks" ]

        -- project-dhall/pkgs/hackage.dhall
        [ "Cabal", "Cabal-syntax", "cabal-install", "cabal-install-solver" ]

        -- project-dhall/pkgs/tests.dhall
        [ "Cabal-QuickCheck", "Cabal-described", "Cabal-tests", "Cabal-tree-diff", "cabal-testsuite" ]

        -- project-dhall/pkgs-upgrade-todo.dhall
        [] : List Text

#. Source Repositories
    Cabal doesn't use any source repository packages so we can leave all of
    these empty[#]_.

    .. code-block:: dhall

        -- project-dhall/ghc-9.4.7/deps-external.dhall
        -- project-dhall/ghc-9.4.7/deps-internal.dhall
        -- project-dhall/ghc-9.4.7/forks-external.dhall
        -- project-dhall/ghc-9.4.7/forks-internal.dhall
        [] : List { loc : Text, tag : Text, sub : List Text }

#. Text Templates
    Add text templates for the ways we want to generate projects. Pasted
    verbatim, the following ``dhall2config``[#]_ template for Cabal and
    ``dhall2stack`` template for Stack put the snippet content before the
    default template content.

    .. code-block:: dhall

        -- project-dhall/ghc-9.4.7/text-templates/dhall2config.dhall
        \(stackage-resolver : Text) ->
        \(ghc-version : Text) ->
          let project-dhall2config = ../../../updo/text-templates/dhall2config.dhall
        
          in  ''
              ${./cabal-snippet.dhall}
              ${project-dhall2config stackage-resolver ghc-version}
              ''

    .. code-block:: dhall

        -- project-dhall/ghc-9.4.7/text-templates/dhall2stack.dhall
        let TYPES = ./../../../updo/types.dhall
        
        let null = https://prelude.dhall-lang.org/List/null
        
        in  \(pkgs-done : List Text) ->
            \(stackage-resolver : Text) ->
              let pkgs-todo = ../../pkgs-upgrade-todo.dhall
        
              let pkg-config =
                    { constraints = ./../constraints.dhall
                    , source-pkgs =
                      { deps-external = ./../deps-external.dhall
                      , deps-internal = ./../deps-internal.dhall
                      , forks-external = ./../forks-external.dhall
                      , forks-internal = ./../forks-internal.dhall
                      }
                    }
        
              in  ''
                  ${./stack-snippet.dhall (None Text)}
                  ${../../../updo/text-templates/dhall2stack.dhall
                      stackage-resolver
                      ( if    null Text pkgs-todo
                        then  TYPES.PkgSet.AllPkgs pkgs-done
                        else  TYPES.PkgSet.PkgUpgrade
                                { todo = pkgs-todo, done = pkgs-done }
                      )
                      pkg-config}
                  ''

    .. note::

        The ``dhall2stack`` template is more complicated than the
        ``dhall2config`` template[#]_ because everything generated goes into one
        ``ghc-x.y.z.dhall2stack.yaml`` file so it **has to** handle upgrades
        whereas the root ``ghc-x.y.z-dhall2config.project`` imports generated 
        ``project-config/pkgs/*.config`` package groups indirectly through
        ``project-config/pkgs.config``.

        In ``project-config/pkgs/*.config`` files, partitioning of packages
        into those included in the upgrade project and those yet to do is done
        by the installed ``updo-pkg-groups`` executable or the
        ``./updo/project-dhall2config/pkg-groups.hs`` script invoked by a make
        recipe and not by the ``dhall2config`` template.

#. Snippets
    Snippets are used to add extra configuration to the generated projects,
    configuration unknown to Updo. Compare generated projects with those same
    files before the conversion to see what's missing.

    .. code-block:: dhall

        -- project-dhall/ghc-9.4.7/text-templates/cabal-snippet.dhall
        ''
        tests: True
        optional-packages: ./vendored/*/*.cabal
        constraints: rere -rere-cfg
        program-options
          ghc-options: -fno-ignore-asserts
        ''

        -- project-dhall/ghc-9.4.7/text-templates/stack-snippet.dhall
        \(stackage-resolver : Optional Text) ->
          let resolver =
                merge
                  { None = ""
                  , Some =
                      \(r : Text) ->
                        ''
        
                        resolver: ${r}''
                  }
                  stackage-resolver
        
          in  ''
              user-message: "WARNING: This stack project is generated."
              allow-newer: true
              flags:
                rere:
                  rere-cfg: false
              ghc-options:
                "$locals": -fhide-source-paths
              ${resolver}
              ''

    .. note::
        We need ``allow-newer: true`` because ``cabal-testsuite`` has a custom
        setup depending on ``3.10.*`` of ``Cabal`` and ``Cabal-syntax`` while
        the rest of the package depends on ``3.11.*``.

#. Bootstrap
    Add the entry and bootstrapping Updo makefile, ``project-files.mk``:

    .. code-block:: makefile

        # project-files.mk
        # To use installed executables instead of *.hs scripts, set these to true.
        PKG_GROUPS_HS_EXE ?= false
        PKGS_SORTED_HS_EXE ?= false
        PKGS_UPGRADE_DONE_HS_EXE ?= false
        
        include project-versions.mk
        include updo/Makefile
        
        project-nix/ghc-%/sha256map.nix: ghc-%.sha256map.nix
        	mkdir -p $(@D) && cp $^ $@
        
        .PHONY: all
        all: \
          projects \
          project-nix/ghc-$(GHC_VERSION)/sha256map.nix \
          project-versions.nix
        
        # To make stack.yaml or cabal.project and no other, mark the file we copy from
        # as intermediate. This is all we want when not doing a GHC upgrade.
        #
        # Comment out these .INTERMEDIATE targets to allow these files to be kept.
        .INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
        .INTERMEDIATE: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
        .INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
        .INTERMEDIATE: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
        
        .DEFAULT_GOAL := all
        
        UPDO_VERSION ?= 1.0.0
        HACKAGE := http://hackage.haskell.org/package
        UPDO_URL := ${HACKAGE}/updo-${UPDO_VERSION}/updo-${UPDO_VERSION}.tar.gz
        
        updo/Makefile:
        	rm -rf updo
        	curl -sSL ${UPDO_URL} | tar -xz
        	mv updo-${UPDO_VERSION} updo
        	chmod +x $$(grep -RIl '^#!' updo)

#. Constrain Versions
    Try to generate projects with ``make``. If this fails, Stack will complain
    the loudest.

    .. code-block:: pre

        $ make -f project-files.mk
        ...
          * directory must match >=1.2 && <1.4, but this GHC boot package has been
            pruned from the Stack configuration.  You need to add the package
            explicitly to extra-deps. (latest matching version is 1.3.8.1).
          * process must match >=1.2.1.0 && <1.7, but this GHC boot package has
            been pruned from the Stack configuration. You need to add the package
            explicitly to extra-deps. (latest matching version is 1.6.17.0).
          * directory must match >=1.2 && <1.4, but this GHC boot package has
            been pruned from the Stack configuration. You need to add the package
            explicitly to extra-deps. (latest matching version is 1.3.8.1).
          * process must match >=1.2.1.0 && <1.7, but this GHC boot package has
            been pruned from the Stack configuration. You need to add the package
            explicitly to extra-deps. (latest matching version is 1.6.17.0).

    Use the suggestions from Stack to add version equality constraints:

    .. code-block:: dhall

        -- project-dhall/ghc-9.4.7/constraints.dhall
        [ { dep = "directory", ver = "1.3.8.1" }
        , { dep = "filepath", ver = "1.4.100.4" }
        , { dep = "process", ver = "1.6.17.0" }
        , { dep = "rere", ver = "0.2" }
        , { dep = "semaphore-compat", ver = "1.0.0@rev:1" }
        , { dep = "unix", ver = "2.8.2.1" }
        ]

    .. note::
        All the recommendations from Stack match ``cabal freeze`` versions before
        the conversion, except for ``process-1.6.18.0`` and ``unix-2.8.3.0``.

#. Fixup Unsatisfiable Version Constraints
    Where there are unsatisfiable version constraints with the Cabal solver,
    comment out the relevant line from the stackage-sourced ``cabal.config``
    that we saved locally:

    .. code-block:: haskell

        -- project-stackage/lts-21.19.config
        -- NOTE: Due to revisions, this file may not work. See:
        -- https://github.com/fpco/stackage-server/issues/232
        
        -- Stackage snapshot from: http://www.stackage.org/snapshot/lts-21.19
        -- Please place this file next to your .cabal file as cabal.config
        -- To only use tested packages, uncomment the following line:
        -- remote-repo: stackage-lts-21.19:http://www.stackage.org/lts-21.19
        with-compiler: ghc-9.4.7
        constraints:
        ...
            -- Cabal installed,
            -- cabal-install ==3.8.1.0,
            -- cabal-install-solver ==3.8.1.0,
            -- Cabal-syntax installed,
            -- directory installed,
            -- filepath installed,
            -- process installed,
            -- unix installed,

.. [#] The ``project-versions.mk`` filename is a convention we've used so far
    but you can use any name for this file.

.. [#] ``updo-1.0.0`` doesn't use a `default empty list <empty-list-default_>`_
    when a configuration file is missing but that feature is in the works,
    implemented but not yet published.

.. _empty-list-default: https://github.com/cabalism/updo/issues/9

.. [#] ``dhall2caball`` is not shown here as it's very similar to ``dhall2stack``.

    .. code-block:: diff

            -- ${./stack-snippet.dhall (None Text)}
            ++ ${./cabal-snippet.dhall}
            -- ${../../../updo/text-templates/dhall2stack.dhall
            ++ ${../../../updo/text-templates/dhall2cabal.dhall

.. _up-do: https://github.com/orgs/up-do/repositories
.. _dex: https://github.com/up-do/dex-lang
.. _stack: https://github.com/up-do/stack
.. _stack-1: https://github.com/up-do/stack
.. _stack-1-fork: https://github.com/commercialhaskell/stack/commit/68bc7057f7c24086f32f4c647571be0faa4a6512
.. _cabal: https://github.com/up-do/cabal
.. _cabal-1: https://github.com/up-do/cabal
.. _cabal-1-fork: https://github.com/haskell/cabal/commit/976f86ab67952d377c25f19e6a2594e0000900a2
.. _stackage-lookup: https://www.stackage.org/lts-20.23
.. _stackage-cabal-config: https://www.stackage.org/lts-21.19/cabal.config
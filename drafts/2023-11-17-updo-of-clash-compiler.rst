---
title: How to Updo Clash
subtitle: Walkthrough of converting Clash to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting Clash
==================

Before the conversion[#]_, there are Stack and Cabal projects.

    .. code-block:: bash

      $ tree -P 'cabal.project*|stack.yaml*' --prune
      .
      ├── cabal.project
      ├── clash-ffi
      │   └── example
      │       └── cabal.project
      └── stack.yaml

The Stack project doesn't build:

    .. code-block:: pre

      $ stack build --test --no-run-tests --bench --no-run-benchmarks

      Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The
            'Stack configuration' refers to the set of package versions specified by the
            snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
            package is replaced, Stack prunes all other such packages that depend on it) and
            any extra-deps:
            
            In the dependencies for tasty-hedgehog-1.1.0.0:
              * tasty must match >=0.11 && <1.5,
                but tasty-1.5 is in the Stack configuration (latest matching version is 1.4.3).
            needed due to clash-cores-1.9.0 -> tasty-hedgehog-1.1.0.0
            
            In the dependencies for clash-cores-1.9.0:
              * clash-prelude-hedgehog needed,
                but no version is in the Stack configuration (latest matching version is 1.8.1).
            needed since clash-cores is a build target.
            
            In the dependencies for clash-prelude-1.9.0:
              * doctest-parallel must match >=0.3.1 && <0.4,
                but doctest-parallel-0.2.5 is in the Stack configuration (latest matching version is
                0.3.1).
            needed since clash-prelude is a build target.
            
            Some different approaches to resolving some or all of this:
            
              * To ignore certain version constraints and build anyway, in /.../.stack/config.yaml (global configuration) or
                /.../clash-compiler/stack.yaml (project-level configuration), set allow-newer: true and add these
                package names under allow-newer-deps: clash-prelude and tasty-hedgehog.
              
              * Recommended action: try adding the following to your extra-deps in /.../clash-compiler/stack.yaml
                (project-level configuration):
                
                - clash-prelude-hedgehog-1.8.1@sha256:9ec3aa3f8195481f5ce4942b34a49c97dd132dd1c8f1fa58aeecbd82c2602e86,1410
                - doctest-parallel-0.3.1@sha256:f1ec7b7044d156dacf3bc5464c9c692d12f490ec8bb933c6a9007d98c42004d5,5862
                - tasty-1.4.3@sha256:5b53fded93109f6704b599c3498eb73c5b0ed1a975f1912dd98b4b19c61f8bc9,2653

The Cabal project builds after we inspect the nix configuration to see what the
default and supported GHC versions are:

    .. code-block:: nix

      # The versions of GHC that we want to be able to build / develop against
      # within the nix environment. Since nix is lazy, only derivations for
      # versions of GHC which are used are actually evaluated.
      ghcVersions = [ "ghc902" "ghc962" ];

      # We pick a single version of GHC to use by default within nix. This is
      # probably cleaner than always having N copies of each package / app and
      # being forced to refer to them by their GHC version.
      defaultGhcVersion = "ghc962";

On the project's readme, there's a table that shows that clash builds with GHC
versions ``8.6`` through to ``9.6``. Indeed we tried the latest version of each
compiler series, versions ``8.6.5``, ``8.8.4``, ``8.10.7``, ``9.0.2``,
``9.2.8``, ``9.4.8`` and ``9.6.3`` do all build clash. With the latest version
in the next compiler series, with ``ghc-9.8.1``, clash fails to build.

    .. code-block:: Makefile

      GHC_VERSION ?= 9.6.3
      STACKAGE_VERSION ?= nightly-2023-11-25
      GHC_UPGRADE ?= 9.6.3
      STACKAGE_UPGRADE ?= nightly-2023-11-25

After adding basic Updo configuration, it errors out showing that some packages
are unknown for Stack.

    .. code-block:: pre

      $ make -f project-files.mk
      ...
      Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The
            'Stack configuration' refers to the set of package versions specified by the
            snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
            package is replaced, Stack prunes all other such packages that depend on it) and
            any extra-deps:
            
            In the dependencies for brick-0.50:
              * base must match <=4.13.0.0,
                but base-4.18.1.0 is in the Stack configuration (latest matching version is 4.13.0.0).
            needed due to clash-term-0.0.0 -> brick-0.50
            
            In the dependencies for hashable-1.3.5.0:
              * base must match >=4.5 && <4.17,
                but base-4.18.1.0 is in the Stack configuration (latest matching version is 4.16.4.0).
              * ghc-bignum must match >=1.0 && <1.1 || >=1.1 && <1.2 || >=1.2 && <1.3,
                but ghc-bignum-1.3 is in the Stack configuration (latest matching version is 1.2).
              * text must match >=0.12 && <1.3,
                but text-2.0.2 is in the Stack configuration (latest matching version is 1.2.5.0).
            needed due to clash-ghc-1.9.0 -> hashable-1.3.5.0
            
            In the dependencies for hedgehog-1.2:
              * ansi-terminal must match >=0.6 && <0.12,
                but ansi-terminal-1.0 is in the Stack configuration (latest matching version is 0.11.5).
            needed due to clash-lib-hedgehog-1.9.0 -> hedgehog-1.2
            
            Some different approaches to resolving some or all of this:
            
              * To ignore all version constraints and build anyway, in /.../.stack/config.yaml (global configuration) or
                /.../clash-compiler/stack.yaml (project-level configuration), set allow-newer: true.
              
              * To ignore certain version constraints and build anyway, also add these package
                names under allow-newer-deps: brick, hashable and hedgehog.

              * Build requires unattainable version of the base package.  Since base is a part
                of GHC, you most likely need to use a different GHC version with the matching
                base.

We can fix this by using the build we've already done with ``ghc-9.6.3``. We'll
freeze to see which versions Cabal choose and then use those versions for Stack
when there's a problem reported by Stack for a dependency. We're seeing problems with
``brick-0.50``, ``hashable-1.3.5.0`` and ``hedgehog-1.2``.

      .. code-block:: dhall

We can fix this by downloading the ``cabal.config`` from stackage and switching
our template from ``StackageWeb`` to ``StackageLocal``.

      .. code-block:: bash

        $ mkdir project-stackage
        $ curl https://www.stackage.org/nightly-2023-11-25/cabal.config > project-stackage/nightly-2023-11-25.config


The docs on Cabal's ``optional-packages`` say that packages listing here are
just like ``packages`` but the list can be empty. The intent is to allow for
globbing to turn up an empty list, something disallowed for ``packages``. This
is a problem for Updo. We can put optional packages verbatim into the Cabal text
snippet but this won't work for the Stack snippet because YAML map keys need to
be unique. We cannot generate a project with two ``packages`` keys.

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
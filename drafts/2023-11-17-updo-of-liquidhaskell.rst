---
title: Updo Liquid Haskell
subtitle: Walkthrough of converting Liquid Haskell to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting Liquid Haskell
=========================

Before the conversion[#]_, there is one Cabal project and many Stack projects.
The resolver ``lts-21.20``, that is ``ghc-9.4.7``. Both projects only build with
``ghc-9.4.7`` because of the resolver with Stack and because of the
``with-compiler: ghc-9.4.7`` setting in the Cabal project.

    .. code-block:: bash

      $ tree -P 'cabal.project*|stack.yaml*' --prune
      .
      ├── cabal.project
      ├── liquid-fixpoint
      │   ├── cabal.project
      │   ├── stack.yaml
      │   └── stack.yaml.lock
      ├── scripts
      │   └── plot-benchmarks
      │       └── stack.yaml
      ├── stack.yaml
      ├── stack.yaml.lock
      └── tests
          └── benchmarks
              └── stitch-lh
                  └── stack.yaml

    .. code-block:: pre

      $ make -f project-files.mk
      Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The
            'Stack configuration' refers to the set of package versions specified by the
            snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
            package is replaced, Stack prunes all other such packages that depend on it) and
            any extra-deps:
            
            In the dependencies for liquid-fixpoint-0.9.2.5(+devel +link-z3-as-a-library):
              * smtlib-backends must match >=0.3,
                but no version is in the Stack configuration (latest matching version is 0.3).
              * smtlib-backends-process must match >=0.3,
                but no version is in the Stack configuration (latest matching version is 0.3).
              * smtlib-backends-z3 must match >=0.3,
                but no version is in the Stack configuration (latest matching version is 0.3).
            needed since liquid-fixpoint is a build target.
            
            Some different approaches to resolving some or all of this:
            
              * Recommended action: try adding the following to your extra-deps in /.../liquidhaskell/stack.yaml
                (project-level configuration):
                
                - smtlib-backends-0.3@sha256:69977f97a8db2c11e97bde92fff7e86e793c1fb23827b284bf89938ee463fbf0,1211
                - smtlib-backends-process-0.3@sha256:3eee93e91f41c8a2fb2699e95b502a24d8053485ccf7749e2766683d1ebfe11d,1676
                - smtlib-backends-z3-0.3@sha256:b748fafd29eed0ea3f9e3924053c080196fad1eee5a73abc2f52e91f1dc19224,1907

To fix this we add those version constraints with ``@rev:..`` revisions but we
could equally well have used ``@sha256:..`` hashes for revisions as prompted to
by Stack.

    .. code-block:: dhall

      [ { dep = "smtlib-backends", ver = "0.3@rev:1" }
      , { dep = "smtlib-backends-process", ver = "0.3@rev:1" }
      , { dep = "smtlib-backends-z3", ver = "0.3@rev:1" }
      ]

A small bump, this example has an upgrade project for ``ghc-9.4.8``.

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
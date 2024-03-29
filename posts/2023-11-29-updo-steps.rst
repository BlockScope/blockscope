---
title: Introducing Updo.
subtitle: How to introduce Updo to a project using Cabal as an example.
tags: haskell, build
---

An earlier post `announced Updo </posts/2023-11-15-updo.html>`_. From the `Updo
Examples <up-do-examples_>`_, let's walk through the `Cabal <cabal_>`_ example
to see how to introduce Updo.

A few of benefits of Updo for `haskell/cabal <upstream-cabal_>`_ are:

- Package Groups
    Packages go into publish-to-hackage, test and benchmark groups.  Helpful to
    keep a project like this, with a lot of local packages, organized.
    
- Dual Projects
    Contributors get to pick Cabal or Stack as their build tool.
    
- Validated Dependencies
    With only a few exceptions listed explicitly as constraints, we get all
    dependencies from stackage, a set of packages that build together.


Projects, Before and After
==========================

There are many projects in the `haskell/cabal <upstream-cabal_>`_ repository:


    .. code-block:: bash

        $ tree -P 'cabal.project*|stack.yaml*' -L 1 --prune
        .
        ├── cabal.project
        ├── cabal.project.buildinfo
        ├── cabal.project.coverage
        ├── cabal.project.doctest
        ├── cabal.project.libonly
        ├── cabal.project.meta
        ├── cabal.project.release
        ├── cabal.project.validate
        ├── cabal.project.validate.libonly
        ├── cabal.project.weeder
        └── stack.yaml

        0 directories, 11 files

Before conversion, the Stack project doesn't build[#]_ but Updo
conversion will fix that by keeping both default projects in sync with an
upstream configuration.

.. note::

    A different approach is mirroring, where one project is maintained and then
    this (or a build product) is parsed and mirrored to the other project type.
    There are two tools that do this:

    - `stack2cabal <stack2cabal_>`_
        This tool converts ``stack.yaml`` to ``cabal.project``.

    - `cabal2stack <cabal2stack_>`_
        This tool converts ``plan.json`` to ``stack.yaml``.

        A ``plan.json`` can be created with ``cabal build --dry-run`` so be careful
        to include everything you need with this build. Enabling or disabling tests
        and benchmarks makes quite a difference in the generated ``stack.yaml``.

          .. code-block:: diff

            > - "Diff-0.5"
            > - "Glob-0.10.2"
            > - "Only-0.1"
            > - "base-compat-batteries-0.13.1"
            > - "binary-orphans-1.0.4.1"
            > - "call-stack-0.4.0"
            > - "cassava-0.5.3.0"
            > - "code-page-0.2.1"
            > - "criterion-1.6.3.0"
            > - "criterion-measurement-0.2.1.0"
            > - "happy-1.20.1.1"
            > - "haskell-lexer-1.1.1"
            > - "js-chart-2.9.4.1"
            > - "microstache-1.0.2.3"
            > - "nothunks-0.1.5"
            > - "pretty-show-1.10"
            < - "tasty-1.5"
            < - "tasty-quickcheck-0.10.3"
            > - "tasty-1.4.3"
            > - "tasty-expected-failure-0.12.3"
            > - "tasty-golden-2.3.5"
            > - "tasty-hunit-0.10.1"
            > - "tasty-quickcheck-0.10.2"
            > - "typed-process-0.2.11.1"
            > - "unbounded-delays-0.1.1.1"
            >   cassava:
            >     "bytestring--lt-0_10_4": false
            >   criterion:
            >     "embed-data-files": false
            >     fast: false
            >   "criterion-measurement":
            >     fast: false
            >   nothunks:
            >     bytestring: true
            >     text: true
            >     vector: true
            >   "tasty-golden":
            >     "build-example": fa


.. [#] The Stack project fails to construct a build plan[#]_.

    .. code-block:: pre

        $ stack build --test --no-run-tests --bench --no-run-benchmarks

        Warning: Ignoring cabal-install's bounds on
                 directory (>=1.3.7.0 && <1.4) and using directory-1.3.6.0.
                 Reason: allow-newer enabled.

        Warning: Ignoring hackage-security's bounds on
                 Cabal (>=1.14 && <1.26 || >=2.0 && <2.6 || >=3.0 && <3.7) and
                 using Cabal-3.11.0.0.
                 Reason: allow-newer enabled.

        Warning: Ignoring hackage-security's bounds on
                 Cabal-syntax (<3.7) and using Cabal-syntax-3.11.0.0.
                 Reason: allow-newer enabled.

        Warning: Ignoring cabal-install's bounds on
                 process (>=1.6.15.0 && <1.7) and using process-1.6.13.2.
                 Reason: allow-newer enabled.

        Warning: Ignoring cabal-testsuite's bounds on
                 Cabal (((>=3.10 && <3.11) && >=3.11.0.0 && <3.12) && >=3.10 && <3.11) and
                 using Cabal-3.11.0.0.
                 Reason: allow-newer enabled.

        Warning: Ignoring cabal-testsuite's bounds on
                 Cabal-syntax (((>=3.10 && <3.11) && >=3.11.0.0 && <3.12) && >=3.10 && <3.11) and
                 using Cabal-syntax-3.11.0.0.
                 Reason: allow-newer enabled.

        Warning: Ignoring cabal-testsuite's bounds on
                 retry (^>=0.9.1.0) and using retry-0.8.1.2.
                 Reason: allow-newer enabled.

        Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The
            'Stack configuration' refers to the set of package versions specified by the
            snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
            package is replaced, Stack prunes all other such packages that depend on it) and
            any extra-deps:
            
            In the dependencies for cabal-install-3.11.0.0:
                * semaphore-compat must match >=1.0.0 && <1.1,
                  but no version is in the Stack configuration (latest matching version is 1.0.0).
            needed since cabal-install is a build target.
            
            In the dependencies for cabal-testsuite-3:
                * network-wait must match ^>=0.1.2.0 || ^>=0.2.0.0,
                  but no version is in the Stack configuration (latest matching version is 0.2.0.0).
            needed since cabal-testsuite is a build target.
            
            In the dependencies for Cabal-tests-3:
                * nothunks must match >=0.1.1.0 && <0.2,
                  but no version is in the Stack configuration (latest matching version is 0.1.5).
            needed since Cabal-tests is a build target.
            
            Some different approaches to resolving some or all of this:
            
                * Recommended action: try adding the following to your extra-deps
                  in /.../cabal/stack.yaml (project-level configuration):
                
                - network-wait-0.2.0.0@sha256:c9fd76...
                - nothunks-0.1.5@sha256:ebe6c8...
                - semaphore-compat-1.0.0@sha256:8ed624...

After conversion we can build Cabal with Cabal itself as before but now we can
also build it with Stack!

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
        $ curl -sSL https://www.stackage.org/lts-21.19/cabal.config \
            > project-stackage/lts-21.19.config

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
        [ "Cabal-QuickCheck"
        , "Cabal-described"
        , "Cabal-tests"
        , "Cabal-tree-diff"
        , "cabal-testsuite"
        ]

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

.. _up-do-examples: https://github.com/orgs/up-do
.. _upstream-cabal: https://github.com/haskell/cabal
.. _cabal2stack: https://github.com/iconnect/cabal2stack 
.. _stack2cabal: https://github.com/hasufell/stack2cabal
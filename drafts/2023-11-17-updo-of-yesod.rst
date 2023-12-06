---
title: Updo Yesod
subtitle: Walkthrough of converting Yesod to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting Yesod
================

Before the conversion[#]_, there is one Stack project and one
Cabal project (the ``devel-example`` project is 7 years old).

    .. code-block:: bash

      $ tree -P 'cabal.project*|stack*.yaml*' --prune
      .
      ├── cabal.project
      ├── stack.yaml
      ├── stack.yaml.lock
      └── yesod-bin
          └── devel-example
              └── stack.yaml

      2 directories, 4 files

Only Stack is used in ``.github/workflows/tests.yml`` and the matrix uses resolvers.

    .. code-block:: yaml

      matrix:
        args:
        - "--resolver nightly-2022-02-11"  # ghc-9.0.2 = nightly-2022-02-11
        - "--resolver lts-18"              # ghc-8.10.7 = lts-18.28, published 2022-03-12
        - "--resolver lts-16"              # ghc-8.8.4 = lts-16.31, published 2021-01-17
        - "--resolver lts-14"              # ghc-8.6.5 = lts-14.27, published 2020-02-15
        - "--resolver lts-12"              # ghc-8.4.4 = lts-12.26, published 2018-12-30
        - "--resolver lts-11"              # ghc-8.2.2 = lts-11.22, published 2018-08-18

      run: |
          set -ex
          stack --version
          stack test --fast --no-terminal ${{ matrix.args }}

The Stack project builds with all the recent resolvers (``lts-18`` shown) but
not with ``lts-12`` or ``lts-11`` (``lts-12`` shown):

    .. code-block:: pre

      $ stack test --fast --resolver=lts-18
      Selected resolver: lts-18.28.
      Cabal file warning in /.../yesod-newsfeed.cabal@17:57: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /.../yesod-newsfeed.cabal@18:57: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /.../yesod-sitemap.cabal@17:62: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /.../yesod-sitemap.cabal@24:62: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      ...
      Completed 5 action(s).

      $ stack test --fast --resolver=lts-12
      ...
      Error: [S-4804]
        Stack failed to construct a build plan.
        
        While constructing the build plan, Stack encountered the following errors. The
        'Stack configuration' refers to the set of package versions specified by the
        snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
        package is replaced, Stack prunes all other such packages that depend on it) and
        any extra-deps:
        
        In the dependencies for attoparsec-aeson-2.1.0.0:
          * aeson must match >=1.4.1.0 && <2.2,
            but aeson-1.3.1.1 is in the Stack configuration (latest matching version is 2.1.2.1).
        needed due to yesod-auth-1.6.11.2 -> attoparsec-aeson-2.1.0.0
        
        Some different approaches to resolving some or all of this:
        
          * To ignore all version constraints and build anyway, in /.../.stack/config.yaml (global configuration) or
            /.../yesod/stack.yaml (project-level configuration), set allow-newer: true.
          
          * To ignore certain version constraints and build anyway, also add these package names under allow-newer-deps:
            attoparsec-aeson.
          
          * Recommended action: try adding the following to your extra-deps in /.../yesod/stack.yaml
            (project-level configuration):
            
            - aeson-2.1.2.1@sha256:5b8d62a60963a925c4d123a46e42a8e235a32188522c9f119f64ac228c2612a7,6359

The Cabal project doesn't build:

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Build profile: -w ghc-8.10.7 -O1
      ...
      Options.hs:70:14: error:
          • The constructor ‘CmdReader’ should have 2 arguments, but has been given 3
          • In the pattern: CmdReader _ cmds f
            In the pattern: Option (CmdReader _ cmds f) props
            In a stmt of a pattern guard for
                          an equation for ‘injectDefaultP’:
               (Option (CmdReader _ cmds f) props) <- o
         |
      70 |   | (Option (CmdReader _ cmds f) props) <- o  =
         |              ^^^^^^^^^^^^^^^^^^

Converting only for ``ghc-8.10.7``, we only need one constraint, for
``attoparsec-aeson`` and we choose an early version to not also have to upgrade
aeson.

    .. code-block:: pre

      $ make -f project-files.mk
      ...
      Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The 'Stack configuration'
            refers to the set of package versions specified by the snapshot (after any dropped packages, or pruned
            GHC boot packages; if a boot package is replaced, Stack prunes all other such packages that depend on
            it) and any extra-deps:
            
            In the dependencies for yesod-auth-1.6.11.2:
              * attoparsec-aeson must match >=2.1, but no version is in the Stack configuration (latest matching
                version is 2.2.0.1).
            needed since yesod-auth is a build target.
            
            In the dependencies for yesod-core-1.6.25.1:
              * attoparsec-aeson must match >=2.1, but no version is in the Stack configuration (latest matching
                version is 2.2.0.1).
            needed since yesod-core is a build target.
            
            Some different approaches to resolving some or all of this:
            
              * Recommended action: try adding the following to your extra-deps in
                /home/philderbeast/dev/src/updo/yesod/stack.yaml (project-level configuration):
                
                - attoparsec-aeson-2.2.0.1@sha256:00026bb205aaa087215a4c3a65a62c4561c3fb58e882778c9607c63e2aa960e9,1593

After adding a constraint, switching to ``StackageLocal``, downloading to
``project-stackage/lts-18.28.config`` and commenting out some conflicts, both
stack and cabal projects build.

    .. code-block:: dhall

      -- project-dhall/ghc-8.10.7/constraints.dhall
      [ { dep = "attoparsec-aeson", ver = "2.1.0.0" } ]

    .. code-block::

        -- project-stackage/lts-18.28.config
        constraints:
             ...
          -- yesod ==1.6.2,
          -- yesod-auth ==1.6.11,
             yesod-auth-hashdb ==1.7.1.7,
             yesod-auth-oauth2 ==0.6.3.4,
          -- yesod-bin ==1.6.2,
          -- yesod-core ==1.6.21.0,
             yesod-fb ==0.6.1,
          -- yesod-form ==1.7.0,
             yesod-gitrev ==0.2.2,
             yesod-markdown ==0.12.6.12,
             yesod-newsfeed ==1.7.0.0,
             yesod-page-cursor ==2.0.0.9,
             yesod-paginator ==1.1.1.0,
          -- yesod-persistent ==1.6.0.7,
             yesod-sitemap ==1.6.0,
             yesod-static ==1.6.1.0,
          -- yesod-test ==1.6.12,
          -- yesod-websockets ==0.3.0.3,
             yes-precure5-command ==5.5.3,

    .. code-block:: pre

      $ stack build --test --no-run-tests --bench --no-run-benchmarks

      Warning: WARNING: This stack project is generated.
      Cabal file warning in /home/philderbeast/dev/src/updo/yesod/yesod-newsfeed/yesod-newsfeed.cabal@17:57: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /home/philderbeast/dev/src/updo/yesod/yesod-newsfeed/yesod-newsfeed.cabal@18:57: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /home/philderbeast/dev/src/updo/yesod/yesod-sitemap/yesod-sitemap.cabal@17:62: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      Cabal file warning in /home/philderbeast/dev/src/updo/yesod/yesod-sitemap/yesod-sitemap.cabal@24:62: version operators used. To use version operators the package needs to specify at least 'cabal-version: >= 1.8'.
      yesod-core      > Test running disabled by --no-run-tests flag.
      yesod-form      > Test running disabled by --no-run-tests flag.
      yesod-static    > Test running disabled by --no-run-tests flag.
      yesod-persistent> Test running disabled by --no-run-tests flag.
      yesod-test      > Test running disabled by --no-run-tests flag.
      yesod-core      > Benchmark running disabled by --no-run-benchmarks flag.
      Completed 6 action(s).

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
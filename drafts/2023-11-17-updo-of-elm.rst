---
title: Updo Elm
subtitle: Walkthrough of converting the Elm Compiler to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting the Elm Compiler
===========================

The Elm compiler is a single package with few dependencies, all except one have
no version bounds.

    .. code-block:: yaml

      build-depends:
        HTTP, SHA, ansi-terminal, ansi-wl-pprint < 1, base, binary, bytestring,
        containers, directory, edit-distance, file-embed, filelock, filepath, ghc-prim,
        haskeline, http-client, http-client-tls, http-types, language-glsl, mtl,
        network, parsec, process, raw-strings-qq, scientific, snap-core, snap-server,
        template-haskell, time, unordered-containers, utf8-string, vector, zip-archive

There's no project but Cabal can build it with the latest GHC compiler,
``ghc-9.8.1``. For Stack, we can set the resolver to ``ghc-9.8.1`` as the
resolver but have to use this locally as we can't get it from stackage.

    .. code-block:: bash

      $ cabal build all --enable-tests --enable-benchmarks
      Error: [Cabal-7111]
      failed to download https://stackage.org/ghc-9.8.1/cabal.config : HTTP code 404

      $ mkdir project-stackage
      $ touch project-stackage/ghc-9.8.1.config

It can be empty and we have to manually set up constraints for
all packages for a Stack project as there isn't a resolver on stackage for it.


We can generate the Cabal project first and from that get the version equality
constraints from the freeze file.

    .. code-block:: pre

      $ cabal freeze
      Resolving dependencies...
      Wrote freeze file:
      /.../elm-compiler/cabal.project.freeze

    .. code-block:: cabal

      -- cabal.project.freeze
      active-repositories: hackage.haskell.org:merge
      constraints:  any.HTTP ==4000.4.1,
                    HTTP -conduit10 +network-uri -warn-as-error -warp-tests,
                    any.HUnit ==1.6.2.0,
                    any.SHA ==1.6.4.4,
                    SHA -exe,
                    any.ansi-terminal ==1.0,
                    ansi-terminal -example,
                    any.ansi-terminal-types ==0.11.5,
                    any.ansi-wl-pprint ==0.6.9,
                    ansi-wl-pprint -example,
                    any.appar ==0.1.8,
                    any.array ==0.5.5.0,
                    any.asn1-encoding ==0.9.6,
                    any.asn1-parse ==0.9.5,
                    any.asn1-types ==0.3.4,
                    any.async ==2.2.5,
                    async -bench,
                    any.attoparsec ==0.14.4,
                    attoparsec -developer,
                    any.base ==4.18.1.0,
                    any.base-orphans ==0.9.1,
                    any.base64-bytestring ==1.2.1.0,
                    any.basement ==0.0.16,
                    any.binary ==0.8.9.1,
                    any.blaze-builder ==0.4.2.3,
                    any.byteorder ==1.0.4,
                    any.bytestring ==0.11.5.2,
                    any.bytestring-builder ==0.10.8.2.0,
                    bytestring-builder +bytestring_has_builder,
                    any.call-stack ==0.4.0,
                    any.case-insensitive ==1.2.1.0,
                    any.cereal ==0.5.8.3,
                    cereal -bytestring-builder,
                    any.clock ==0.8.4,
                    clock -llvm,
                    any.colour ==2.3.6,
                    any.containers ==0.6.7,
                    any.cookie ==0.4.6,
                    any.crypton ==0.34,
                    crypton -check_alignment +integer-gmp -old_toolchain_inliner
                      +support_aesni +support_deepseq +support_pclmuldq
                      +support_rdrand -support_sse +use_target_attributes,
                    any.crypton-connection ==0.3.1,
                    any.crypton-x509 ==1.7.6,
                    any.crypton-x509-store ==1.6.9,
                    any.crypton-x509-system ==1.6.7,
                    any.crypton-x509-validation ==1.6.12,
                    any.data-default-class ==0.1.2.0,
                    any.deepseq ==1.4.8.1,
                    any.digest ==0.0.1.7,
                    digest +pkg-config,
                    any.directory ==1.3.8.1,
                    any.edit-distance ==0.2.2.1,
                    elm -dev,
                    any.exceptions ==0.10.7,
                    any.file-embed ==0.0.15.0,
                    any.filelock ==0.1.1.7,
                    any.filepath ==1.4.100.4,
                    any.ghc-bignum ==1.3,
                    any.ghc-boot-th ==9.6.3,
                    any.ghc-prim ==0.10.0,
                    any.hashable ==1.4.3.0,
                    hashable +integer-gmp -random-initial-seed,
                    any.haskeline ==0.8.2.1,
                    any.hourglass ==0.2.12,
                    any.hsc2hs ==0.68.10,
                    hsc2hs -in-ghc-tree,
                    any.http-client ==0.7.15,
                    http-client +network-uri,
                    any.http-client-tls ==0.3.6.3,
                    any.http-types ==0.12.3,
                    any.integer-gmp ==1.1,
                    any.integer-logarithms ==1.0.3.1,
                    integer-logarithms -check-bounds +integer-gmp,
                    any.io-streams ==1.5.2.2,
                    io-streams +network -nointeractivetests +zlib,
                    any.io-streams-haproxy ==1.0.1.0,
                    any.iproute ==1.7.12,
                    any.language-glsl ==0.3.0,
                    any.lifted-base ==0.2.3.12,
                    any.memory ==0.18.0,
                    memory +support_bytestring +support_deepseq,
                    any.mime-types ==0.1.2.0,
                    any.monad-control ==1.0.3.1,
                    any.mtl ==2.3.1,
                    any.network ==3.1.4.0,
                    network -devel,
                    any.network-uri ==2.6.4.2,
                    any.old-locale ==1.0.0.7,
                    any.old-time ==1.1.0.3,
                    any.parsec ==3.1.16.1,
                    any.pem ==0.2.4,
                    any.pretty ==1.1.3.6,
                    any.prettyclass ==1.0.0.0,
                    any.primitive ==0.9.0.0,
                    any.process ==1.6.17.0,
                    any.random ==1.2.1.1,
                    any.raw-strings-qq ==1.1,
                    any.readable ==0.3.1,
                    any.regex-base ==0.94.0.2,
                    any.regex-posix ==0.96.0.1,
                    regex-posix -_regex-posix-clib,
                    any.rts ==1.0.2,
                    any.scientific ==0.3.7.0,
                    scientific -bytestring-builder -integer-simple,
                    any.snap-core ==1.0.5.1,
                    snap-core -debug +network-uri -portable,
                    any.snap-server ==1.1.2.1,
                    snap-server -build-pong -build-testserver -debug -openssl -portable,
                    any.socks ==0.6.1,
                    any.splitmix ==0.1.0.5,
                    splitmix -optimised-mixer,
                    any.stm ==2.5.1.0,
                    any.streaming-commons ==0.2.2.6,
                    streaming-commons -use-bytestring-builder,
                    any.template-haskell ==2.20.0.0,
                    any.terminfo ==0.4.1.6,
                    any.text ==2.0.2,
                    any.th-compat ==0.1.4,
                    any.time ==1.12.2,
                    any.tls ==1.9.0,
                    tls +compat -hans +network,
                    any.transformers ==0.6.1.0,
                    any.transformers-base ==0.4.6,
                    transformers-base +orphaninstances,
                    any.transformers-compat ==0.7.2,
                    transformers-compat -five +five-three -four +generic-deriving +mtl -three -two,
                    any.unix ==2.8.1.0,
                    any.unix-compat ==0.7,
                    unix-compat -old-time,
                    any.unix-time ==0.4.11,
                    any.unordered-containers ==0.2.19.1,
                    unordered-containers -debug,
                    any.utf8-string ==1.0.2,
                    any.vector ==0.13.1.0,
                    vector +boundschecks -internalchecks -unsafechecks -wall,
                    any.vector-stream ==0.1.0.0,
                    any.zip-archive ==0.4.3,
                    zip-archive -executable,
                    any.zlib ==0.6.3.0,
                    zlib -bundled-c-zlib -non-blocking-ffi -pkg-config,
                    any.zlib-bindings ==0.1.1.5
      index-state: hackage.haskell.org 2023-11-28T07:37:28Z


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
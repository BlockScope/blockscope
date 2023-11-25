---
title: How to Updo Futhark
subtitle: Walkthrough of converting Futhark to use Updo
tags: haskell, build
---

From the examples[#]_ ...

.. [#] Find examples as `github/up-do <up-do_>`_ repositories


Converting Futhark
==================

Before the conversion[#]_, there's on Stack project. The Cabal project builds
and we can choose any one of a number of versions of GHC to do this. The latest
version of stackage, ``nightly-2023-11-25`` only supports ``ghc-9.6.3`` but we
can successfully build with ``ghc-9.8.1``. To get around this gap, we'll make
the "current" version ``ghc-9.6.3`` and the "upgrade" version ``ghc-9.8.1``. For
now and to get started we'll use the same set of versions for "upgrade" as we're
using for "current".

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
      stack build --test --no-run-tests --bench --no-run-benchmarks --dry-run --stack-yaml stack.yaml

      Warning: WARNING: This stack project is generated.

      Error: [S-4804]
            Stack failed to construct a build plan.
            
            While constructing the build plan, Stack encountered the following errors. The
            'Stack configuration' refers to the set of package versions specified by the
            snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
            package is replaced, Stack prunes all other such packages that depend on it) and
            any extra-deps:
            
            In the dependencies for futhark-0.26.0:
            * fgl-visualize needed,
              but no version is in the Stack configuration (latest matching version is 0.1.0.1).

            * futhark-data must match >=1.1.0.0,
              but no version is in the Stack configuration (latest matching version is 1.1.0.1).

            * futhark-manifest must match >=1.2.0.1,
              but no version is in the Stack configuration (latest matching version is 1.2.0.1).

            * futhark-server must match >=1.2.2.1,
              but no version is in the Stack configuration (latest matching version is 1.2.2.1).

            needed since futhark is a build target.
            
            Some different approaches to resolving some or all of this:
            
            * Recommended action: try adding the following to your extra-deps in /.../futhark/stack.yaml
            (project-level configuration):
            
            - fgl-visualize-0.1.0.1@sha256:e682066053a6e75478a08fd6822dd0143a3b8ea23244bdb01dd389a266447c5e,995
            - futhark-data-1.1.0.1@sha256:20dc734aca2d1c25366b6e113d8736c4b68e100123c2316c6add2164a107cbad,2368
            - futhark-manifest-1.2.0.1@sha256:f3f8b4ca666134d8ffd3de716d7f914e6fa8c3c2107080f1c7da58a372fbc5f1,1726
            - futhark-server-1.2.2.1@sha256:a96255659522ccda61df540960eb26a8b1f1526e77c66f38e5ebc3a39632dda4,1357

We can fix this by adding those version constraints.

      .. code-block:: dhall

        -- project-dhall/ghc-9.6.3/constraints.dhall
        [ { dep = "fgl-visualize", ver = "0.1.0.1" }
        , { dep = "futhark-data", ver = "1.1.0.1" }
        , { dep = "futhark-manifest", ver = "1.2.0.1" }
        , { dep = "futhark-server", ver = "1.2.2.1" }
        ]

That done, the "current" projects both build:

    .. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks
      Resolving dependencies...
      Build profile: -w ghc-9.6.3 -O1
      ...
      $ cabal build all --enable-tests --enable-benchmarks
      Up to date

      $ stack build --test --no-run-tests --bench --no-run-benchmarks
      ...
      Completed 151 action(s).

Upgrade Project with Empty Stackage Config
==========================================

For the "upgrade", we can use an empty local stackage config file.

.. code-block:: pre

      $ mkdir project-stackage
      $ touch project-stackage/empty.config

Building only the "upgrade" Cabal project, we get version conflicts for ``versions:base``:

.. code-block:: pre

      $ cabal build all --enable-tests --enable-benchmarks --project-file=cabal.upgrade.project
      Warning: Unknown/unsupported 'ghc' version detected (Cabal 3.11.0.0 supports
      'ghc' version < 9.8): /.../.ghcup/bin/ghc is version 9.8.1
      Resolving dependencies...
      Error: [Cabal-7107]
      Could not resolve dependencies:
      [__0] trying: futhark-0.26.0 (user goal)
      [__1] trying: versions-6.0.3 (dependency of futhark)
      [__2] next goal: base (dependency of futhark)
      [__2] rejecting: base-4.19.0.0/installed-f417 (conflict: versions => base>=4.10 && <4.19)
      [__2] skipping: base-4.19.0.0
            (has the same characteristics that caused the previous version to fail: excluded by constraint '>=4.10 && <4.19' from 'versions')
      [__2] rejecting: base-4.18.1.0, ..., base-3.0.3.1
            (constraint from non-reinstallable package requires installed instance)
      [__2] fail (backjumping, conflict set: base, futhark, versions)
      After searching the rest of the dependency tree exhaustively, these were
      the goals I've had most trouble fulfilling: base, versions, futhark

We'll fix this by allowing newer versions, as was done before the Updo upgrade,
putting these into the cabal snippet.

      .. code-block:: cabal

            allow-newer:
                versions:base
              , versions:text
              , generic-lens:text
              , generic-lens-core:text

Now we can build the "upgrade" Cabal project:

      .. code-block:: bash

            $ cabal build all --enable-tests --enable-benchmarks --project-file=cabal.upgrade.project
            Resolving dependencies...
            Build profile: -w ghc-9.8.1 -O1
            ...
            $ cabal build all --enable-tests --enable-benchmarks --project-file=cabal.upgrade.project
            Up to date

Upgrade Project with Compiler Resolver
======================================

      .. code-block:: pre

            Error: [S-4804]
                  Stack failed to construct a build plan.
                  
                  While constructing the build plan, Stack encountered the following errors. The
                  'Stack configuration' refers to the set of package versions specified by the
                  snapshot (after any dropped packages, or pruned GHC boot packages; if a boot
                  package is replaced, Stack prunes all other such packages that depend on it) and
                  any extra-deps:
                  
                  In the dependencies for fgl-visualize-0.1.0.1:
                  * dotgen must match >=0.4,
                    but no version is in the Stack configuration (latest matching version is 0.4.3).
                  * fgl must match >=5.4,
                    but no version is in the Stack configuration (latest matching version is 5.8.2.0).
                  needed due to futhark-0.26.0 -> fgl-visualize-0.1.0.1
                  
                  In the dependencies for futhark-0.26.0:
                  * Diff must match >=0.4.1,
                    but no version is in the Stack configuration (latest matching version is 0.5).
                  * aeson must match >=2.0.0.0,
                    but no version is in the Stack configuration (latest matching version is 2.2.1.0).
                  * alex needed,
                    but no version is in the Stack configuration (latest matching version is 3.4.0.1).
                  * ansi-terminal must match >=0.6.3.1,
                    but no version is in the Stack configuration (latest matching version is 1.0).
                  * base16-bytestring needed,
                    but no version is in the Stack configuration (latest matching version is 1.0.2.0).
                  * blaze-html must match >=0.9.0.1,
                    but no version is in the Stack configuration (latest matching version is 0.9.1.2).
                  * bmp must match >=1.2.6.3,
                    but no version is in the Stack configuration (latest matching version is 1.2.6.3).
                  * bytestring-to-vector must match >=0.3.0.1,
                    but no version is in the Stack configuration (latest matching version is 0.3.0.1).
                  * cmark-gfm must match >=0.2.1,
                    but no version is in the Stack configuration (latest matching version is 0.2.6).
                  * co-log-core needed,
                    but no version is in the Stack configuration (latest matching version is 0.3.2.1).
                  * cryptohash-md5 needed,
                    but no version is in the Stack configuration (latest matching version is 0.11.101.0).
                  * directory-tree must match >=0.12.1,
                    but no version is in the Stack configuration (latest matching version is 0.12.1).
                  * dlist must match >=0.6.0.1,
                    but no version is in the Stack configuration (latest matching version is 1.0).
                  * fgl needed,
                    but no version is in the Stack configuration (latest matching version is 5.8.2.0).
                  * file-embed must match >=0.0.14.0,
                    but no version is in the Stack configuration (latest matching version is 0.0.15.0).
                  * free must match >=5.1.10,
                    but no version is in the Stack configuration (latest matching version is 5.2).
                  * githash must match >=0.1.6.1,
                    but no version is in the Stack configuration (latest matching version is 0.1.7.0).
                  * half must match >=0.3,
                    but no version is in the Stack configuration (latest matching version is 0.3.1).
                  * happy needed,
                    but no version is in the Stack configuration (latest matching version is 1.20.1.1).
                  * language-c-quote must match >=0.12,
                    but no version is in the Stack configuration (latest matching version is 0.13.0.1).
                  * lens needed,
                    but no version is in the Stack configuration (latest matching version is 5.2.3).
                  * lsp must match >=2.2.0.0,
                    but no version is in the Stack configuration (latest matching version is 2.3.0.0).
                  * lsp-types must match >=2.0.1.0,
                    but no version is in the Stack configuration (latest matching version is 2.1.0.0).
                  * mainland-pretty must match >=0.7.1,
                    but no version is in the Stack configuration (latest matching version is 0.7.1).
                  * megaparsec must match >=9.0.0,
                    but no version is in the Stack configuration (latest matching version is 9.6.1).
                  * mwc-random needed,
                    but no version is in the Stack configuration (latest matching version is 0.15.0.2).
                  * neat-interpolation must match >=0.3,
                    but no version is in the Stack configuration (latest matching version is 0.5.1.4).
                  * parallel must match >=3.2.1.0,
                    but no version is in the Stack configuration (latest matching version is 3.2.2.0).
                  * prettyprinter must match >=1.7,
                    but no version is in the Stack configuration (latest matching version is 1.7.1).
                  * prettyprinter-ansi-terminal must match >=1.1,
                    but no version is in the Stack configuration (latest matching version is 1.1.3).
                  * process-extras must match >=0.7.2,
                    but no version is in the Stack configuration (latest matching version is 0.7.4).
                  * random must match >=1.2.0,
                    but no version is in the Stack configuration (latest matching version is 1.2.1.1).
                  * regex-tdfa must match >=1.2,
                    but no version is in the Stack configuration (latest matching version is 1.3.2.2).
                  * srcloc must match >=0.4,
                    but no version is in the Stack configuration (latest matching version is 0.6.0.1).
                  * statistics needed,
                    but no version is in the Stack configuration (latest matching version is 0.16.2.1).
                  * temporary needed,
                    but no version is in the Stack configuration (latest matching version is 1.3).
                  * terminal-size must match >=0.3,
                    but no version is in the Stack configuration (latest matching version is 0.3.4).
                  * vector must match >=0.12,
                    but no version is in the Stack configuration (latest matching version is 0.13.1.0).
                  * versions must match >=6.0.0,
                    but no version is in the Stack configuration (latest matching version is 6.0.3).
                  * zlib must match >=0.6.1.2,
                    but no version is in the Stack configuration (latest matching version is 0.6.3.0).
                  needed since futhark is a build target.
                  
                  In the dependencies for futhark-data-1.1.0.1:
                  * bytestring-to-vector must match >=0.3.0.1,
                    but no version is in the Stack configuration (latest matching version is 0.3.0.1).
                  * half must match >=0.3,
                    but no version is in the Stack configuration (latest matching version is 0.3.1).
                  * megaparsec must match >=9.0.0,
                    but no version is in the Stack configuration (latest matching version is 9.6.1).
                  * scientific must match >=0.3.6,
                    but no version is in the Stack configuration (latest matching version is 0.3.7.0).
                  * vector must match >=0.12,
                    but no version is in the Stack configuration (latest matching version is 0.13.1.0).
                  * vector-binary-instances must match >=0.2.2.0,
                    but no version is in the Stack configuration (latest matching version is 0.2.5.2).
                  needed due to futhark-0.26.0 -> futhark-data-1.1.0.1
                  
                  In the dependencies for futhark-manifest-1.2.0.1:
                  * aeson must match >=2.0.0.0,
                    but no version is in the Stack configuration (latest matching version is 2.2.1.0).
                  needed due to futhark-0.26.0 -> futhark-manifest-1.2.0.1
                  
                  In the dependencies for futhark-server-1.2.2.1:
                  * temporary needed,
                    but no version is in the Stack configuration (latest matching version is 1.3).
                  needed due to futhark-0.26.0 -> futhark-server-1.2.2.1
                  
                  Some different approaches to resolving some or all of this:
                  
                  * Recommended action: try adding the following to your extra-deps in /.../futhark/stack.upgrade.yaml
                  (project-level configuration):
                  
                  - Diff-0.5@sha256:9c8a972eead9e079b90c6581fa8ef5755662c10dd075951b32b801145704afbb,1814
                  - aeson-2.2.1.0@sha256:a23a61aada8233e10573e1612c0b2efe5a1aba0d59b05dbe2f63301822f136cb,6582
                  - alex-3.4.0.1@sha256:9d88e2463b268dfd41e5c9d6964e28a1532bae3c70241de85460b84a4b5ac528,3886
                  - ansi-terminal-1.0@sha256:640ffecfd95471388d939fcacb57bdc0cef15f0457746c234a12cdd5a6c6d1e8,2706
                  - base16-bytestring-1.0.2.0@sha256:a694e88f9ec9fc79f0b03f233d3fea592b68f70a34aac2ddb5bcaecb6562e2fd,2630
                  - blaze-html-0.9.1.2@sha256:2e40ad3828320b72122f09754091fb686fa0fd4c083769f17ef84584972ec450,3020
                  - bmp-1.2.6.3@sha256:93901b0e0e13bd729207eca5963bbd4eb95ebbcd74f13c5646bb7cd7e91c0c3e,1395
                  - bytestring-to-vector-0.3.0.1@sha256:fb902424b3fdcc31a4b37b9071a5fa224c07f044df8b411fa1a5c2373ff30d7f,1689
                  - cmark-gfm-0.2.6@sha256:8672b9388f5ddfa8ece691e59b4272fa807a2ddf0698970cd73af9bebb98058d,5307
                  - co-log-core-0.3.2.1@sha256:09140377d273593820e8f1fb69b377fa8bce917765d2df74ade21885c8bd81c0,4035
                  - cryptohash-md5-0.11.101.0@sha256:71a6e856a4ce0b844a27eb4ba58e214e4263ffbde9c8f406eed3f9a43ad8efec,3080
                  - directory-tree-0.12.1@sha256:6f24ee1eca965a199a96f853582e61a338f1a529c87bc02c7f1451e2f2ed03fe,3170
                  - dlist-1.0@sha256:55ff69d20ce638fc7727342ee67f2f868da61d3dcf3763f790bf9aa0b145e568,3812
                  - dotgen-0.4.3@sha256:6045b1243f1b716d0cd67198f0cd75d96db9f63c5cc0826d05e0dc855fcbf9d0,1874
                  - fgl-5.8.2.0@sha256:9bc966528db0a9bd57c63dd3cb2bbe08e1a2d2de7ac7f517aaaa5d79607f759e,3796
                  - file-embed-0.0.15.0@sha256:18beed8999dff37bcffd0d1d8a59dcd406be8e517c10213afefc941091985c06,1426
                  - free-5.2@sha256:4dd76c1a19f81d7866e7b75682350f1ef3e0441b19b4d07eccdd9ef326a0be70,4176
                  - githash-0.1.7.0@sha256:cb405aed03e1da9a5ea7601e4d7eea3739104a8a6e47ac4970e079d616326836,1527
                  - half-0.3.1@sha256:f43f16671b42bdc92b4be9e0b0ce1bcff817c928d0a50f13a6264a24586c1a7c,2158
                  - happy-1.20.1.1@sha256:a381633c5e8f9e9e5a8e1900930ce13172397b4677fcfcc08cd38eb3f73b61b1,5811
                  - language-c-quote-0.13.0.1@sha256:f2b7613339a4fe947c8db3fb5943b71fd07d0f8b05978156684db9dce4b7776a,3950
                  - lens-5.2.3@sha256:637287c76adff383063b3206a4213640de1a74839ec16008cc71b5b407f7d05e,15237
                  - lsp-2.3.0.0@sha256:d61c26c931b03ed2d115729ea7bf94e9cc91761278199b466f4f8110b0704c2f,3700
                  - lsp-types-2.1.0.0@sha256:271e80ad8a51c46a61e2ca18dd3454d4bd1a9b6e0f2aa15d6cbad9e2984da69b,29541
                  - mainland-pretty-0.7.1@sha256:1b6161f258f8e00e979ccc7410c97be3601f07c8f37d86b8672e440e7ce55773,1733
                  - megaparsec-9.6.1@sha256:8d8f8ee5aca5d5c16aa4219afd13687ceab8be640f40ba179359f2b42a628241,3323
                  - mwc-random-0.15.0.2@sha256:109e0fb72ce64bda468fc44d9cb5abbf455d6337140b57eb851a8183baba0597,3372
                  - neat-interpolation-0.5.1.4@sha256:6ca5e3a763c841861b0449abecf0bca4a80e5dc0ed397f2272cfe70ed145970c,3293
                  - parallel-3.2.2.0@sha256:b993406e98fe9126eff4a69d28e7e361037dd1c0892a9fd6c26e1f92eb9c4fa4,1926
                  - prettyprinter-1.7.1@sha256:9c43c9d8c3cd9f445596e5a2379574bba87f935a4d1fa41b5407ee3cf4edc743,6987
                  - prettyprinter-ansi-terminal-1.1.3@sha256:b00f727b964cf13adff7acf575a71fa7e405f5240aff50b07f10df76ab89f8d3,2574
                  - process-extras-0.7.4@sha256:4e79289131415796c181889c4a226ebab7fc3b0d27b164f65e1aad123ae9b9e3,1759
                  - random-1.2.1.1@sha256:e7c1f881159d5cc788619c9ee8b8f340ba2ff0db571cdf3d1a1968ebc5108789,6777
                  - regex-tdfa-1.3.2.2@sha256:92afd144189801dff0fa2544b55c7b6c7c7e556c10dddfb61f2d75909f68af98,6997
                  - scientific-0.3.7.0@sha256:909755ab19b453169ff85281323da1488407776b2360bd9f7afdd219fd306ef2,4869
                  - srcloc-0.6.0.1@sha256:4356a7f46ef7bf135d61fe58e680954a6d5b58e8030aa153adbc8da6e8243e20,988
                  - statistics-0.16.2.1@sha256:164523ee07f40b60f1a2e7149ddd2d42729b9076ad6a36fd7a188c89147691c2,5692
                  - temporary-1.3@sha256:3a66c136f700dbf42f3c5000ca93e80b26dead51e54322c83272b236c1ec8ef1,1485
                  - terminal-size-0.3.4@sha256:f0318c54273d04afb65109683b442792dcb67af1ad01ab5ec64423a28bb97715,1291
                  - vector-0.13.1.0@sha256:4650d28eb918812a3158130f833b5ff5020259b28a8f9ee5d28701ce60cf8a16,8997
                  - vector-binary-instances-0.2.5.2@sha256:9ba8f2c5a9527821ab47bbd991dd7b7533bcaa68662c84c4f16b871655117ceb,2728
                  - versions-6.0.3@sha256:bed42e22d8076f07b656bfb3f4bf4aebcc07c88688b5cdad395329fa433d90b1,1985
                  - zlib-0.6.3.0@sha256:19eb7759af71957811d5ec10ddb1e2f4c98700ddb9c0da6860c0441d811f0e6d,5325

Having made those changes, we try again. This cycle goes on many times until we
get a successful make with a lot of constraints.

      .. code-block:: dhall

            -- project-dhall/ghc-9.8.1-compiler/constraints.dhall
            [ { dep = "fgl-visualize", ver = "0.1.0.1" }
            , { dep = "futhark-data", ver = "1.1.0.1" }
            , { dep = "futhark-manifest", ver = "1.2.0.1" }
            , { dep = "futhark-server", ver = "1.2.2.1" }
            -- 1st group of recommendations
            , { dep = "Diff", ver ="0.5" }
            , { dep = "aeson", ver ="2.2.1.0" }
            , { dep = "alex", ver ="3.4.0.1" }
            , { dep = "ansi-terminal", ver ="1.0" }
            , { dep = "base16-bytestring", ver ="1.0.2.0" }
            , { dep = "blaze-html", ver ="0.9.1.2" }
            , { dep = "bmp", ver ="1.2.6.3" }
            , { dep = "bytestring-to-vector", ver ="0.3.0.1" }
            , { dep = "cmark-gfm", ver ="0.2.6" }
            , { dep = "co-log-core", ver ="0.3.2.1" }
            , { dep = "cryptohash-md5", ver ="0.11.101.0" }
            , { dep = "directory-tree", ver ="0.12.1" }
            , { dep = "dlist", ver ="1.0" }
            , { dep = "dotgen", ver ="0.4.3" }
            , { dep = "fgl", ver ="5.8.2.0" }
            , { dep = "file-embed", ver ="0.0.15.0" }
            , { dep = "free", ver ="5.2" }
            , { dep = "githash", ver ="0.1.7.0" }
            , { dep = "half", ver ="0.3.1" }
            , { dep = "happy", ver ="1.20.1.1" }
            , { dep = "language-c-quote", ver ="0.13.0.1" }
            , { dep = "lens", ver ="5.2.3" }
            , { dep = "lsp", ver ="2.3.0.0" }
            , { dep = "lsp-types", ver ="2.1.0.0" }
            , { dep = "mainland-pretty", ver ="0.7.1" }
            , { dep = "megaparsec", ver ="9.6.1" }
            , { dep = "mwc-random", ver ="0.15.0.2" }
            , { dep = "neat-interpolation", ver ="0.5.1.4" }
            , { dep = "parallel", ver ="3.2.2.0" }
            , { dep = "prettyprinter", ver ="1.7.1" }
            , { dep = "prettyprinter-ansi-terminal", ver ="1.1.3" }
            , { dep = "process-extras", ver ="0.7.4" }
            , { dep = "random", ver ="1.2.1.1" }
            , { dep = "regex-tdfa", ver ="1.3.2.2" }
            , { dep = "scientific", ver ="0.3.7.0" }
            , { dep = "srcloc", ver ="0.6.0.1" }
            , { dep = "statistics", ver ="0.16.2.1" }
            , { dep = "temporary", ver ="1.3" }
            , { dep = "terminal-size", ver ="0.3.4" }
            , { dep = "vector", ver ="0.13.1.0" }
            , { dep = "vector-binary-instances", ver ="0.2.5.2" }
            , { dep = "versions", ver ="6.0.3" }
            , { dep = "zlib", ver ="0.6.3.0" }
            -- 2nd group of recommendations
            , { dep = "ListLike", ver ="4.7.8.2" }
            , { dep = "OneTuple", ver ="0.4.1.1" }
            , { dep = "QuickCheck", ver ="2.14.3" }
            , { dep = "ansi-terminal-types", ver ="0.11.5" }
            , { dep = "assoc", ver ="1.1" }
            , { dep = "async", ver ="2.2.4" }
            , { dep = "attoparsec", ver ="0.14.4" }
            , { dep = "base-orphans", ver ="0.9.1" }
            , { dep = "bifunctors", ver ="5.6.1" }
            , { dep = "blaze-builder", ver ="0.4.2.3" }
            , { dep = "blaze-markup", ver ="0.8.3.0" }
            , { dep = "call-stack", ver ="0.4.0" }
            , { dep = "case-insensitive", ver ="1.2.1.0" }
            , { dep = "colour", ver ="2.3.6" }
            , { dep = "comonad", ver ="5.0.8" }
            , { dep = "contravariant", ver ="1.5.5" }
            , { dep = "data-default", ver ="0.7.1.1" }
            , { dep = "data-default-class", ver ="0.1.2.0" }
            , { dep = "data-fix", ver ="0.3.2" }
            , { dep = "dense-linear-algebra", ver ="0.1.0.0" }
            , { dep = "distributive", ver ="0.6.2.1" }
            , { dep = "exception-mtl", ver ="0.4.0.2" }
            , { dep = "exception-transformers", ver ="0.4.0.12" }
            , { dep = "generic-deriving", ver ="1.14.5" }
            , { dep = "generically", ver ="0.1.1" }
            , { dep = "hashable", ver ="1.4.3.0" }
            , { dep = "haskell-src-meta", ver ="0.8.13" }
            , { dep = "indexed-traversable", ver ="0.1.3" }
            , { dep = "indexed-traversable-instances", ver ="0.1.1.2" }
            , { dep = "integer-conversion", ver ="0.1.0.1" }
            , { dep = "integer-logarithms", ver ="1.0.3.1" }
            , { dep = "kan-extensions", ver ="5.2.5" }
            , { dep = "lens-aeson", ver ="1.2.3" }
            , { dep = "math-functions", ver ="0.3.4.3" }
            , { dep = "mod", ver ="0.2.0.1" }
            , { dep = "network-uri", ver ="2.6.4.2" }
            , { dep = "parser-combinators", ver ="1.3.0" }
            , { dep = "primitive", ver ="0.9.0.0" }
            , { dep = "profunctors", ver ="5.6.2" }
            , { dep = "reflection", ver ="2.1.7" }
            , { dep = "regex", ver ="1.1.0.2" }
            , { dep = "regex-base", ver ="0.94.0.2" }
            , { dep = "row-types", ver ="1.0.1.2" }
            , { dep = "safe", ver ="0.3.19" }
            , { dep = "semialign", ver ="1.3" }
            , { dep = "semigroupoids", ver ="6.0.0.1" }
            , { dep = "some", ver ="1.0.6" }
            , { dep = "sorted-list", ver ="0.2.2.0" }
            , { dep = "splitmix", ver ="0.1.0.5" }
            , { dep = "strict", ver ="0.5" }
            , { dep = "syb", ver ="0.7.2.4" }
            , { dep = "tagged", ver ="0.8.8" }
            , { dep = "text-iso8601", ver ="0.1" }
            , { dep = "text-rope", ver ="0.2" }
            , { dep = "text-short", ver ="0.1.5" }
            , { dep = "th-abstraction", ver ="0.6.0.0" }
            , { dep = "th-compat", ver ="0.1.4" }
            , { dep = "these", ver ="1.2" }
            , { dep = "time-compat", ver ="1.9.6.1" }
            , { dep = "transformers-base", ver ="0.4.6" }
            , { dep = "transformers-compat", ver ="0.7.2" }
            , { dep = "unliftio-core", ver ="0.2.1.0" }
            , { dep = "unordered-containers", ver ="0.2.19.1" }
            , { dep = "uuid", ver ="1.3.15" }
            , { dep = "uuid-types", ver ="1.0.5.1" }
            , { dep = "vector-algorithms", ver ="0.9.0.1" }
            , { dep = "vector-stream", ver ="0.1.0.0" }
            , { dep = "vector-th-unbox", ver ="0.2.2" }
            , { dep = "witherable", ver ="0.4.2" }
            -- 3rd group of recommendations
            , { dep = "StateVar", ver ="1.2.2" }
            , { dep = "adjunctions", ver ="4.4.2" }
            , { dep = "base-compat", ver ="0.13.1" }
            , { dep = "bitvec", ver ="1.1.5.0" }
            , { dep = "constraints", ver ="0.14" }
            , { dep = "cryptohash-sha1", ver ="0.11.101.0" }
            , { dep = "data-default-instances-containers", ver ="0.0.1" }
            , { dep = "data-default-instances-dlist", ver ="0.0.1" }
            , { dep = "data-default-instances-old-locale", ver ="0.0.1" }
            , { dep = "entropy", ver ="0.4.1.10" }
            , { dep = "fail", ver ="4.9.0.0" }
            , { dep = "fmlist", ver ="0.9.4" }
            , { dep = "generic-lens", ver ="2.2.2.0" }
            , { dep = "haskell-src-exts", ver ="1.23.1" }
            , { dep = "invariant", ver ="0.6.2" }
            , { dep = "network-info", ver ="0.2.1" }
            , { dep = "regex-pcre-builtin", ver ="0.95.2.3.8.44" }
            , { dep = "semirings", ver ="0.6" }
            , { dep = "th-orphans", ver ="0.13.14" }
            , { dep = "time-locale-compat", ver ="0.1.1.5" }
            , { dep = "utf8-string", ver ="1.0.2" }
            -- 4th group of recommendations
            , { dep = "base-compat-batteries", ver ="0.13.1" }
            , { dep = "boring", ver ="0.2.1" }
            , { dep = "generic-lens-core", ver ="2.2.1.0" }
            , { dep = "old-locale", ver ="1.0.0.7" }
            , { dep = "semigroups", ver ="0.20" }
            , { dep = "th-lift", ver ="0.8.4" }
            , { dep = "th-reify-many", ver ="0.1.10" }
            , { dep = "type-equality", ver ="1" }
            , { dep = "void", ver ="0.7.3" }
            -- 5th group of recommendations
            , { dep = "indexed-profunctors", ver ="0.1.1.1" }
            , { dep = "th-expand-syns", ver ="0.4.11.0" }
            -- 6th group of recommendations
            , { dep = "tasty", ver ="1.5" }
            , { dep = "tasty-hunit", ver ="0.10.1" }
            , { dep = "tasty-quickcheck", ver ="0.10.3" }
            -- 7th group of recommendations
            , { dep = "optparse-applicative", ver = "0.18.1.0" }
            ]


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
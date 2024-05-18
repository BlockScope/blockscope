---
title: Updoing hledger
subtitle: Converting hledger to use Updo for project generation.
tags: haskell, updo
---

I've had `hledger` on the list of projects [proposed for
conversion](https://github.com/up-do#proposed) to Updo for some time. I've now
converted it as [up-do/hledger](https://github.com/up-do/hledger). I've added
configuration for GHC versions `9.0.2`, `9.2.8`, `9.4.8` and `9.8.2`.

I developed Updo for a large project that had one current GHC version and one
(or two) prospective GHC upgrade versions. In that situation we worked with a
pair of current projects and a pair of upgrade projects (if generating both
Cabal and Stack projects).

```
$ tree -P '*.project|*.yaml' --prune -L 1
.
├── cabal.project
├── cabal.upgrade.project
├── stack.upgrade.yaml
└── stack.yaml
```

The above files are actually generated in Updo by way of intermediates:

```makefile
# To make stack.yaml or cabal.project and no other, mark the file we copy from
# as intermediate. This is all we want when not doing a GHC upgrade.
#
# Comment out these .INTERMEDIATE targets to allow these files to be kept.
.INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
```

I noticed that hledger used a GHC version naming scheme with its Stack projects.

```
$ tree -P 'stack*.yaml' --prune -L 1
.
├── stack8.10.yaml
├── stack9.0.yaml
├── stack9.2.yaml
├── stack9.4.yaml
├── stack9.6.yaml
└── stack.yaml
```

What I've done then when converting hledger to use Updo is keep the intermediate
files:

```
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
```

Each of these `.project` files should have pinned dependencies that closely
match the versions used in the matching `.yaml` file.

These can be compared with:

```
$ stack --stack-yaml=ghc-9.8.2.dhall2stack.yaml ls dependencies cabal > ghc-9.8.2.dhall2stack.yaml.freeze
$ cabal freeze --project-file=ghc-9.8.2.dhall2cabal.project
```

It is a little annoying that the formatting differs between the two outputs.


```diff
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
```

I've added Stack lock files too so that these can be compared (and saw that they
were ignored in `.gitignore`).

```diff
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

```

I went with `vty-windows-0.2.0.2` for all configurations. I don't know why
`stack9.6.yaml` is the only one to specify `vty-windows-0.2.0.1`.

On versions, in `project-versions.mk` I've left `ghc-9.0.2` as the current
version and `ghc-9.8.2` as the upgrade version, just because that was the order
I tackled them in. As for using Updo, here are some commands I used during the
conversion:

```
$ make -f project-files.mk
$ make -f project-files.mk upgrade-projects
$ GHC_UPGRADE=9.6.5 STACKAGE_UPGRADE=lts-22.22 make -f project-files.mk upgrade-projects
$ make -f project-files.mk clean
```

Probably the biggest hurdle to using Updo right now is being able to use it
without needing to have
[nix-prefetch-git](https://github.com/cabalism/updo/issues/5) available.

The configuration can be found in `project-stackage` and `project-dhall`:

```
$ tree project-stackage
project-stackage
├── lts-19.33.config
├── lts-20.26.config
├── lts-21.25.config
├── lts-22.22.config
└── nightly-2024-05-18.config

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
```
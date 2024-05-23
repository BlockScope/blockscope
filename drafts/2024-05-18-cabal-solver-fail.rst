---
title: Cabal solver fail
subtitle: Cabal solves for versions but that doesn't guarantee a build
tags: haskell, updo
---

When I first started with Haskell I had trouble with Cabal's solver. I could
never get it to solve dependencies. That's all good now and I have been using it
a lot without any issues but one thing to remember is that Cabal's solver only
solves for version constraints. It doesn't guarantee that the build will
succeed.

I've just stumbled upon an instance of this with ``hledger`` and thought it
might be a good example of what can go wrong.

The ``cabal.project`` doesn't build with ``ghc-9.2.8`` using dependencies chosen
by Cabal's solver:

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

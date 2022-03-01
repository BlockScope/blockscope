---
title: Z3 Thoralf DIY
subtitle: The DIY test suite as an annotated worked example.
tags: haskell, tcplugins, z3
---

The Program
-----------

.. code:: haskell

    -- UoM.hs
    {-# LANGUAGE TypeFamilies, TypeOperators, GADTs, DataKinds, PackageImports #-}

    {-# OPTIONS_GHC -fplugin Plugins.Thoralf.UoM #-}

    module UoM where

    import Data.Kind (Type)

    import "uom-quantity" Data.Theory.UoM (type (/:), Unit, One)
    import "uom-quantity" Data.UnitsOfMeasure.Syntax (Exp)
    import "thoralf-plugin" ThoralfPlugin.Singletons.Symbol (SSymbol)

    import "thoralf-plugin-uom" Data.UnitsOfMeasure.Constraint (IsProd, IsDiv, IsEnc)
    import "thoralf-plugin-uom" ThoralfPlugin.Singletons.Nat (SNat(..))

    data Quantity :: Unit -> Type where
        MkQuantity :: Double -> Quantity m

    instance Show (Quantity a) where
        show (MkQuantity x) = show x

    scalar :: Double -> Quantity One
    scalar = MkQuantity

    mkQuantity :: IsEnc s n b => Double -> SSymbol s -> SNat n -> Quantity b
    mkQuantity d _ _ = MkQuantity d

    add :: Quantity a -> Quantity a -> Quantity a
    add (MkQuantity x) (MkQuantity y) = MkQuantity (x + y)

    negate :: Quantity a -> Quantity a
    negate (MkQuantity x) = MkQuantity (-x)

    mult :: IsProd a b c => Quantity a -> Quantity b -> Quantity c
    mult (MkQuantity x) (MkQuantity y) = MkQuantity (x * y)

    div :: IsDiv a b c => Quantity a -> Quantity b -> Quantity c
    div (MkQuantity x) (MkQuantity y) = MkQuantity (x / y)

    extract :: Quantity a -> Double
    extract (MkQuantity d) = d

    -- velocity: m/s
    -- time: s
    -- distance = velocity * time
    type M = Exp '[ '("m", 1)] -- metre
    type S = Exp '[ '("s", 1)] -- second
    type MpS = M /: S -- metres per second

    metres :: Quantity MpS -> Quantity S -> Quantity M
    metres = mult

    distance :: IsDiv M S mps => Quantity mps -> Quantity S -> Quantity M
    distance = mult

    -- Main.hs
    {-# LANGUAGE TemplateHaskell #-}

    module Main where

    import Test.Tasty.TH (defaultMainGenerator)
    import Test.Tasty.QuickCheck (testProperty)
    import Text.Printf (printf)
    import UoM (Quantity(..), extract, metres, distance)

    main :: IO ()
    main = do
        putStrLn "UoM examples"
        putStrLn $ printf "3 m/s for 3 s = %s m" (show $ metres (MkQuantity 3) (MkQuantity 3))
        putStrLn $ printf "3 m/s for 2 s = %s m" (show $ distance (MkQuantity 3) (MkQuantity 2))
        putStrLn ""
        $(defaultMainGenerator)

    prop_distance :: Double -> Double -> Bool
    prop_distance x y = x * y == extract (distance (MkQuantity x) (MkQuantity y))

Translated to SMT2
------------------

.. code:: smt2

    (declare-const n1 Int)
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))

    ; given constraints
    ; [G] $d~_a1BE {0}:: mps ~ mps (CDictCan)
    ; [G] $d~~_a1BF {0}:: mps ~ mps (CDictCan)
    ; (One [],a1Bq)
    ; (Base ["m"],a1Bo)
    ; (Base ["s"],a1Bu)
    ; (*: [a1Bo,a1Bq],a1Bs)
    ; (*: [a1Bu,a1Bq],a1Bw)
    ; (/: [a1Bs,a1Bw],a1By)
    ; (a1By,a1tT)

    ; declarations
    (declare-const a1tT (Array String Int))
    (declare-const a1Bo (Array String Int))
    (declare-const a1Bq (Array String Int))
    (declare-const a1Bs (Array String Int))
    (declare-const a1Bu (Array String Int))
    (declare-const a1Bw (Array String Int))
    (declare-const a1By (Array String Int))

    ; givens
    (assert (= ((as const (Array String Int)) 0) a1Bq))
    (assert (= (store base "m" n1) a1Bo))
    (assert (= (store base "s" n1) a1Bu))
    (assert (= ((_ map (+ (Int Int) Int)) a1Bo a1Bq) a1Bs))
    (assert (= ((_ map (+ (Int Int) Int)) a1Bu a1Bq) a1Bw))
    (assert (= ((_ map (- (Int Int) Int)) a1Bs a1Bw) a1By))
    (assert (= a1By a1tT))

    ; given constraints
    ; [G] $d~_a1Ct {0}:: c ~ c (CDictCan)
    ; [G] $d~~_a1Cu {0}:: c ~ c (CDictCan)
    ; (/: [a1Cd,a1Ce],a1Cn)
    ; (a1Cn,a1Cf)

    ; declarations
    (declare-const a1Cd (Array String Int))
    (declare-const a1Ce (Array String Int))
    (declare-const a1Cf (Array String Int))
    (declare-const a1Cn (Array String Int))

    ; givens
    (assert (= ((_ map (- (Int Int) Int)) a1Cd a1Ce) a1Cn))
    (assert (= a1Cn a1Cf))

    ; given constraints
    ; [G] $d~_a1D6 {0}:: c ~ c (CDictCan)
    ; [G] $d~~_a1D7 {0}:: c ~ c (CDictCan)
    ; (*: [a1CM,a1CN],a1D0)
    ; (a1D0,a1CO)

    ; declarations
    (declare-const a1CM (Array String Int))
    (declare-const a1CN (Array String Int))
    (declare-const a1CO (Array String Int))
    (declare-const a1D0 (Array String Int))

    ; givens
    (assert (= ((_ map (+ (Int Int) Int)) a1CM a1CN) a1D0))
    (assert (= a1D0 a1CO))

    ; given constraints
    ; [G] $d~_a1Eh {0}:: b ~ b (CDictCan)
    ; [G] $d~~_a1Ei {0}:: b ~ b (CDictCan)
    ; (Enc [a1DX,a1DY],a1Eb)
    ; (a1Eb,a1DZ)

    ; declarations
    (declare-const a1DX String)
    (declare-const a1DY Int)
    (declare-const a1DZ (Array String Int))
    (declare-const a1Eb (Array String Int))
    (assert (<= 0 a1DY))

    ; givens
    (assert (= (store enc a1DX a1DY) a1Eb))
    (assert (= a1Eb a1DZ))

    ; wanted constraints
    ; [
    ;   ( *:
    ;       [ Base ["m"]
    ;       , One []
    ;       ]
    ;   , *:
    ;       [ /:
    ;           [ *:
    ;               [ Base ["m"]
    ;               , One []
    ;               ]
    ;           , *:
    ;               [ Base ["s"]
    ;               , One []
    ;               ]
    ;           ]
    ;       , *:
    ;            [ Base ["s"]
    ;            , One []
    ;            ]
    ;       ]
    ;   )
    ; ]

    ; wanteds
    (assert
        (or
            false
            (not
                (=
                    ((_ map (+ (Int Int) Int))
                        (store base "m" n1)
                        ((as const (Array String Int)) 0))
                    ((_ map (+ (Int Int) Int))
                        ((_ map (- (Int Int) Int))
                            ((_ map (+ (Int Int) Int))
                                (store base "m" n1)
                                ((as const (Array String Int)) 0))
                            ((_ map (+ (Int Int) Int))
                                (store base "s" n1)
                                ((as const (Array String Int)) 0)))
                        ((_ map (+ (Int Int) Int))
                            (store base "s" n1)
                            ((as const (Array String Int)) 0)))))))

    ; given constraints
    ; [G] $d~_a1GM {0}:: c ~ c (CDictCan)
    ; [G] $d~~_a1GN {0}:: c ~ c (CDictCan)
    ; (/: [a1Et,a1Eu],a1GG)
    ; (a1GG,a1Ev)

    ; declarations
    (declare-const a1Et (Array String Int))
    (declare-const a1Eu (Array String Int))
    (declare-const a1Ev (Array String Int))
    (declare-const a1GG (Array String Int))

    ; givens
    (assert (= ((_ map (- (Int Int) Int)) a1Et a1Eu) a1GG))
    (assert (= a1GG a1Ev))

    ; given constraints
    ; [G] $d~_a1GV {0}:: c ~ c (CDictCan)
    ; [G] $d~~_a1GW {0}:: c ~ c (CDictCan)
    ; (*: [a1EJ,a1EK],a1GP)
    ; (a1GP,a1EL)

    ; declarations
    (declare-const a1EJ (Array String Int))
    (declare-const a1EK (Array String Int))
    (declare-const a1EL (Array String Int))
    (declare-const a1GP (Array String Int))

    ; givens
    (assert (= ((_ map (+ (Int Int) Int)) a1EJ a1EK) a1GP))
    (assert (= a1GP a1EL))

    ; given constraints
    ; [G] $d~_a1He {0}:: mps ~ mps (CDictCan)
    ; [G] $d~~_a1Hf {0}:: mps ~ mps (CDictCan)
    ; (One [],a1H0)
    ; (Base ["m"],a1GY)
    ; (Base ["s"],a1H4)
    ; (*: [a1GY,a1H0],a1H2)
    ; (*: [a1H4,a1H0],a1H6)
    ; (/: [a1H2,a1H6],a1H8)
    ; (a1H8,a1Fa)

    ; wanted constraints
    ; [
    ;   ( *:
    ;       [ a1Fa
    ;       , *:
    ;           [ Base ["s"]
    ;           , One []
    ;           ]
    ;       ]
    ;   , *:
    ;       [ Base ["m"]
    ;       , One []
    ;       ]
    ;   )
    ; ]

    ; declarations
    (declare-const a1Fa (Array String Int))
    (declare-const a1GY (Array String Int))
    (declare-const a1H0 (Array String Int))
    (declare-const a1H2 (Array String Int))
    (declare-const a1H4 (Array String Int))
    (declare-const a1H6 (Array String Int))
    (declare-const a1H8 (Array String Int))

    ; givens
    (assert (= ((as const (Array String Int)) 0) a1H0))
    (assert (= (store base "m" n1) a1GY))
    (assert (= (store base "s" n1) a1H4))
    (assert (= ((_ map (+ (Int Int) Int)) a1GY a1H0) a1H2))
    (assert (= ((_ map (+ (Int Int) Int)) a1H4 a1H0) a1H6))
    (assert (= ((_ map (- (Int Int) Int)) a1H2 a1H6) a1H8))
    (assert (= a1H8 a1Fa))

    ; wanteds
    (assert
        (or
            false
            (not
                (=
                    ((_ map (+ (Int Int) Int))
                        a1Fa
                        ((_ map (+ (Int Int) Int))
                            (store base "s" n1)
                            ((as const (Array String Int)) 0)))
                    ((_ map (+ (Int Int) Int))
                        (store base "m" n1)
                        ((as const (Array String Int)) 0))))))

    ; given constraints
    ; [G] $d~_a1Hv {0}:: b ~ b (CDictCan)
    ; [G] $d~~_a1Hw {0}:: b ~ b (CDictCan)
    ; (Enc [a1Fw,a1Fx],a1Hp)
    ; (a1Hp,a1Fy)
    
    ; declarations
    (declare-const a1Fw String)
    (declare-const a1Fx Int)
    (declare-const a1Fy (Array String Int))
    (declare-const a1Hp (Array String Int))
    (assert (<= 0 a1Fx))

    ; givens
    (assert (= (store enc a1Fw a1Fx) a1Hp))
    (assert (= a1Hp a1Fy))

    (check-sat)
    unsat

Live
----

.. code:: smt2

    (set-option :print-success true)
    (set-option :produce-models true)
    (push 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1Bz {3}:: One ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Bx {2}:: Base "m" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1BD {2}:: Base "s" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1BB {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1BF {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1BH {1}:: (fsk0 /: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1BL {1}:: fsk0 ~ mps (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (One [],a1By)
    ; (Base ["m"],a1Bw)
    ; (Base ["s"],a1BC)
    ; (*: [a1Bw,a1By],a1BA)
    ; (*: [a1BC,a1By],a1BE)
    ; (/: [a1BA,a1BE],a1BG)
    ; (a1BG,a1u1)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1u1 (Array String Int))
    (declare-const a1Bw (Array String Int))
    (declare-const a1By (Array String Int))
    (declare-const a1BA (Array String Int))
    (declare-const a1BC (Array String Int))
    (declare-const a1BE (Array String Int))
    (declare-const a1BG (Array String Int))
    (assert
    (= ((as const (Array String Int)) 0) a1By))
    (assert
    (= (store base "m" one) a1Bw))
    (assert
    (= (store base "s" one) a1BC))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1Bw
    a1By) a1BA))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1BC
    a1By) a1BE))
    (assert
    (= (
    (_
        map
        (-
            (Int Int)
            Int))
    a1BA
    a1BE) a1BG))
    (assert
    (= a1BG a1u1))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1Cw {1}:: (a /: b) ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1CA {1}:: fsk0 ~ c (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (/: [a1Cl,a1Cm],a1Cv)
    ; (a1Cv,a1Cn)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1Cl (Array String Int))
    (declare-const a1Cm (Array String Int))
    (declare-const a1Cn (Array String Int))
    (declare-const a1Cv (Array String Int))
    (assert
    (= (
    (_
        map
        (-
            (Int Int)
            Int))
    a1Cl
    a1Cm) a1Cv))
    (assert
    (= a1Cv a1Cn))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1D9 {1}:: (a *: b) ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Dd {1}:: fsk0 ~ c (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (*: [a1CU,a1CV],a1D8)
    ; (a1D8,a1CW)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1CU (Array String Int))
    (declare-const a1CV (Array String Int))
    (declare-const a1CW (Array String Int))
    (declare-const a1D8 (Array String Int))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1CU
    a1CV) a1D8))
    (assert
    (= a1D8 a1CW))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1Ek {1}:: Enc s n ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Eo {1}:: fsk0 ~ b (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (Enc [a1E5,a1E6],a1Ej)
    ; (a1Ej,a1E7)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1E5 String)
    (declare-const a1E6 Int)
    (declare-const a1E7 (Array String Int))
    (declare-const a1Ej (Array String Int))
    (assert (<= 0 a1E6))
    (assert
    (= (store enc a1E5 a1E6) a1Ej))
    (assert
    (= a1Ej a1E7))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; []

    ; WANTEDS (GHC style)
    ; [WD] hole{a1Gp} {2}:: (Base "m" *: One)
    ;                       ~
    ;                       (((Base "m" *: One) /: (Base "s" *: One))
    ;                        *: (Base "s" *: One)) (CNonCanonical)

    ; GIVENS (Thoralf style)
    ; []

    ; WANTEDS (Thoralf style)
    ; (*: [Base ["m"],One []],*: [/: [*: [Base ["m"],One []],*: [Base ["s"],One []]],*: [Base ["s"],One []]])

    (push 1)
    (check-sat)
    sat
    (assert
    (or
        false
        (not
            (= (
    (_
        map
        (+
            (Int Int)
            Int))
    (store base "m" one)
    (
        (as
        const
        (Array String Int))
        0)) (
    (_
        map
        (+
            (Int Int)
            Int))
    (
        (_
        map
        (-
            (Int Int)
            Int))
        (
        (_
            map
            (+
                (Int Int)
                Int))
        (store base "m" one)
        (
            (as
            const
            (Array String Int))
            0))
        (
        (_
            map
            (+
                (Int Int)
                Int))
        (store base "s" one)
        (
            (as
            const
            (Array String Int))
            0)))
    (
        (_
        map
        (+
            (Int Int)
            Int))
        (store base "s" one)
        (
        (as
            const
            (Array String Int))
        0)))))))
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1GP {1}:: (a /: b) ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1GT {1}:: fsk0 ~ c (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (/: [a1EB,a1EC],a1GO)
    ; (a1GO,a1ED)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1EB (Array String Int))
    (declare-const a1EC (Array String Int))
    (declare-const a1ED (Array String Int))
    (declare-const a1GO (Array String Int))
    (assert
    (= (
    (_
        map
        (-
            (Int Int)
            Int))
    a1EB
    a1EC) a1GO))
    (assert
    (= a1GO a1ED))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1GY {1}:: (a *: b) ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1H2 {1}:: fsk0 ~ c (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (*: [a1ER,a1ES],a1GX)
    ; (a1GX,a1ET)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1ER (Array String Int))
    (declare-const a1ES (Array String Int))
    (declare-const a1ET (Array String Int))
    (declare-const a1GX (Array String Int))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1ER
    a1ES) a1GX))
    (assert
    (= a1GX a1ET))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1H9 {3}:: One ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1H7 {2}:: Base "m" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Hd {2}:: Base "s" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Hb {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hf {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hh {1}:: (fsk0 /: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hl {1}:: fsk0 ~ mps (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (One [],a1H8)
    ; (Base ["m"],a1H6)
    ; (Base ["s"],a1Hc)
    ; (*: [a1H6,a1H8],a1Ha)
    ; (*: [a1Hc,a1H8],a1He)
    ; (/: [a1Ha,a1He],a1Hg)
    ; (a1Hg,a1Fi)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1Fi (Array String Int))
    (declare-const a1H6 (Array String Int))
    (declare-const a1H8 (Array String Int))
    (declare-const a1Ha (Array String Int))
    (declare-const a1Hc (Array String Int))
    (declare-const a1He (Array String Int))
    (declare-const a1Hg (Array String Int))
    (assert
    (= ((as const (Array String Int)) 0) a1H8))
    (assert
    (= (store base "m" one) a1H6))
    (assert
    (= (store base "s" one) a1Hc))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1H6
    a1H8) a1Ha))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1Hc
    a1H8) a1He))
    (assert
    (= (
    (_
        map
        (-
            (Int Int)
            Int))
    a1Ha
    a1He) a1Hg))
    (assert
    (= a1Hg a1Fi))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1H9 {3}:: One ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1H7 {2}:: Base "m" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Hd {2}:: Base "s" ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1Hb {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hf {2}:: (fsk0 *: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hh {1}:: (fsk0 /: fsk1) ~ fsk2 (CFunEqCan)
    ; [G] cobox_a1Hl {1}:: fsk0 ~ mps (CTyEqCan)

    ; WANTEDS (GHC style)
    ; [WD] hole{a1Hw} {3}:: (mps *: (Base "s" *: One))
    ;                       ~
    ;                       (Base "m" *: One) (CNonCanonical)

    ; GIVENS (Thoralf style)
    ; (One [],a1H8)
    ; (Base ["m"],a1H6)
    ; (Base ["s"],a1Hc)
    ; (*: [a1H6,a1H8],a1Ha)
    ; (*: [a1Hc,a1H8],a1He)
    ; (/: [a1Ha,a1He],a1Hg)
    ; (a1Hg,a1Fi)

    ; WANTEDS (Thoralf style)
    ; (*: [a1Fi,*: [Base ["s"],One []]],*: [Base ["m"],One []])

    (push 1)
    (declare-const a1Fi (Array String Int))
    (declare-const a1H6 (Array String Int))
    (declare-const a1H8 (Array String Int))
    (declare-const a1Ha (Array String Int))
    (declare-const a1Hc (Array String Int))
    (declare-const a1He (Array String Int))
    (declare-const a1Hg (Array String Int))
    (assert
    (= ((as const (Array String Int)) 0) a1H8))
    (assert
    (= (store base "m" one) a1H6))
    (assert
    (= (store base "s" one) a1Hc))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1H6
    a1H8) a1Ha))
    (assert
    (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1Hc
    a1H8) a1He))
    (assert
    (= (
    (_
        map
        (-
            (Int Int)
            Int))
    a1Ha
    a1He) a1Hg))
    (assert
    (= a1Hg a1Fi))
    (check-sat)
    sat
    (assert
    (or
        false
        (not
            (= (
    (_
        map
        (+
            (Int Int)
            Int))
    a1Fi
    (
        (_
        map
        (+
            (Int Int)
            Int))
        (store base "s" one)
        (
        (as
            const
            (Array String Int))
        0))) (
    (_
        map
        (+
            (Int Int)
            Int))
    (store base "m" one)
    (
        (as
        const
        (Array String Int))
        0))))))
    (check-sat)
    unsat
    (pop 1)
    (exit)
    (set-option :print-success true)
    (set-option :produce-models true)
    (declare-datatypes () ((Type (apply (fst Type) (snd Type)) (lit (getstr String)))))
    (declare-const one Int)
    (assert (= one 1))
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))
    (declare-const exp (Array String Int))
    (assert (= enc base))
    (push 1)

    ; GIVENS (GHC style)
    ; [G] cobox_a1Hy {1}:: Enc s n ~ fsk0 (CFunEqCan)
    ; [G] cobox_a1HC {1}:: fsk0 ~ b (CTyEqCan)

    ; WANTEDS (GHC style)
    ; []

    ; GIVENS (Thoralf style)
    ; (Enc [a1FE,a1FF],a1Hx)
    ; (a1Hx,a1FG)

    ; WANTEDS (Thoralf style)
    ; []

    (push 1)
    (declare-const a1FE String)
    (declare-const a1FF Int)
    (declare-const a1FG (Array String Int))
    (declare-const a1Hx (Array String Int))
    (assert (<= 0 a1FF))
    (assert
    (= (store enc a1FE a1FF) a1Hx))
    (assert
    (= a1Hx a1FG))
    (check-sat)
    sat
    (assert false)
    (check-sat)
    unsat
    (pop 1)
    (exit)
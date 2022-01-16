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

    (declare-const a1tT (Array String Int))
    (declare-const a1Bo (Array String Int))
    (declare-const a1Bq (Array String Int))
    (declare-const a1Bs (Array String Int))
    (declare-const a1Bu (Array String Int))
    (declare-const a1Bw (Array String Int))
    (declare-const a1By (Array String Int))

    (assert (= ((as const (Array String Int)) 0) a1Bq))
    (assert (= (store base "m" n1) a1Bo))
    (assert (= (store base "s" n1) a1Bu))
    (assert (= ((_ map (+ (Int Int) Int)) a1Bo a1Bq) a1Bs))
    (assert (= ((_ map (+ (Int Int) Int)) a1Bu a1Bq) a1Bw))
    (assert (= ((_ map (- (Int Int) Int)) a1Bs a1Bw) a1By))
    (assert (= a1By a1tT))

    (declare-const a1Cd (Array String Int))
    (declare-const a1Ce (Array String Int))
    (declare-const a1Cf (Array String Int))
    (declare-const a1Cn (Array String Int))

    (assert (= ((_ map (- (Int Int) Int)) a1Cd a1Ce) a1Cn))
    (assert (= a1Cn a1Cf))

    (declare-const a1CM (Array String Int))
    (declare-const a1CN (Array String Int))
    (declare-const a1CO (Array String Int))
    (declare-const a1D0 (Array String Int))

    (assert (= ((_ map (+ (Int Int) Int)) a1CM a1CN) a1D0))
    (assert (= a1D0 a1CO))

    (declare-const a1DX String)
    (declare-const a1DY Int)
    (declare-const a1DZ (Array String Int))
    (declare-const a1Eb (Array String Int))
    (assert (<= 0 a1DY))

    (assert (= (store enc a1DX a1DY) a1Eb))
    (assert (= a1Eb a1DZ))

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

    (declare-const a1Et (Array String Int))
    (declare-const a1Eu (Array String Int))
    (declare-const a1Ev (Array String Int))
    (declare-const a1GG (Array String Int))

    (assert (= ((_ map (- (Int Int) Int)) a1Et a1Eu) a1GG))
    (assert (= a1GG a1Ev))

    (declare-const a1EJ (Array String Int))
    (declare-const a1EK (Array String Int))
    (declare-const a1EL (Array String Int))
    (declare-const a1GP (Array String Int))

    (assert (= ((_ map (+ (Int Int) Int)) a1EJ a1EK) a1GP))
    (assert (= a1GP a1EL))

    (declare-const a1Fa (Array String Int))
    (declare-const a1GY (Array String Int))
    (declare-const a1H0 (Array String Int))
    (declare-const a1H2 (Array String Int))
    (declare-const a1H4 (Array String Int))
    (declare-const a1H6 (Array String Int))
    (declare-const a1H8 (Array String Int))

    (assert (= ((as const (Array String Int)) 0) a1H0))
    (assert (= (store base "m" n1) a1GY))
    (assert (= (store base "s" n1) a1H4))
    (assert (= ((_ map (+ (Int Int) Int)) a1GY a1H0) a1H2))
    (assert (= ((_ map (+ (Int Int) Int)) a1H4 a1H0) a1H6))
    (assert (= ((_ map (- (Int Int) Int)) a1H2 a1H6) a1H8))
    (assert (= a1H8 a1Fa))

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

    (declare-const a1Fw String)
    (declare-const a1Fx Int)
    (declare-const a1Fy (Array String Int))
    (declare-const a1Hp (Array String Int))
    (assert (<= 0 a1Fx))

    (assert (= (store enc a1Fw a1Fx) a1Hp))
    (assert (= a1Hp a1Fy))

    (check-sat)
    unsat
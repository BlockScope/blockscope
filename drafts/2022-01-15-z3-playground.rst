---
title: Z3 playground
subtitle: SMT scripts.
tags: haskell, tcplugins
---

Simple Algebra
--------------

.. code:: smt2

    (declare-const a Int)
    (assert (not (= (* 1 a) (* a 1))))
    (check-sat)
    unsat

.. code:: smt2

    (declare-const n1 Int)
    (declare-const base (Array String Int))
    (declare-const enc (Array String Int))

    (declare-const a1tT (Array String Int))
    (declare-const m (Array String Int))
    (declare-const one (Array String Int))
    (declare-const m_mul_one (Array String Int))
    (declare-const s (Array String Int))
    (declare-const s_mul_one (Array String Int))
    (declare-const m_div_s (Array String Int))
    (declare-const cup (Array String Int))
    (declare-const cake (Array String Int))
    (declare-const cupcake (Array String Int))
    (declare-const cup cake (Array String Int))

    (assert (= ((as const (Array String Int)) 0) one))
    (assert (= (store base "m" n1) m))
    (assert (= (store base "s" n1) s))
    (assert (= ((_ map (+ (Int Int) Int)) m one) m_mul_one))
    (assert (= ((_ map (+ (Int Int) Int)) s one) s_mul_one))
    (assert (= ((_ map (- (Int Int) Int)) m_mul_one s_mul_one) m_div_s))
    (assert (= m_div_s a1tT))

SMT-solver as a Calculator
--------------------------

.. code:: smt2

    (declare-const x Real)
    (declare-const y Real)
    (declare-const z Real)
    (assert (=(-(+(* 3 x) (* 2 y)) z) 1))
    (assert (=(+(-(* 2 x) (* 2 y)) (* 4 z)) -2))
    (assert (=(-(+ (- 0 x) (* 0.5 y)) z) 0))
    (check-sat)
    (get-model)

.. code:: pre

    sat
    (
    (define-fun z () Real
        (- 2.0))
    (define-fun y () Real
        (- 2.0))
    (define-fun x () Real
        1.0)
    )

.. code:: smt2

    (declare-fun x () (_ BitVec 16))
    (declare-fun y () (_ BitVec 16))
    (declare-fun z () (_ BitVec 16))
    (assert (= x (_ bv2 16))) (assert (= y (_ bv3 16)))
    (assert (= z (bvadd x y)))
    (check-sat)
    (get-model)

.. code:: pre

    sat
    (
    (define-fun z () (_ BitVec 16)
        #x0005)
    (define-fun y () (_ BitVec 16)
        #x0003)
    (define-fun x () (_ BitVec 16)
        #x0002)
    )
---
title: Z3 playground
subtitle: SMT scripts.
tags: haskell, tcplugins
---

Simple Algebra
--------------

.. code:: SMT2

    (declare-const a Int)
    (assert (not (= (* 1 a) (* a 1))))
    (check-sat)
    unsat

.. code:: SMT2

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
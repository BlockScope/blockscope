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

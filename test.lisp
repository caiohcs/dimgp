;; TODO
(defpackage #:dimgp/test
  (:use #:common-lisp #:fiveam #:dimgp))

(in-package #:dimgp/test)

(def-suite add-suite)

(in-suite add-suite)

(dimgp::read-image "imgs/lena.ppm")

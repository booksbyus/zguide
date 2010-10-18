;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Test zmsg class in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zmsg_test
  (:nicknames #:zmsg_test)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.zmsg_test)

(defun main ()
  (zmsg:test t)
  (cleanup))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Test zmsg class in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zmsg_test.asd
  (:use #:cl #:asdf))

(in-package :zguide.zmsg_test.asd)

(defsystem zmsg_test
  :version "0.0.0"
  :description "Test zmsg class in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zmsg)
  :serial t
  :components ((:file "zmsg_test")))

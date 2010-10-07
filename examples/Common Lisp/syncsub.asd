;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Synchronized subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.syncsub.asd
  (:use #:cl #:asdf))

(in-package :zguide.syncsub.asd)

(defsystem syncsub
  :version "0.0.0"
  :description "Synchronized subscriber in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "syncsub" :depends-on ("zhelpers"))))

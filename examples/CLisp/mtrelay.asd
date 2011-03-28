;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multithreaded relay in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mtrelay.asd
  (:use #:cl #:asdf))

(in-package :zguide.mtrelay.asd)

(defsystem mtrelay
  :version "0.0.0"
  :description "Multithreaded relay in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq :bordeaux-threads)
  :serial t
  :components ((:file "zhelpers")
               (:file "mtrelay" :depends-on ("zhelpers"))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Synchronized publisher in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.syncpub.asd
  (:use #:cl #:asdf))

(in-package :zguide.syncpub.asd)

(defsystem syncpub
  :version "0.0.0"
  :description "Synchronized publisher in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "syncpub" :depends-on ("zhelpers"))))

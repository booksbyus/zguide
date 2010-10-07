;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Pubsub envelope subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.psenvsub.asd
  (:use #:cl #:asdf))

(in-package :zguide.psenvsub.asd)

(defsystem psenvsub
  :version "0.0.0"
  :description "Pubsub envelope subscriber in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "psenvsub" :depends-on ("zhelpers"))))

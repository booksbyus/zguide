;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Simple request-reply broker in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rrbroker.asd
  (:use #:cl #:asdf))

(in-package :zguide.rrbroker.asd)

(defsystem rrbroker
  :version "0.0.0"
  :description "Simple request-reply broker in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "rrbroker" :depends-on ("zhelpers"))))

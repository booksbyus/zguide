;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Publisher for durable subscriber in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.durapub.asd
  (:use #:cl #:asdf))

(in-package :zguide.durapub.asd)

(defsystem durapub
  :version "0.0.0"
  :description "Publisher for durable subscriber in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "durapub" :depends-on ("zhelpers"))))

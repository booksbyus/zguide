;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Hello World server in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.hwserver.asd
  (:use #:cl #:asdf))

(in-package :zguide.hwserver.asd)

(defsystem hwserver
  :version "0.0.0"
  :description "Hello World server in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "hwserver" :depends-on ("zhelpers"))))

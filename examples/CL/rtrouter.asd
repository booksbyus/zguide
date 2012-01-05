;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Cross-connected ROUTER sockets addressing each other in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtrouter.asd
  (:use #:cl #:asdf))

(in-package :zguide.rtrouter.asd)

(defsystem rtrouter
  :version "0.0.0"
  :description "Cross-connected ROUTER sockets addressing each other in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "rtrouter" :depends-on ("zhelpers"))))

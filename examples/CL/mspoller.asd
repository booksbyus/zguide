;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Reading from multiple sockets in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mspoller.asd
  (:use #:cl #:asdf))

(in-package :zguide.mspoller.asd)

(defsystem mspoller
  :version "0.0.0"
  :description "Reading from multiple sockets in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "mspoller" :depends-on ("zhelpers"))))

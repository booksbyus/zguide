;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Reading from multiple sockets in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.msreader.asd
  (:use #:cl #:asdf))

(in-package :zguide.msreader.asd)

(defsystem msreader
  :version "0.0.0"
  :description "Reading from multiple sockets in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "msreader" :depends-on ("zhelpers"))))

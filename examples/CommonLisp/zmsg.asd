;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multipart message class for example applications in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zmsg.asd
  (:use #:cl #:asdf))

(in-package :zguide.zmsg.asd)

(defsystem zmsg
  :version "0.0.0"
  :description "Multipart message class for example applications in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "zmsg" :depends-on ("zhelpers"))))

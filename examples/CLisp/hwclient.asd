;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Hello World client in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.hwclient.asd
  (:use #:cl #:asdf))

(in-package :zguide.hwclient.asd)

(defsystem hwclient
  :version "0.0.0"
  :description "Hello World client in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "hwclient" :depends-on ("zhelpers"))))

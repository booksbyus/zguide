;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Weather update server in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuserver.asd
  (:use #:cl #:asdf))

(in-package :zguide.wuserver.asd)

(defsystem wuserver
  :version "0.0.0"
  :description "Weather update server in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "wuserver" :depends-on ("zhelpers"))))

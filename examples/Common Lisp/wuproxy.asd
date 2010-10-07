;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Weather proxy device in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuproxy.asd
  (:use #:cl #:asdf))

(in-package :zguide.wuproxy.asd)

(defsystem wuproxy
  :version "0.0.0"
  :description "Weather proxy device in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "wuproxy" :depends-on ("zhelpers"))))

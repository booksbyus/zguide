;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Report 0MQ version
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zversion.asd
  (:use #:cl #:asdf))

(in-package :zguide.zversion.asd)

(defsystem zversion
  :version "0.0.0"
  :description "Report 0MQ version."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "zversion" :depends-on ("zhelpers"))))

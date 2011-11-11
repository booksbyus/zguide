;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Weather update client in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.wuclient.asd
  (:use #:cl #:asdf))

(in-package :zguide.wuclient.asd)

(defsystem wuclient
  :version "0.0.0"
  :description "Weather update client in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq :split-sequence)
  :serial t
  :components ((:file "zhelpers")
               (:file "wuclient" :depends-on ("zhelpers"))))

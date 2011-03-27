;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Pubsub envelope publisher in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.psenvpub.asd
  (:use #:cl #:asdf))

(in-package :zguide.psenvpub.asd)

(defsystem psenvpub
  :version "0.0.0"
  :description "Pubsub envelope publisher in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "psenvpub" :depends-on ("zhelpers"))))

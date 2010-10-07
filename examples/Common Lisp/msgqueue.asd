;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Simple message queuing broker in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.msgqueue.asd
  (:use #:cl #:asdf))

(in-package :zguide.msgqueue.asd)

(defsystem msgqueue
  :version "0.0.0"
  :description "Simple message queuing broker in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "msgqueue" :depends-on ("zhelpers"))))

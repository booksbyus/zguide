;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Task ventilator in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskvent.asd
  (:use #:cl #:asdf))

(in-package :zguide.taskvent.asd)

(defsystem taskvent
  :version "0.0.0"
  :description "Task ventilator in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "taskvent" :depends-on ("zhelpers"))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Task sink - design 2 in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.tasksink2.asd
  (:use #:cl #:asdf))

(in-package :zguide.tasksink2.asd)

(defsystem tasksink2
  :version "0.0.0"
  :description "Task sink - design 2 in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "tasksink2" :depends-on ("zhelpers"))))

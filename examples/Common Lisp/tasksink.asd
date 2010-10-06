;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Task sink in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.tasksink.asd
  (:use #:cl #:asdf))

(in-package :zguide.tasksink.asd)

(defsystem tasksink
  :version "0.0.0"
  :description "Task sink in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "tasksink" :depends-on ("zhelpers"))))

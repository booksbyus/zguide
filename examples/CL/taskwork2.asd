;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Task worker - design 2 in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskwork2.asd
  (:use #:cl #:asdf))

(in-package :zguide.taskwork2.asd)

(defsystem taskwork2
  :version "0.0.0"
  :description "Task worker - design 2 in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "taskwork2" :depends-on ("zhelpers"))))

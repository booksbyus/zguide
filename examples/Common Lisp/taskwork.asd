;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Task worker in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.taskwork.asd
  (:use #:cl #:asdf))

(in-package :zguide.taskwork.asd)

(defsystem taskwork
  :version "0.0.0"
  :description "Task worker in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "taskwork" :depends-on ("zhelpers"))))

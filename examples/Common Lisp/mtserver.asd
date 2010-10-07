;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Multithreaded Hello World server in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.mtserver.asd
  (:use #:cl #:asdf))

(in-package :zguide.mtserver.asd)

(defsystem mtserver
  :version "0.0.0"
  :description "Multithreaded Hello World server in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq :bordeaux-threads)
  :serial t
  :components ((:file "zhelpers")
               (:file "mtserver" :depends-on ("zhelpers"))))

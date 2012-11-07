;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Custom routing Router to Mama (ROUTER to REQ) in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtmama.asd
  (:use #:cl #:asdf))

(in-package :zguide.rtmama.asd)

(defsystem rtmama
  :version "0.0.0"
  :description "Custom routing Router to Mama (ROUTER to REQ) in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq :bordeaux-threads)
  :serial t
  :components ((:file "zhelpers")
               (:file "rtmama" :depends-on ("zhelpers"))))

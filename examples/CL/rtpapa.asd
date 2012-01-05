;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Custom routing Router to Papa (ROUTER to REP) in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.rtpapa.asd
  (:use #:cl #:asdf))

(in-package :zguide.rtpapa.asd)

(defsystem rtpapa
  :version "0.0.0"
  :description "Custom routing Router to Papa (ROUTER to REP) in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "rtpapa" :depends-on ("zhelpers"))))

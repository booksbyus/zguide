;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Demonstrate identities as used by the request-reply pattern in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.identity.asd
  (:use #:cl #:asdf))

(in-package :zguide.identity.asd)

(defsystem identity
  :version "0.0.0"
  :description "Demonstrate identities as used by the
request-reply pattern in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq)
  :serial t
  :components ((:file "zhelpers")
               (:file "identity" :depends-on ("zhelpers"))))

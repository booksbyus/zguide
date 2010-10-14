;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Least-recently used (LRU) queue device in Common Lisp
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.lruqueue.asd
  (:use #:cl #:asdf))

(in-package :zguide.lruqueue.asd)

(defsystem lruqueue
  :version "0.0.0"
  :description "Least-recently used (LRU) queue device in Common Lisp."
  :maintainer "Kamil Shakirov <kamils80@gmail.com>"
  :author "Kamil Shakirov <kamils80@gmail.com>"
  :licence "MIT/X11"
  :depends-on (:zeromq :bordeaux-threads)
  :serial t
  :components ((:file "zhelpers")
               (:file "lruqueue" :depends-on ("zhelpers"))))

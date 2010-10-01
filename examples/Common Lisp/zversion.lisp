;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Hello World client in Common Lisp
;;;  Connects REQ socket to tcp://localhost:5555
;;;  Sends "Hello" to server, expects "World" back
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.zversion
  (:nicknames #:zversion)
  (:use #:cl #:zhelpers)
  (:export #:main))

(in-package :zguide.zversion)

(defun main ()
  (version))

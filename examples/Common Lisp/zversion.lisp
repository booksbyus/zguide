;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;; Report 0MQ version
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

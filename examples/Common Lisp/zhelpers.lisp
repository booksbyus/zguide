;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Helpers for example applications
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :split-sequence)
  (require :zeromq))

(defpackage #:zguide.zhelpers
  (:nicknames #:zhelpers)
  (:use #:cl)
  (:export
   #:version
   #:cleanup
   #:cmd-args))

(in-package :zguide.zhelpers)

(defun version ()
  (format t "Current 0MQ version is ~A~%" (zmq:version)))

(defun cleanup ()
  (tg:gc)
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit)
  #+clisp (ext:quit)
  #+lispworks (lispworks:quit)
  #+ecl (ext:quit))

(defun cmd-args ()
  (or
   #+sbcl sb-ext:*posix-argv*
   #+ccl ccl:*command-line-argument-list*
   #+clisp ext:*args*
   #+lispworks system:*line-arguments-list*
   #+ecl (ext:command-args)
   nil))

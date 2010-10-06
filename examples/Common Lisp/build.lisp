;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-
;;;
;;;  Application build script
;;;
;;; Kamil Shakirov <kamils80@gmail.com>
;;;

(defpackage #:zguide.build
  (:nicknames #:build)
  (:use #:cl)
  (:export #:build-example))

(in-package :zguide.build)

(defun build-example (app-name)
  (let ((app-entry (find-symbol "MAIN" (string-upcase app-name))))
    #+sbcl (sb-ext:save-lisp-and-die app-name :executable t :toplevel app-entry)
    #+ccl (ccl:save-application app-name :prepend-kernel t :toplevel-function app-entry)))

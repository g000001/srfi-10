;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-10
  (:export :enable-read-time-application
           :disable-read-time-application))

(defpackage :srfi-10-internal
  (:use :srfi-10 :cl :fiveam))
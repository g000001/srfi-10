;;;; package.lisp

(cl:in-package #:cl-user)

(defpackage "https://github.com/g000001/srfi-10"
  (:export #:enable-read-time-application
           #:disable-read-time-application
           #:define-reader-ctor))

(defpackage "https://github.com/g000001/srfi-10#internals"
  (:use "https://github.com/g000001/srfi-10" #:cl #:5am))

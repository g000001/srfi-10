;;;; srfi-10.lisp

(cl:in-package :srfi-10-internal)

(def-suite srfi-10)

(in-suite srfi-10)

(defvar *original-readtable* nil)

(defvar *restore-read-time-application* nil)

(defun define-reader-ctor (symbol proc)
  (setf (get symbol 'reader-ctor)
        proc))

(defun lookup (tag stream &aux it)
  (cond ((and (consp tag)
              (eq 'cl:lambda (car tag)))
         ;; lambda
         tag)
        ((fboundp tag)
         ;; symbol-function
         tag)
        ((setq it (get tag 'reader-ctor))
         it)
        ('T (error stream "The reader-ctor ~S is undefined." tag))))

(test lookup
  (with-output-to-string (out)
    (is (eq 'cl:list (lookup 'cl:list out))))
  (with-output-to-string (out)
    (is (equal '(lambda (x) x)
               (lookup '(lambda (x) x) out))))
  (with-output-to-string (out)
    (let ((sym (gensym)))
      (define-reader-ctor sym #'cl:values)
      (is (eq #'cl:values (lookup sym out)))))
  (with-output-to-string (out)
    (signals (error)
      (lookup (gensym) out))))

(defun read-time-application (stream sub-char numarg)
  (when numarg
    (warn "A numeric argument was ignored in #~W~A." numarg sub-char))
  (let ((token (read stream t nil t)))
    (unless *read-suppress*
      (unless *read-eval*
        (error stream "can't read #, while *READ-EVAL* is NIL"))
      (apply (lookup (car token) stream)
             (cdr token)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %enable-read-time-application ()
    (unless *original-readtable*
      (setf *original-readtable* *readtable*
            *readtable* (copy-readtable))
      (set-dispatch-macro-character #\# #\,
                                    #'read-time-application))
    (values))

  (defun %disable-read-time-application ()
    (when *original-readtable*
      (setf *readtable* *original-readtable*
            *original-readtable* nil))
    (values)) )

(defmacro enable-read-time-application ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-read-time-application* 'T)
    (%enable-read-time-application)))

(defmacro disable-read-time-application ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-read-time-application* nil)
    (%disable-read-time-application)))

(test read-time-application
  (let ((*original-readtable* nil)
        (*restore-read-time-application* nil)
        (*readtable* (copy-readtable nil)))
    (enable-read-time-application)
    ;;
    (is (= (read-from-string "#,(+ 1 1)")
           2))
    ;;
    (is (= (read-from-string "#,(+ 1 1 1)")
           3))
    ;;
    (is (equal (read-from-string "#,(list 'x 'y 'z)")
               '('x 'y 'z)) )

    ;; (+ 1 '(+ 2 3)) => error
    (signals (error)
      (equal (read-from-string "#,(+ 1 (+ 2 3))")))
    ;;
    (let ((sym (gentemp)))
      (define-reader-ctor sym #'cl:values)
      (is (= 1 (read-from-string (format nil "#,(~S 1)" sym))))
      (unintern sym))

    ;; undefined tag
    (let ((sym (gentemp)))
      (signals (error)
        (read-from-string (format nil "#,(~S 1)" sym)))
      (unintern sym))))

(test side-effects-check
  (is-false *original-readtable*)
  (is-false *restore-read-time-application*)
  ;;
  (enable-read-time-application)
  (disable-read-time-application)
  (is-false *original-readtable*)
  (is-false *restore-read-time-application*))


#|(progn
  (enable-read-time-application)
  (define-reader-ctor 'u8 #'srfi-4:u8vector))|#


#|(progn
  #,(u8 1 2 3 4))|#





;;;; srfi-10.asd

(cl:in-package :asdf)

(defsystem :srfi-10
  :version "1"
  :description "SRFI-10 for CL: #, external form"
  :long-description "SRFI-10 for CL: #, external form
https://srfi.schemers.org/srfi-10"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :components ((:file "package")
               (:file "srfi-10")))

(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-10))))
  (let ((name "https://github.com/g000001/srfi-10")
        (nickname :srfi-10))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))

(defsystem :srfi-10.test
  :version "1"
  :description "SRFI-10 for CL: #, external form"
  :long-description "SRFI-10 for CL: #, external form
https://srfi.schemers.org/srfi-10"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on (:srfi-10 :fiveam))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-10.test))))
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :5am :run) (_ "https://github.com/g000001/srfi-10#internals" :srfi-10))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


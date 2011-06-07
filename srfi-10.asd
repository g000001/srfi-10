;;;; srfi-10.asd

(cl:in-package :asdf)

(defsystem :srfi-10
  :serial t
  :components ((:file "package")
               (:file "srfi-10")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-10))))
  (load-system :srfi-10)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-10-internal :srfi-10))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


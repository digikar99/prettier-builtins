(defpackage :prettier-builtins
  (:use :cl :alexandria)
  (:export #:prettify
           #:unprettify
           #:*print-array*
           #:*print-hash-table*
           #:*hash-table-print-spec*))

(in-package :prettier-builtins)

(defvar *pretty-types-alist* ())

(defvar *pprint-table* (copy-pprint-dispatch))

(defun prettify (&rest types)
  (loop :for type :in types
        :do (if (assoc type *pretty-types-alist* :test #'type=)
                (set-pprint-dispatch type
                                     (cdr (assoc type *pretty-types-alist*
                                                 :test #'type=)))
                (cerror "Continue"
                        "No pretty printer provided for ~S" type))))

(defun unprettify (&rest types)
  (dolist (type types)
    (set-pprint-dispatch type nil)))

(in-package :prettier-builtins)

(defvar *hash-table-print-spec* '(4 "=>" 20)
  "Should be a list of 3 elements indicating:
  the tab-size used before and after the separator
  the separator between the key and value as a string
  the minimum offset column after which to place the separator")

(defvar *print-hash-table* t
  "When NIL, pretty-printing does not print the elements of the hash-table.")

(defun pretty-print-hash-table (stream hash-table)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (hash-table stream :type t :identity t)
      (format stream ":TEST ~S :COUNT ~D"
              (hash-table-test hash-table)
              (hash-table-count hash-table))
      (when *print-hash-table*
        (let ((count 0))
          (unless (zerop (hash-table-count hash-table))
            (pprint-newline :mandatory stream))
          (destructuring-bind (colinc sep colnum) *hash-table-print-spec*
            (block print
              (maphash (lambda (key value)
                         (when (and (boundp '*print-length*)
                                    (realp *print-length*)
                                    (<= *print-length* count))
                           (write-string "..." stream)
                           (pprint-newline :mandatory stream)
                           (return-from print))
                         (let ((key-str   (with-output-to-string (stream)
                                            (format stream "~S" key))))
                           (write-string key-str stream)
                           (when (<= colnum (length key-str))
                             (pprint-newline :mandatory stream))
                           (pprint-tab :line colnum colinc stream)
                           (write-string sep stream)
                           (pprint-tab :line colnum colinc stream)
                           (write value :stream stream :pretty t))
                         (pprint-newline :mandatory stream)
                         (incf count))
                       hash-table))))))))

(pushnew (cons 'hash-table #'pretty-print-hash-table)
         *pretty-types-alist* :test #'type= :key #'car)

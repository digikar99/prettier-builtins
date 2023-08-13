(in-package :prettier-builtins)

(defun pretty-print-number (stream arg colon-modifier-p at-modifier-p)
  (declare (ignore colon-modifier-p at-modifier-p))
  (unless (typep arg 'number)
    (return-from pretty-print-number (write arg :stream stream :pretty t)))
  (let ((number arg))
    (if (or (>= (abs number) 100) (< (abs number) 0.001))
        (format stream
                (typecase number
                  (float (if (< number 0)
                             " ~9,3,2e"
                             " ~10,3,2e"))
                  (t     "~s"))
              arg)
        (format stream
                (typecase number
                  (float "~7,3,,,f    ")
                  (t      "~s"))
              arg))))

(defvar *array-element-print-format* "~/PRETTIER-BUILTINS::PRETTY-PRINT-NUMBER/"
  "The format control string used to print the elements of DENSE-ARRAYS:ARRAY.

It is possible to set this value to \"~/USER-DEFINED-FUNCTION/\" where
USER-DEFINED-FUNCTION should accept at least four arguments.

Also see:
- https://en.wikipedia.org/wiki/Format_(Common_Lisp)
- http://www.gigamonkeys.com/book/a-few-format-recipes.html")

(declaim (ftype (function (cl:array) (cl:simple-array * 1))))
(defun array-storage (array)
  (declare (ignorable array)
           (optimize speed))
  (loop :with array := array
        :do (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
              (typecase array
                ((cl:simple-array * (*)) (return array))
                (cl:simple-array (return #+sbcl (sb-ext:array-storage-vector array)
                                         #+ccl (ccl::%array-header-data-and-offset array)
                                         #-(or sbcl ccl)
                                         (error "Don't know how to obtain ARRAY-STORAGE on ~S"
                                                (lisp-implementation-type))))
                (t (setq array (cl:array-displacement array)))))))

(defun array-stride (array axis)
  (loop :for d :in (subseq (array-dimensions array) (1+ axis))
        :with stride := 1
        :do (setq stride (* d stride))
        :finally (return stride)))

(defun pretty-print-array (stream array)
  ;; (print (type-of array))
  (let* ((*print-right-margin* (or *print-right-margin* 80))
         (sv      (array-storage array))
         (layout  :row-major)
         (rank    (array-rank array))
         (index   0)
         (fmt-control (or *array-element-print-format*
                          (switch ((array-element-type array) :test #'type=)
                            ('double-float "~,15,3@e")
                            ('single-float "~,7,2@e")
                            (t             "~s"))))
         (*print-level* (if *print-level*
                            (1+ *print-level*)
                            *print-level*)))
    ;; Do variable declarations before just to save some horizontal space
    (labels ((data-as-lol (&optional (depth 0))
               ;; Get the relevant data from storage vector as a
               ;; potentially nested list (list of lists)
               (cond ((= depth rank)
                      (format nil fmt-control (aref sv index)))
                     ((and *print-level* (= depth *print-level*))
                      "#")
                     (t
                      (loop :with dim := (array-dimension array depth)
                            :with print-length
                              := (if *print-length*
                                     (min dim *print-length*)
                                     dim)
                            :with offset := (if (cl:= 0 depth)
                                                (nth-value
                                                 1 (array-displacement array))
                                                0)
                            :with stride := (array-stride array depth)
                              :initially (incf index offset)
                            :repeat print-length
                            :collect (data-as-lol (1+ depth)) :into data
                            :do (incf index stride)
                            :finally
                               (decf index
                                     (+ offset (* stride print-length)))
                               (return (nconc data
                                              (when (< print-length dim)
                                                '("..."))))))))
             (pretty-print-new-line (stream)
               #-ccl (pprint-newline :mandatory stream)
               #+ccl (format stream "~%  ")))
      (let* ((items (data-as-lol))
             (len   (length items))
             (num-lines 3))
        (pprint-logical-block (stream items)
          (print-unreadable-object (array stream :identity t)
            (format stream "~S ~S " (class-name (class-of array)) layout)
            ;; header
            (if (zerop rank)
                (format stream "NIL ~S"
                        (array-element-type array))
                (format stream "~{~S~^x~} ~S"
                        (array-dimensions array)
                        (array-element-type array)))
            (when (and *print-array*
                       (not (or (zerop (array-total-size array))
                                (and *print-lines* (< *print-lines* 3))
                                (and *print-level*
                                     (< *print-level* 1)
                                     (progn
                                       (write-string " #" stream)
                                       t)))))
              (pretty-print-new-line stream)
              ;; DONE: *print-level*
              ;; DONE: *print-lines*
              ;; DONE: *print-length*
              ;; print the array elements
              (cond ((zerop rank)
                     (format stream " ~A" (aref sv 0)))
                    (t
                     (loop :for item :in items
                           :for i :below len
                           :do (let* ((printed-item (format nil "~A" item))
                                      (newline-count (1+ (count #\newline printed-item)))
                                      (printed-lines
                                        (uiop:split-string printed-item
                                                           :separator '(#\newline))))
                                 (when (or (null *print-lines*)
                                           (and *print-lines*
                                                (<= (+ num-lines newline-count)
                                                    *print-lines*)))
                                   (dolist (line printed-lines)
                                     (write-string line stream)
                                     (pretty-print-new-line stream))
                                   (when (= i (1- len))
                                     (pprint-indent :block -2 stream))
                                   (incf num-lines newline-count))
                                 (when (and *print-lines*
                                            (not (<= (+ num-lines newline-count)
                                                     *print-lines*))
                                            (< i (1- len)))
                                   ;; We have more items to print, but no more lines
                                   (format stream "..")
                                   (pprint-indent :block -2 stream)
                                   (pretty-print-new-line stream)
                                   (return)))))))))))))

(pushnew (cons '(and array (not string)) #'pretty-print-array)
         *pretty-types-alist* :test #'type= :key #'car)

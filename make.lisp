
;; $Id: make.lisp,v 1.7 2007/08/12 13:01:13 alemmens Exp $

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *rucksack-directory* *load-pathname*))

(defun make (&key (debug t))
  (when debug
    (proclaim '(optimize (debug 3) (speed 0) (space 0))))
  (with-compilation-unit ()
    (loop for file in '("queue"
                        "package"
                        "errors"
                        "mop"
                        "serialize" 
                        "heap"
                        "object-table"
                        "schema-table"
                        "garbage-collector"
                        "cache"
                        "objects"
                        "p-btrees"
                        "index"
                        "rucksack"
                        "transactions"
                        "test")
          do (tagbody
              :retry
              (let ((lisp (make-pathname :name file
                                         :type "lisp"
                                         :defaults *rucksack-directory*)))
                (multiple-value-bind (fasl warnings failure)
                    (compile-file lisp)
                  (declare (ignore warnings))
                  (when failure
                    (restart-case
                        (error "COMPILE-FILE reported failure on ~A" lisp)
                      (retry ()
                        :report "Retry compilation"
                        (go :retry))
                      (continue ()
                        :report "Load resulting fasl anyway"
                        nil)))
                  (load fasl)))))))

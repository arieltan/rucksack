;; $Id: test-index-1b.lisp,v 1.1 2008/01/23 15:49:07 alemmens Exp $

(in-package :rs-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class redefinition example
;;;
;;; To run this example:
;;; - First run the indexing example in test-index-1a.lisp.
;;; - Compile and load this file
;;;   This will change the class definition of HACKER.
;;;   Because of this change, Rucksack will remove some slot indexes and
;;;   create (and fill) other slot indexes.
;;; - (SHOW-HACKERS)
;;;   Notice that "Hackers indexed by hacker-id." now doesn't list any hackers,
;;;   because the ID index was removed.
;;; - (SHOW-HACKERS-BY-AGE)
;;;   This will print the hackers sorted by age.  It shows that:
;;;   (1) the existing hackers all got a new age slot, initialized by
;;;       UPDATE-PERSISTENT-INSTANCE-FOR-REDEFINED-CLASS to a random
;;;       number according to their initform
;;;   (2) a new index has been created for the new age slot
;;;   (3) the index has been filled with the new values for the age slot.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-rucksack (rs *hacker-rucksack*)
  (with-transaction ()

    ;; For classes that may change during program development, you should
    ;; wrap all class definitions in a WITH-RUCKSACK to make sure that
    ;; the corresponding schemas and indexes are updated correctly.
    
    ;; In this case we redefine the HACKER class: we remove the index for
    ;; the ID slot, and we add a new AGE slot (with an index).

    (defclass hacker ()
      ((id :initform (gensym "HACKER-")
           :reader hacker-id)
       (name :initform (random-elt *hackers*)
             :accessor name
             :index :case-insensitive-string-index)
       (age :initform (random 100)
            :accessor age
            :index :number-index))
      (:metaclass persistent-class)
      (:index t))))

(defun show-hackers-by-age ()
  (with-rucksack (rs *hacker-rucksack*)
    (with-transaction ()
      (print "Hackers by age.")
      (rucksack-map-slot rs 'hacker 'age
                         (lambda (hacker)
                           (format t "~&~A has age ~D.~%"
                                   (name hacker)
                                   (age hacker)))))))


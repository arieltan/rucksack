;; $Id: test-schema-update-1b.lisp,v 1.1 2008/01/23 15:49:07 alemmens Exp $

(in-package :rucksack-test-schema-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema updates and UPDATE-INSTANCE-FOR-REDEFINED-CLASS, part 2 of 3
;;;
;;; Run this example after test-schema-update-1a.lisp:
;;;
;;; - Compile and load this file
;;; - Evaluate (TEST-2)
;;; - Then move on to test-schema-update-1c.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Redefine the PERSON class
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-rucksack (rs *dir*)
    (with-transaction ()
      
      (defclass person ()
        ((name :initarg :name
               :initform (elt *names* (random (length *names*)))
               :reader name)
         (year-of-birth :initform (random-year)
                        :accessor year-of-birth))
        (:metaclass persistent-class)
        (:index t)))))

(defconstant +this-year+ 2006)

(defun random-year ()
  (+ 1900 (random 100)))

(defmethod update-persistent-instance-for-redefined-class
           ((person person) added-slots discarded-slots plist
            &key &allow-other-keys)
  ;; Make sure that existing PERSONS get the YEAR-OF-BIRTH value
  ;; corresponding to their (obsolete) AGE slot.
  (let ((age (getf plist 'age)))
    (setf (year-of-birth person) (- +this-year+ age))
    (format *trace-output*
            "~&Setting year of birth for ~D to ~D."
            age
            (year-of-birth person))))

(defmethod age ((person person))
  ;; Make sure that the AGE method still works.
  (- +this-year+ (year-of-birth person)))


(defun test-2 ()
  ;; Create some persons with the new class definition.
  (with-rucksack (rs *dir*)
    (with-transaction ()
      (loop repeat 10
            do (make-instance 'person))))

  ;; Show some PERSON instances and some old PERSON instances.
  ;; (We don't show all PERSON instances, because showing them may
  ;; update them and we want to keep a few old instances for the next
  ;; part of the test).
  
  (with-rucksack (rs *dir*)
    (with-transaction ()
      (let ((cache (rucksack-cache rs))
            (count 0))
        (rucksack-map-class rs 'person
                            (lambda (id)
                              (when (evenp count)
                                (print (cache-get-object id cache)))
                              (incf count))
                            :id-only t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;; Some old PERSON instances (updated after being loaded).
Setting year of birth for 77 to 1929.
#<PERSON #12 JOHN with age 77> 
Setting year of birth for 39 to 1967.
#<PERSON #24 JOHN with age 95> 
Setting year of birth for 41 to 1965.
#<PERSON #28 JANE with age 17> 
Setting year of birth for 75 to 1931.
#<PERSON #32 PETER with age 88> 
Setting year of birth for 11 to 1995.
#<PERSON #36 MARY with age 49> 
Setting year of birth for 72 to 1934.

;; Some new PERSON instances.
#<PERSON #38 RONALD with age 72> 
#<PERSON #42 JOHN with age 50> 
#<PERSON #46 DICK with age 57> 
#<PERSON #50 DICK with age 22> 
#<PERSON #54 MARY with age 82> 
#<PERSON #58 JANE with age 84>

|#


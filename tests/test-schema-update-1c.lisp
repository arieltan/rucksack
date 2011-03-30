;; $Id: test-schema-update-1c.lisp,v 1.1 2008/01/23 15:49:07 alemmens Exp $

(in-package :rucksack-test-schema-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema updates and UPDATE-INSTANCE-FOR-REDEFINED-CLASS, part 3 of 3
;;;
;;; Run this example after test-schema-update-1b.lisp:
;;; - compile and load this file
;;; - evaluate (TEST-3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Redefine the PERSON class once more.
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-rucksack (rs *dir*)
    (with-transaction ()
      
      (defclass person ()
        ((name :initarg :name
               :initform (elt *names* (random (length *names*)))
               :reader name)
         (date-of-birth :accessor date-of-birth
                        :initform (random-date)))
        (:metaclass persistent-class)
        (:index t)))))

(defun random-date ()
  (make-date (random-year)
             (+ 1 (random 12))
             (+ 1 (random 28))))

(defun make-date (year &optional (month 1) (day 1))
  (encode-universal-time 0 0 0 day month year))

(defun date-string (universal-time)
  (multiple-value-bind (sec min hr day month year)
      (decode-universal-time universal-time)
    (declare (ignore sec min hr))
    (format nil "~D-~2,'0D-~2,'0D"
            year month day)))

          
(defmethod update-persistent-instance-for-redefined-class
           ((person person) added-slots discarded-slots plist
            &key &allow-other-keys)
  ;; Now we need to deal with version 0 persons (with an obsolete
  ;; AGE slot) and with version 1 persons (with an obsolete
  ;; YEAR-OF-BIRTH slot).
  (cond ((member 'age discarded-slots)
         ;; Version 0
         (let* ((age (getf plist 'age))
                (year (- +this-year+ age)))
           (setf (date-of-birth person) (make-date year 1 1))
           (format *trace-output*
                   "~&Setting date of birth from age ~D to ~A."
                   age
                   (date-string (date-of-birth person)))))
        ((member 'year-of-birth discarded-slots)
         ;; Version 1
         (let ((year (getf plist 'year-of-birth)))
           (setf (date-of-birth person) (make-date year 1 1))
           (format *trace-output*
                   "~&Setting date of birth from year ~D to ~A."
                   year
                   (date-string (date-of-birth person)))))))


(defmethod year-of-birth ((person person))
  ;; Make sure that the YEAR-OF-BIRTH method still works.
  (nth-value 5 (decode-universal-time (date-of-birth person))))


(defun test-3 ()
  ;; Create some persons with the second version of the class definition.
  (with-rucksack (rs *dir*)
    (with-transaction ()
      (loop repeat 10
            do (make-instance 'person))))
  ;; Show all persons (for three versions of the class definition).
  (with-rucksack (rs *dir*)
    (with-transaction ()
      (rucksack-map-class rs 'person #'print))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that we see three different kinds of output, corresponding to the
;;; three class versions.
;;;
;;; Output like:
;;;
;;;   Setting date of birth from age 6 to 2000-01-01.
;;;   #<PERSON #26 MARY with age 6> 
;;;
;;; is for version 0 instances that are updated to version 2.
;;;
;;; Output like:
;;;
;;;   Setting date of birth from year 2001 to 2001-01-01.
;;;   #<PERSON #12 PETER with age 5> 
;;;
;;; is for version 1 PERSON instances that are updated to version 2.
;;;
;;; And output like:
;;;
;;;   #<PERSON #60 JANE with age 26> 
;;;
;;; is for version 2 instances (that don't need to be updated).

;;; Note also that you'll get this kind of output only once.  If you load
;;; the file again, all old version instances have been updated already
;;; so you won't see any "Setting date of birth..." messages anymore.

#|             
Setting date of birth from year 2001 to 2001-01-01.
#<PERSON #12 PETER with age 5> 
Setting date of birth from age 46 to 1960-01-01.
#<PERSON #22 MARY with age 46> 
Setting date of birth from year 1955 to 1955-01-01.
#<PERSON #24 PETER with age 51> 
Setting date of birth from age 6 to 2000-01-01.
#<PERSON #26 MARY with age 6> 
Setting date of birth from year 1920 to 1920-01-01.
#<PERSON #28 PETER with age 86> 
Setting date of birth from age 33 to 1973-01-01.
#<PERSON #30 MARY with age 33> 
Setting date of birth from year 1917 to 1917-01-01.
#<PERSON #32 JANE with age 89> 
Setting date of birth from age 15 to 1991-01-01.
#<PERSON #34 JOHN with age 15> 
Setting date of birth from year 1922 to 1922-01-01.
#<PERSON #36 DICK with age 84> 
Setting date of birth from age 26 to 1980-01-01.
#<PERSON #38 DICK with age 26> 
Setting date of birth from year 1960 to 1960-01-01.
#<PERSON #40 JANE with age 46> 
Setting date of birth from year 1918 to 1918-01-01.
#<PERSON #42 RONALD with age 88> 
Setting date of birth from year 1929 to 1929-01-01.
#<PERSON #44 PETER with age 77> 
Setting date of birth from year 1950 to 1950-01-01.
#<PERSON #46 JANE with age 56> 
Setting date of birth from year 1927 to 1927-01-01.
#<PERSON #48 DICK with age 79> 
Setting date of birth from year 1929 to 1929-01-01.
#<PERSON #50 RONALD with age 77> 
Setting date of birth from year 1939 to 1939-01-01.
#<PERSON #52 PETER with age 67> 
Setting date of birth from year 1993 to 1993-01-01.
#<PERSON #54 JOHN with age 13> 
Setting date of birth from year 1919 to 1919-01-01.
#<PERSON #56 JANE with age 87> 
Setting date of birth from year 1953 to 1953-01-01.
#<PERSON #58 PETER with age 53> 
#<PERSON #60 JANE with age 26> 
#<PERSON #62 RONALD with age 105> 
#<PERSON #64 MARY with age 55> 
#<PERSON #66 JANE with age 94> 
#<PERSON #68 DICK with age 98> 
#<PERSON #70 RONALD with age 60> 
#<PERSON #72 PETER with age 10> 
#<PERSON #74 DICK with age 71> 
#<PERSON #76 JOHN with age 98> 
#<PERSON #78 RONALD with age 73>

|#

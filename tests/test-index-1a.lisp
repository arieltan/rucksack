;; $Id: test-index-1a.lisp,v 1.1 2008/01/23 15:49:07 alemmens Exp $

(in-package :rucksack-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing example
;;;
;;; To run this example:
;;; - compile and load this file
;;; - (IN-PACKAGE :RUCKSACK-TEST)
;;; - (CREATE-HACKERS)
;;; - (SHOW-HACKERS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *hackers* '("David" "Jim" "Peter" "Thomas"
                          "Arthur" "Jans" "Klaus" "James" "Martin"))

(defun random-elt (list)
  (elt list (random (length list))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; NOTE: The EVAL-WHEN above is necessary to ensure that the compiler
  ;; 'knows about' the HACKER class when it compiles the PRINT-OBJECT method
  ;; for HACKER.  We could avoid this by splitting this file into two:
  ;; the first one would contain the class definitions, and the second 
  ;; would contain everything else (especially methods that specialize on one
  ;; of the classes defined in the first one).

  (defparameter *hacker-rucksack* #p"/tmp/rucksack/hackers/")

  (with-rucksack (rs *hacker-rucksack* :if-exists :supersede)
    (with-transaction ()

      ;; We define some persistent classes with indexed slots.
      ;; So we must wrap the class definition in a WITH-RUCKSACK,
      ;; otherwise the indexes can't be built.

      (defclass hacker ()
        ((id :initform (gensym "HACKER-")
             :reader hacker-id
             :index :symbol-index
             :unique t)
         (name :initform (random-elt *hackers*)
               :accessor name
               :index :case-insensitive-string-index))
        (:metaclass persistent-class)
        (:index t))
      
      (defclass lisp-hacker (hacker)
        ()
        (:metaclass persistent-class)
        (:index t)))))


(defmethod print-object ((hacker hacker) stream)
  (print-unreadable-object (hacker stream :type t)
    (format stream "~S called ~S"
            (hacker-id hacker)
            (name hacker))))

(defun create-hackers ()
  (with-rucksack (rs *hacker-rucksack*)
    ;; Fill the rucksack with some hackers.
    (with-transaction ()
      (loop repeat 20
            do (make-instance 'hacker))
      (loop repeat 10
            do (make-instance 'lisp-hacker))
      (rucksack-map-class rs 'hacker #'print))))

(defun show-hackers ()
  (with-rucksack (rs *hacker-rucksack*)
    (with-transaction ()
      (print "Hackers indexed by object id.")
      (rucksack-map-class rs 'hacker #'print)
      (print "Hackers indexed by name.")
      (rucksack-map-slot rs 'hacker 'name #'print)
      (print "Hackers indexed by hacker-id.")
      (rucksack-map-slot rs 'hacker 'id #'print)
      (print "Lisp hackers.")
      (rucksack-map-class rs 'lisp-hacker #'print)
      (print "Non-lisp hackers.")
      (rucksack-map-class rs 'hacker #'print
                          :include-subclasses nil)
      (print "Hacker object ids.")
      (rucksack-map-class rs 'hacker #'print
                          :id-only t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

RS-TEST 3 > (create-hackers)

#<HACKER #:HACKER-9234 called "Martin"> 
#<HACKER #:HACKER-9235 called "Martin"> 
#<HACKER #:HACKER-9236 called "Martin"> 
#<HACKER #:HACKER-9237 called "Jim"> 
#<HACKER #:HACKER-9238 called "Thomas"> 
#<HACKER #:HACKER-9239 called "David"> 
#<HACKER #:HACKER-9240 called "Thomas"> 
#<HACKER #:HACKER-9241 called "Jim"> 
#<HACKER #:HACKER-9242 called "Martin"> 
#<HACKER #:HACKER-9243 called "Jim"> 
#<HACKER #:HACKER-9244 called "Peter"> 
#<HACKER #:HACKER-9245 called "Jim"> 
#<HACKER #:HACKER-9246 called "Thomas"> 
#<HACKER #:HACKER-9247 called "Jans"> 
#<HACKER #:HACKER-9248 called "Peter"> 
#<HACKER #:HACKER-9249 called "Peter"> 
#<HACKER #:HACKER-9250 called "Arthur"> 
#<HACKER #:HACKER-9251 called "Thomas"> 
#<HACKER #:HACKER-9252 called "James"> 
#<HACKER #:HACKER-9253 called "Martin"> 
#<LISP-HACKER #:HACKER-9254 called "Jans"> 
#<LISP-HACKER #:HACKER-9255 called "Martin"> 
#<LISP-HACKER #:HACKER-9256 called "Thomas"> 
#<LISP-HACKER #:HACKER-9257 called "Klaus"> 
#<LISP-HACKER #:HACKER-9258 called "David"> 
#<LISP-HACKER #:HACKER-9259 called "Thomas"> 
#<LISP-HACKER #:HACKER-9260 called "David"> 
#<LISP-HACKER #:HACKER-9261 called "James"> 
#<LISP-HACKER #:HACKER-9262 called "Peter"> 
#<LISP-HACKER #:HACKER-9263 called "Peter"> 
NIL
T

RS-TEST 4 > (show-hackers)

"Hackers indexed by object id." 
#<HACKER #:HACKER-9234 called "Martin"> 
#<HACKER #:HACKER-9235 called "Martin"> 
#<HACKER #:HACKER-9236 called "Martin"> 
#<HACKER #:HACKER-9237 called "Jim"> 
#<HACKER #:HACKER-9238 called "Thomas"> 
#<HACKER #:HACKER-9239 called "David"> 
#<HACKER #:HACKER-9240 called "Thomas"> 
#<HACKER #:HACKER-9241 called "Jim"> 
#<HACKER #:HACKER-9242 called "Martin"> 
#<HACKER #:HACKER-9243 called "Jim"> 
#<HACKER #:HACKER-9244 called "Peter"> 
#<HACKER #:HACKER-9245 called "Jim"> 
#<HACKER #:HACKER-9246 called "Thomas"> 
#<HACKER #:HACKER-9247 called "Jans"> 
#<HACKER #:HACKER-9248 called "Peter"> 
#<HACKER #:HACKER-9249 called "Peter"> 
#<HACKER #:HACKER-9250 called "Arthur"> 
#<HACKER #:HACKER-9251 called "Thomas"> 
#<HACKER #:HACKER-9252 called "James"> 
#<HACKER #:HACKER-9253 called "Martin"> 
#<LISP-HACKER #:HACKER-9254 called "Jans"> 
#<LISP-HACKER #:HACKER-9255 called "Martin"> 
#<LISP-HACKER #:HACKER-9256 called "Thomas"> 
#<LISP-HACKER #:HACKER-9257 called "Klaus"> 
#<LISP-HACKER #:HACKER-9258 called "David"> 
#<LISP-HACKER #:HACKER-9259 called "Thomas"> 
#<LISP-HACKER #:HACKER-9260 called "David"> 
#<LISP-HACKER #:HACKER-9261 called "James"> 
#<LISP-HACKER #:HACKER-9262 called "Peter"> 
#<LISP-HACKER #:HACKER-9263 called "Peter"> 
"Hackers indexed by name." 
#<HACKER #:HACKER-9250 called "Arthur"> 
#<LISP-HACKER #:HACKER-9260 called "David"> 
#<LISP-HACKER #:HACKER-9258 called "David"> 
#<HACKER #:HACKER-9239 called "David"> 
#<LISP-HACKER #:HACKER-9261 called "James"> 
#<HACKER #:HACKER-9252 called "James"> 
#<LISP-HACKER #:HACKER-9254 called "Jans"> 
#<HACKER #:HACKER-9247 called "Jans"> 
#<HACKER #:HACKER-9245 called "Jim"> 
#<HACKER #:HACKER-9243 called "Jim"> 
#<HACKER #:HACKER-9241 called "Jim"> 
#<HACKER #:HACKER-9237 called "Jim"> 
#<LISP-HACKER #:HACKER-9257 called "Klaus"> 
#<LISP-HACKER #:HACKER-9255 called "Martin"> 
#<HACKER #:HACKER-9253 called "Martin"> 
#<HACKER #:HACKER-9242 called "Martin"> 
#<HACKER #:HACKER-9236 called "Martin"> 
#<HACKER #:HACKER-9235 called "Martin"> 
#<HACKER #:HACKER-9234 called "Martin"> 
#<LISP-HACKER #:HACKER-9263 called "Peter"> 
#<LISP-HACKER #:HACKER-9262 called "Peter"> 
#<HACKER #:HACKER-9249 called "Peter"> 
#<HACKER #:HACKER-9248 called "Peter"> 
#<HACKER #:HACKER-9244 called "Peter"> 
#<LISP-HACKER #:HACKER-9259 called "Thomas"> 
#<LISP-HACKER #:HACKER-9256 called "Thomas"> 
#<HACKER #:HACKER-9251 called "Thomas"> 
#<HACKER #:HACKER-9246 called "Thomas"> 
#<HACKER #:HACKER-9240 called "Thomas"> 
#<HACKER #:HACKER-9238 called "Thomas"> 
"Hackers indexed by hacker-id." 
#<HACKER #:HACKER-9234 called "Martin"> 
#<HACKER #:HACKER-9235 called "Martin"> 
#<HACKER #:HACKER-9236 called "Martin"> 
#<HACKER #:HACKER-9237 called "Jim"> 
#<HACKER #:HACKER-9238 called "Thomas"> 
#<HACKER #:HACKER-9239 called "David"> 
#<HACKER #:HACKER-9240 called "Thomas"> 
#<HACKER #:HACKER-9241 called "Jim"> 
#<HACKER #:HACKER-9242 called "Martin"> 
#<HACKER #:HACKER-9243 called "Jim"> 
#<HACKER #:HACKER-9244 called "Peter"> 
#<HACKER #:HACKER-9245 called "Jim"> 
#<HACKER #:HACKER-9246 called "Thomas"> 
#<HACKER #:HACKER-9247 called "Jans"> 
#<HACKER #:HACKER-9248 called "Peter"> 
#<HACKER #:HACKER-9249 called "Peter"> 
#<HACKER #:HACKER-9250 called "Arthur"> 
#<HACKER #:HACKER-9251 called "Thomas"> 
#<HACKER #:HACKER-9252 called "James"> 
#<HACKER #:HACKER-9253 called "Martin"> 
#<LISP-HACKER #:HACKER-9254 called "Jans"> 
#<LISP-HACKER #:HACKER-9255 called "Martin"> 
#<LISP-HACKER #:HACKER-9256 called "Thomas"> 
#<LISP-HACKER #:HACKER-9257 called "Klaus"> 
#<LISP-HACKER #:HACKER-9258 called "David"> 
#<LISP-HACKER #:HACKER-9259 called "Thomas"> 
#<LISP-HACKER #:HACKER-9260 called "David"> 
#<LISP-HACKER #:HACKER-9261 called "James"> 
#<LISP-HACKER #:HACKER-9262 called "Peter"> 
#<LISP-HACKER #:HACKER-9263 called "Peter"> 
"Lisp hackers." 
#<LISP-HACKER #:HACKER-9254 called "Jans"> 
#<LISP-HACKER #:HACKER-9255 called "Martin"> 
#<LISP-HACKER #:HACKER-9256 called "Thomas"> 
#<LISP-HACKER #:HACKER-9257 called "Klaus"> 
#<LISP-HACKER #:HACKER-9258 called "David"> 
#<LISP-HACKER #:HACKER-9259 called "Thomas"> 
#<LISP-HACKER #:HACKER-9260 called "David"> 
#<LISP-HACKER #:HACKER-9261 called "James"> 
#<LISP-HACKER #:HACKER-9262 called "Peter"> 
#<LISP-HACKER #:HACKER-9263 called "Peter"> 
"Non-lisp hackers." 
#<HACKER #:HACKER-9234 called "Martin"> 
#<HACKER #:HACKER-9235 called "Martin"> 
#<HACKER #:HACKER-9236 called "Martin"> 
#<HACKER #:HACKER-9237 called "Jim"> 
#<HACKER #:HACKER-9238 called "Thomas"> 
#<HACKER #:HACKER-9239 called "David"> 
#<HACKER #:HACKER-9240 called "Thomas"> 
#<HACKER #:HACKER-9241 called "Jim"> 
#<HACKER #:HACKER-9242 called "Martin"> 
#<HACKER #:HACKER-9243 called "Jim"> 
#<HACKER #:HACKER-9244 called "Peter"> 
#<HACKER #:HACKER-9245 called "Jim"> 
#<HACKER #:HACKER-9246 called "Thomas"> 
#<HACKER #:HACKER-9247 called "Jans"> 
#<HACKER #:HACKER-9248 called "Peter"> 
#<HACKER #:HACKER-9249 called "Peter"> 
#<HACKER #:HACKER-9250 called "Arthur"> 
#<HACKER #:HACKER-9251 called "Thomas"> 
#<HACKER #:HACKER-9252 called "James"> 
#<HACKER #:HACKER-9253 called "Martin"> 
"Hacker object ids." 
36 
65 
69 
73 
78 
83 
88 
92 
96 
100 
104 
109 
113 
117 
122 
126 
130 
135 
139 
144 
148 
160 
164 
168 
173 
177 
181 
185 
189 
193 
NIL
T

|#

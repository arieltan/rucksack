;; $Id: queue.lisp,v 1.4 2007/01/20 18:17:55 alemmens Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Usage:

- Create a queue with (make-instance 'queue)

- The rest should be obvious.
|#

(defpackage :queue
  (:use :common-lisp)
  (:export
   #:queue
   #:queue-size
   #:queue-add #:queue-add-at-front
   #:queue-empty-p #:queue-peek
   #:queue-remove #:queue-clear
   #:empty-queue-error))

(in-package :queue)

;;; 
;;; QUEUE
;;;

(defclass queue ()
  ((end :initform nil)
   (contents :initform '())
   (size :initform 0 :reader queue-size)))

(define-condition empty-queue-error (error)
  ((queue :initarg :queue))
  (:report (lambda (error stream)
             (with-slots (queue)
                 error
               (format stream "Queue ~A is empty." queue)))))

          
(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "of size ~D" (queue-size queue))))


(defun queue-add (queue object)
  "Adds an object to the end of the queue."
  (with-slots (end contents size)
      queue
    (cond ((null end)
           (setf contents (list object))
           (setf end contents))
          (t 
           (setf (cdr end) (list object))
           (setf end (cdr end))))
    (incf size))
  queue)

(defun queue-add-at-front (queue object)
  (with-slots (end contents size)
      queue
    (cond ((null end)
           (setf contents (list object))
           (setf end contents))
          (t (push object contents)))
    (incf size))
  queue)

(defun queue-remove (queue &key errorp)
  "Returns the first (i.e. least recently added) element of the queue.
If the queue is empty, it returns nil (when :ERRORP is nil) or signals
an empty-queue-error (when :ERRORP is true)."
  (with-slots (end contents size)
      queue
    (if (null contents)
        (and errorp
             (error 'empty-queue-error :queue queue))
      (prog1
          (pop contents)
        (when (null contents)
          (setq end nil))
        (decf size)))))


(defun queue-empty-p (queue)
  "Returns true if the queue is empty, otherwise nil."
  (with-slots (contents)
      queue
    (null contents)))

(defun queue-peek (queue &optional (type 't))
  "Returns the first object in the queue that has the given type (and removes
all objects from the queue before it).  Returns NIL (and clears the entire queue)
if there is no such object."
  (with-slots (contents size end)
      queue
    (loop while (and contents 
                     (not (typep (first contents) type)))
          do (decf size)
             (pop contents))
    (when (null contents)
      (setq end nil))
    (first contents)))


(defun queue-clear (queue)
  "Removes all elements from the queue (and returns the empty queue)."
  (with-slots (end contents size)
      queue
    (setf end nil
          contents '()
          size 0))
  queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

QUEUE> (setq *q* (make-instance 'queue))
#<QUEUE of size 0 @ #x21eed772>
QUEUE> (queue-add *q* "Hi")
#<QUEUE of size 1 @ #x21eed772>
QUEUE> (queue-peek *q*)
"Hi"
QUEUE> (queue-add *q* 123)
#<QUEUE of size 2 @ #x21eed772>
QUEUE> (queue-size *q*)
2
QUEUE> (queue-peek *q*)
"Hi"
QUEUE> (queue-remove *q*)
"Hi"
QUEUE> (queue-remove *q*)
123
QUEUE> (queue-remove *q*)
NIL
QUEUE> (queue-remove *q* :errorp t)
; Evaluation aborted
QUEUE> (queue-add *q* "Hi")
#<QUEUE of size 1 @ #x21eed772>
QUEUE> (queue-add *q* 123)
#<QUEUE of size 2 @ #x21eed772>
QUEUE> (queue-peek *q* 'integer)
123
QUEUE> (queue-size *q*)
1
QUEUE> (queue-add-at-front *q* "hi")
#<QUEUE of size 2 @ #x21eed772>
QUEUE> (queue-peek *q*)
"hi"

|#

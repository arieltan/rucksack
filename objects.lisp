;; $Id: objects.lisp,v 1.22 2008/02/19 22:44:06 alemmens Exp $

(in-package :rucksack)

(defvar *rucksack* nil
  "The current rucksack (NIL if there is no open rucksack).")

(defun current-rucksack ()
  *rucksack*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent objects API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Conventions:
;;  Persistent equivalents of CL functions always have a "p-" prefix.

(defgeneric object-id (object)
  (:documentation "Returns the object id of a persistent-object or
persistent-data."))

(defgeneric p-eql (x y)
  (:documentation "The persistent equivalent of EQL."))

#|
persistent-object
persistent-data
  persistent-cons
  persistent-array

p-cons
p-car
p-cdr
(setf p-car)
(setf p-cdr)
p-list

p-make-array
p-aref
(setf p-aref)
p-array-dimensions

p-length
p-find
p-replace
p-position
|#



(defmethod p-eql (a b)
  ;; Default method.
  (eql a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass proxy ()
  ((object-id :initarg :object-id :reader object-id)
   (rucksack :initform (current-rucksack) :initarg :rucksack :reader rucksack))
  (:documentation "Proxies are some kind of in-memory forwarding pointer
to data in the cache.  They are never saved on disk."))

(defparameter *dont-dereference-proxies* nil)

(defmethod maybe-dereference-proxy ((proxy proxy))
  (if *dont-dereference-proxies*
      proxy
    (cache-get-object (object-id proxy) (cache proxy))))

(defmethod maybe-dereference-proxy (object)
  ;; Default: just return the object.
  object)

(defun cache (object)
  (and (slot-boundp object 'rucksack)
       (rucksack object)
       (rucksack-cache (rucksack object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level persistent data structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persistent-data ()
  ((object-id :initarg :object-id :reader object-id)
   (transaction-id :reader transaction-id)
   (rucksack :initarg :rucksack :initform (current-rucksack) :reader rucksack)
   (contents :initarg :contents :accessor contents))
  (:documentation
 "PERSISTENT-DATA classes do not have PERSISTENT-CLASS as metaclass
because we don't want to specialize SLOT-VALUE-USING-CLASS & friends
for persistent-data instances.  Their contents are accessed by special
functions like P-CAR instead."))

(defmethod print-object ((object persistent-data) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "#~D~@[ with transaction id ~D~]"
            (slot-value object 'object-id)
            (and (slot-boundp object 'transaction-id)
                 (slot-value object 'transaction-id)))))

(defmethod compute-persistent-slot-names ((class standard-class)
                                          (object persistent-data))
  ;; Tell the schema table that instances of persistent-data have
  ;; one persistent slot: the CONTENTS slot.
  '(contents))

  
(defmethod p-eql ((a persistent-data) (b persistent-data))
  (= (object-id a) (object-id b)))

(defmethod persistent-data-read (function (data persistent-data) &rest args)
  (let ((value (apply function (contents data) args)))
    (if (typep value 'proxy)
        (maybe-dereference-proxy value)
      value)))

(defmethod persistent-data-write (function (data persistent-data) value
                                           &rest args)
  (apply function value (contents data) args)
  (cache-touch-object data (cache data))
  value)

(defun make-persistent-data (class contents
                                   &optional (rucksack (current-rucksack)))
  (let ((object (make-instance class
                               :contents contents
                               :rucksack rucksack))
        (cache (and rucksack (rucksack-cache rucksack))))
    (when cache
      (let ((object-id (cache-create-object object cache)))
        ;; Q: What about the transaction-id slot?
        ;; Do we need to set that too?
        (setf (slot-value object 'object-id) object-id)))
    object))



;;
;; Array
;;

(defclass persistent-array (persistent-data)
  ())

(defun p-make-array (dimensions &rest options &key &allow-other-keys)
  (let ((contents (apply #'make-array dimensions options)))
    (make-persistent-data 'persistent-array contents)))

(defmethod p-aref ((array persistent-array) &rest indices)
  (apply #'persistent-data-read #'aref array indices))

(defmethod (setf p-aref) (new-value (array persistent-array) &rest indices)
  (persistent-data-write (lambda (new-value contents)
                           (setf (apply #'aref contents indices) new-value))
                         array
                         new-value))

(defmethod p-array-dimensions ((array persistent-array))
  (persistent-data-read #'array-dimensions array))

;; DO: Other array functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Basics
;;

(defclass persistent-cons (persistent-data)
  ())

(defun p-cons (car cdr)
  (make-persistent-data 'persistent-cons (cons car cdr)))

(defmethod p-car ((cons persistent-cons))
  (persistent-data-read #'car cons))

(defmethod p-car ((x (eql nil)))
  nil)

(defmethod (setf p-car) (value (cons persistent-cons))
  (persistent-data-write (lambda (new-value contents)
                           (setf (car contents) new-value))
                         cons
                         value))

(defmethod p-cdr ((cons persistent-cons))
  (persistent-data-read #'cdr cons))

(defmethod p-cdr ((x (eql nil)))
  nil)

(defmethod (setf p-cdr) (value (cons persistent-cons))
  (persistent-data-write (lambda (new-value contents)
                           (setf (cdr contents) new-value))
                         cons
                         value))

(defun p-list (&rest objects)
  (if (endp objects)
      objects
    (p-cons (car objects)
            (apply #'p-list (cdr objects)))))

(defun unwrap-persistent-list (list)
  "Converts a persistent list to a 'normal' Lisp list."
  (loop until (p-endp list)
        collect (p-car list)
        do (setq list (p-cdr list))))

;;
;; Other functions from chapter 14 of the spec.
;;

(defun p-caar (object)
  "The persistent equivalent of CAAR."
  (p-car (p-car object)))

(defun p-cadr (object)
  "The persistent equivalenet of CADR."
  (p-car (p-cdr object)))

(defun p-cdar (object)
  "The persistent equivalent of CDAR."
  (p-cdr (p-car object)))

(defun p-cddr (object)
  "The persistent equivalent of CDDR."
  (p-cdr (p-cdr object)))


(defmethod p-consp ((object persistent-cons))
  t)

(defmethod p-consp ((object t))
  nil)


(defmethod p-endp ((object (eql nil)))
  t)

(defmethod p-endp ((object persistent-cons))
  nil)

(defmethod p-endp ((object t))
  (error 'type-error
         :datum object
         :expected-type '(or null persistent-cons)))


(defun p-last (list &optional (n 1))
  "Returns the last persistent cons cell of a persistent list (or
NIL if the list is empty)."
  (unless (= n 1)
    ;; DO: Implement this case.
    (error "The optional argument for P-LAST isn't implemented yet."))
  (let ((result list)
        (tail (p-cdr list)))
    (loop until (p-endp tail)
          do (shiftf result tail (p-cdr tail)))
    result))


(defun p-mapcar (function list)
  ;; DO: Accept more than one list argument.
  (let ((result '()))
    (loop while list do
          (setq result (p-cons (funcall function (p-car list))
                               result)
                list (p-cdr list)))
    (p-nreverse result)))

(defun p-mapc (function list)
  ;; DO: Accept more than one list argument.
  (let ((tail list))
    (loop while tail do
          (funcall function (p-car tail))
          (setq tail (p-cdr tail)))
    list))

(defun p-maplist (function list)
  ;; DO: Accept more than one list argument.
  (let ((result '()))
    (loop while list do
          (setq result (p-cons (funcall function list) result)
                list (p-cdr list)))
    (p-nreverse result)))

(defun p-mapl (function list)
  ;; DO: Accept more than one list argument.
  (let ((tail list))
    (loop while tail do
          (funcall function tail)
          (setq tail (p-cdr tail)))
    list))
  
(defun p-member-if (predicate list &key key)
  (unless key
    (setq key #'identity))
  (p-mapl (lambda (tail)
            (when (funcall predicate (funcall key (p-car tail)))
              (return-from p-member-if tail)))
          list)
  nil)


(defmacro p-pop (place &environment env)
  "Pop an item from the persistent list specified by PLACE."
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
       (prog1 (p-car ,(car new))
         (setq ,(car new) (p-cdr ,(car new)))
         ,setter))))


(defmacro p-push (item place &environment env)
  "Push ITEM onto the persistent list specified by PLACE.  Return the
modified persistent list. ITEM is evaluated before place."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (let ((item-var (gensym "ITEM")))
      `(let* ((,item-var ,item)
              ,@(mapcar #'list dummies vals)
              (,(car newval) (p-cons ,item-var ,getter)))
         ,setter))))


        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent sequence functions
;; (Just a start...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-p-vector (persistent-array function-name)
  (unless (= 1 (length (p-array-dimensions persistent-array)))
    (error "~S expected a persistent vector instead of ~S."
           function-name
           persistent-array)))

(defmethod p-length ((vector persistent-array))
  (check-p-vector vector 'p-length)
  (first (p-array-dimensions vector)))

(defmethod p-length ((list persistent-cons))
  ;; DO: Check for circular lists.
  (let ((result 0))
    (p-mapc (lambda (pair)
              (declare (ignore pair))
              (incf result))
            list)
    result))

(defmethod p-find (value (vector persistent-array)
                         &key (key #'identity) (test #'p-eql)
                         (start 0) (end nil))
  (check-p-vector vector 'p-find)
  (loop for i from start below (or end (p-length vector))
        do (let ((elt (funcall key (p-aref vector i))))
             (when (funcall test value elt)
               (return-from p-find (p-aref vector i)))))
  ;; Return nil if not found
  nil)

(defmethod p-find (value (list persistent-cons)
                         &key (key #'identity) (test #'p-eql)
                         (start 0) (end nil))
  ;; Move list to start position.
  (loop repeat start
        do (setq list (p-cdr list)))
  ;; The real work.
  (loop for i from start do
        (if (or (p-endp list) (and end (= i end)))
            (return-from p-find nil)
          (let ((elt (funcall key (p-car list))))
            (if (funcall test value elt)
                (return-from p-find (p-car list))
              (setq list (p-cdr list))))))
  ;; Return nil if not found.
  nil)

(defmethod p-find (value (list (eql nil)) &key &allow-other-keys)
  nil)

(defmethod p-position (value (vector persistent-array)
                             &key (key #'identity) (test #'p-eql)
                             (start 0) (end nil))
  (check-p-vector vector 'p-position)
  (loop for i from start below (or end (p-length vector))
        do (let ((elt (funcall key (p-aref vector i))))
             (when (funcall test value elt)
               (return-from p-position i))))
  ;; Return nil if not found
  nil)


(defmethod p-position (value (list persistent-cons)
                             &key (key #'identity) (test #'p-eql)
                             (start 0) (end nil))
  ;; Move list to start position.
  (loop repeat start
        do (setq list (p-cdr list)))
  ;; The real work.
  (loop for i from start do
        (if (or (p-endp list) (and end (= i end)))
            (return-from p-position nil)
          (let ((elt (funcall key (p-car list))))
            (if (funcall test value elt)
                (return-from p-position i)
              (setq list (p-cdr list))))))
  ;; Return nil if not found.
  nil)


(defmethod p-replace ((vector-1 persistent-array)
                      (vector-2 persistent-array)
                      &key (start1 0) end1 (start2 0) end2)
  ;; We don't need to look at the cached sequence elements,
  ;; so we can just use CL:REPLACE on the vector contents and bypass
  ;; the p-aref calls.
  (replace (contents vector-1) (contents vector-2)
           :start1 start1
           :end1 end1
           :start2 start2
           :end2 end2)
  ;; Touch the vector because it has changed.
  (cache-touch-object vector-1 (cache vector-1))
  vector-1)


(defmethod p-delete-if (test (list persistent-cons)
                        &key (from-end nil) (start 0) end count key)
  ;; DO: Implement FROM-END.
  ;; DO: Write tests.
  (declare (ignore from-end))
  (unless key
    (setq key #'identity))
  ;; Move list to start position.
  (let ((tail list)
        (prev nil))
    (loop repeat start
          do (setq prev tail
                   tail (p-cdr tail)))
    ;; The real work.
    (let ((nr-deleted 0))
      (loop for i from start do
            (if (or (p-endp tail)
                    (and end (= i end))
                    (and count (>= nr-deleted count)))
                (return-from p-delete-if list)
              (if (funcall test (funcall key (p-car tail)))
                  ;; Delete the element.
                  (progn
                    (if prev
                        (setf (p-cdr prev) (p-cdr tail))
                      (setq list (p-cdr tail)))
                    ;; Keep count.
                    (incf nr-deleted))
                ;; Don't delete anything.
                (setq prev tail)))
            ;; Keep moving.
            (setq tail (p-cdr tail)))))
  ;; Return the (possibly modified) list.
  list)


(defmethod p-nreverse ((object (eql nil)))
  nil)

(defmethod p-nreverse ((object persistent-cons))
  (let* ((previous object)
         (current (p-cdr previous)))
    (setf (p-cdr previous) '())
    (loop until (p-endp current)
          do (let ((next (p-cdr current)))
               (setf (p-cdr current) previous
                     previous current
                     current next)))
    previous))

;; DO: Implement P-NREVERSE for persistent vectors.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full fledged persistent objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass persistent-object ()
  ((object-id :initarg :object-id :reader object-id
              :persistence nil :index nil)
   (transaction-id :reader transaction-id :persistence nil :index nil)
   (rucksack :initarg :rucksack :reader rucksack :persistence nil :index nil))
  (:default-initargs
   :rucksack *rucksack*)
  (:metaclass persistent-class)
  (:index nil)
  (:documentation "Classes of metaclass PERSISTENT-CLASS automatically
inherit from this class."))


(defparameter *initializing-instance*
  ;; A hack to paper over some MOP differences.  Maybe a cleaner way
  ;; to solve this would be to write our own method for SHARED-INITIALIZE,
  ;; as suggested by Pascal Costanza.
  ;; See emails of 2006-09-03/04 on rucksack-devel@common-lisp.net.
  nil)

(defmethod initialize-instance :around ((object persistent-object)
                                        &rest args
                                        &key rucksack
                                        ;; The DONT-INDEX argument is used
                                        ;; when creating the indexes themselves
                                        ;; (to prevent infinite recursion).
                                        (dont-index nil)
                                        &allow-other-keys)
  (maybe-update-slot-info (class-of object))
  ;; This happens when persistent-objects are created in memory, not when
  ;; they're loaded from the cache (loading uses ALLOCATE-INSTANCE instead).
  (let ((rucksack (or rucksack (rucksack object))))
    (unless (slot-boundp object 'object-id)
      (setf (slot-value object 'object-id)
	    (cache-create-object object (rucksack-cache rucksack))))
    ;; DO: Explain why we don't set the transaction-id slot here.
    (unless (slot-boundp object 'rucksack)
      (setf (slot-value object 'rucksack) rucksack))
    (unless dont-index
      (rucksack-maybe-index-new-object rucksack (class-of object) object)))
  ;;
  (let (;; Tell (SETF SLOT-VALUE-USING-CLASS), which may be called
        ;; by SHARED-INITIALIZE in some implementations, that we're
        ;; just initializing the instance and it shouldn't try to
        ;; update any indexes.
        (*initializing-instance* t))
    (let ((result (call-next-method)))
      ;; Update slot indexes for persistent slots that are bound now.
      (unless dont-index
        (let ((class (class-of object)))
          (dolist (slot (class-slots class))
            (let ((slot-name (slot-definition-name slot)))
              (when (and (slot-boundp object slot-name)
                         (slot-persistence slot))
                (rucksack-maybe-index-changed-slot (or rucksack (rucksack object))
                                                   class object slot
                                                   nil (slot-value object slot-name)
                                                   nil t))))))
      ;;
      result)))


(defmethod print-object ((object persistent-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "#~D~@[ with transaction id ~D~]"
            (slot-value object 'object-id)
            (transaction-id object))))


;; It's a bit stupid that we have to write the same code for three
;; P-EQL methods, but we don't seem to have much choice.

(defmethod p-eql ((a persistent-object) (b persistent-object))
  (= (object-id a) (object-id b)))

(defmethod p-eql ((a persistent-data) (b persistent-object))
  (= (object-id a) (object-id b)))

(defmethod p-eql ((a persistent-object) (b persistent-data))
  (= (object-id a) (object-id b)))


;;
;;
;;



(defmethod slot-value-using-class :around ((class persistent-class)
                                           object
                                           slot)
  (maybe-update-slot-info class)
  ;; Automatically dereference proxies.
  (maybe-dereference-proxy (call-next-method)))


(defmethod (setf slot-value-using-class) :around (new-value
                                                  (class persistent-class)
                                                  object
                                                  slot-name-or-def)
  (maybe-update-slot-info class)
  ;; If this is a persistent slot, tell the cache that this object
  ;; has changed. The cache will save it when necessary.
  (let ((slot (slot-def-and-name class slot-name-or-def)))
    (if (and (slot-persistence slot)
             ;; If the RUCKSACK slot isn't bound yet, the object is
             ;; just being loaded from disk and we don't need to
             ;; do anything special.
             (slot-boundp object 'rucksack))
        (let* ((old-boundp (slot-boundp-using-class class object slot-name-or-def))
               (old-value
                (and old-boundp
                     (slot-value-using-class class object slot-name-or-def)))
               (result (call-next-method)))
          (cache-touch-object object (cache object))
          ;; Update indexes.
          (unless *initializing-instance*
            (rucksack-maybe-index-changed-slot (rucksack object)
                                               class object slot
                                               old-value new-value
                                               old-boundp t))
          result)
      (call-next-method))))


(defmethod slot-makunbound-using-class :around ((class persistent-class)
                                                object
                                                slot-name-or-def)
  (maybe-update-slot-info class)
  ;; If this is a persistent slot, tell the cache that this object
  ;; has changed. Rely on the cache to save it when necessary.
  (let ((slot (slot-def-and-name class slot-name-or-def)))
    (if (and (slot-persistence slot)
             ;; If the RUCKSACK slot isn't bound yet, the object is
             ;; just being loaded from disk and we don't need to
             ;; do anything special.
             (slot-boundp object 'rucksack))
        (let* ((old-boundp (slot-boundp-using-class class object slot-name-or-def))
               (old-value
                (and old-boundp
                     (slot-value-using-class class object slot-name-or-def)))
               (result (call-next-method)))
          (cache-touch-object object (cache object))
          (rucksack-maybe-index-changed-slot (rucksack object)
                                             class object slot
                                             old-value nil
                                             old-boundp nil)
          result)
      (call-next-method))))


(defun slot-def-and-name (class slot-name-or-def)
  "Returns (1) slot definition and (2) slot name."
  #+lispworks(values (find slot-name-or-def (class-slots class)
                           :key #'slot-definition-name)
                     slot-name-or-def)
  #-lispworks(values slot-name-or-def
                     (slot-definition-name slot-name-or-def)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializing/deserializing cached data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +p-object+ +extension-0+
  "The serialization marker for cached objects.")

(defmethod serialize ((object persistent-object) stream)
  ;; When the serializer meets a persistent object, it only needs to save the
  ;; object id.  The cache will make sure that the object is saved elsewhere.
  (serialize-marker +p-object+ stream)
  (serialize (object-id object) stream))

(defmethod serialize ((object persistent-data) stream)
  ;; When the serializer meets a persistent-data object, it only needs to save
  ;; the object id.  The cache will make sure that the cached object is saved
  ;; elsewhere.
  (serialize-marker +p-object+ stream)
  (serialize (object-id object) stream))

(defmethod serialize ((object proxy) stream)
  ;; Proxies are serialized like the cached objects they stand for.
  (serialize-marker +p-object+ stream)
  (serialize (object-id object) stream))

(defmethod deserialize-contents ((marker (eql +p-object+)) stream)
  ;; Return a proxy.  The proxy contents will be automatically loaded
  ;; when necessary.
  (let ((id (deserialize stream)))
    (make-instance 'proxy
                   :object-id id)))

(defmethod scan-contents ((marker (eql +p-object+))
                          (buffer serialization-buffer)
                          (gc mark-and-sweep-heap))
  ;; Hook into the garbage collector's scanner: when the scanner
  ;; finds a p-object, it adds that object to the gc's roots and
  ;; returns.
  (let ((object-id (deserialize buffer)))
    ;; Add the object to the root set unless it's already marked.
    (unless (object-alive-p (object-table gc) object-id)
      ;; By just pushing the child's id on the root list, we effectively
      ;; get a depth-first traversal.
      (push object-id (roots gc)))))


(defmethod scan-object (object-id (block serialization-buffer) gc)
  (let ((previous-pointer (deserialize block))
        (transaction-id (deserialize block))
        (id (deserialize block))
        (nr-slots (deserialize block))
        (schema-id (deserialize block)))
    ;; DO: Handle previous versions if necessary.
    (declare (ignore schema-id transaction-id previous-pointer)) ; later
    (unless (= id object-id)
      (internal-rucksack-error
       "Object-id mismatch during GC scan (required: ~D; actual: ~D)."
       object-id id))
    (loop repeat nr-slots
          do (scan block gc))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading/updating cached objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod save-object (object object-id (cache standard-cache)
                               transaction-id previous-block
                               &key schema)
  "Serializes the object to a buffer, allocates a heap block of the right
size and writes the buffer to the block.  Returns the (heap position of the)
block containing the object."
  (unless schema
    (setq schema
          (find-or-create-schema-for-object (schema-table cache) object)))
  (let* ((heap (heap cache))
         (schema-id (schema-id schema))
         (nr-slots (nr-persistent-slots schema))
         (buffer (serialization-buffer heap)))
    (reset-buffer buffer)
    ;; Serialize standard fields.
    (serialize-previous-version-pointer previous-block buffer)
    (serialize transaction-id buffer)
    (serialize object-id buffer)
    (serialize nr-slots buffer)
    (serialize schema-id buffer)
    ;; Serialize slot values.
    ;; (Bind *dont-dereference-proxies* to T to make sure that
    ;; the slot-value-using-class method doesn't intercept here.)
    (let ((*dont-dereference-proxies* t))
      (loop for slot-name in (persistent-slot-names schema)
            do (if (slot-boundp object slot-name)
                   (serialize (slot-value object slot-name) buffer)
                 (serialize-marker +unbound-slot+ buffer))))
    ;; Allocate a heap block of the right size.
    (let* ((size (+ (buffer-count buffer)
                    (block-header-size heap)))
           (block (allocate-block heap :size size)))
      ;; Save the serialized buffer in the block.
      (save-buffer buffer (heap-stream heap)
                   :file-position (+ block (block-header-size heap)))
      (handle-written-object object-id block heap)
      ;; Return the block.
      block)))

(defun serialize-previous-version-pointer (previous-pointer stream)
  ;; The previous version pointer is a fixed-width field, so we
  ;; serialize it 'by hand'.
  (if previous-pointer
      (progn
        ;; 1 marker octet plus 6 octets for the 'pointer'.
        (serialize-marker +positive-byte-48+ stream)
        (serialize-byte-48 previous-pointer stream))
    (progn 
      (serialize-marker +nil+ stream)
      ;; Fill up with dummy octets to ensure fixed width.
      (loop repeat 6
            do (serialize-marker +ignore+ stream)))))


;;
;; Loading objects
;;

(defmethod load-object (object-id transaction (cache standard-cache))
  (multiple-value-bind (buffer id nr-slots schema-id most-recent-p)
      (find-committed-object-version object-id
                                     (transaction-id transaction)
                                     (heap cache))
    (declare (ignore id))
    (let* ((table (schema-table cache))
           (schema (find-schema-for-id table schema-id))
           (object (allocate-instance (find-class (schema-class-name schema)))))
      (unless (= nr-slots (nr-persistent-slots schema))
        (internal-rucksack-error
         "Schema inconsistency (expected ~D slots, got ~D slots)."
         (nr-persistent-slots schema)
         nr-slots))
      (let ((added-slots '())
            (discarded-slots '())
            ;; DISCARDED-SLOT-VALUES is a list of discarded slot names and
            ;; their (obsolete) values.
            (discarded-slot-values '()))
        (when (schema-obsolete-p schema)
          (setf added-slots (added-slot-names schema)
                discarded-slots (discarded-slot-names schema)))
        ;; Load and set slot values.
        ;; DO: We should probably initialize the transient slots to their
        ;; initforms here.
        ;; NOTE: The MOP doesn't intercept the (setf slot-value) here,
        ;; because the rucksack and object-id slots are still unbound.
        (loop for slot-name in (persistent-slot-names schema)
              do (let ((marker (read-next-marker buffer))
                       (old-slot-p (member slot-name discarded-slots)))
                   (if (eql marker +unbound-slot+)
                       (unless old-slot-p
                         (slot-makunbound object slot-name))
                     ;; Deserialize the value
                     (let ((value (deserialize-contents marker buffer)))
                       (if old-slot-p
                           (progn 
                             (push value discarded-slot-values)
                             (push slot-name discarded-slot-values))
                         (setf (slot-value object slot-name) value))))))
        ;; Set CACHE, OBJECT-ID and TRANSACTION-ID slots if it's a persistent
        ;; object.
        (when (typep object '(or persistent-object persistent-data))
          (setf (slot-value object 'rucksack) (current-rucksack)
                (slot-value object 'object-id) object-id
                (slot-value object 'transaction-id) (transaction-id transaction)))
        ;; Call UPDATE-PERSISTENT-INSTANCE-FOR-REDEFINED-CLASS if necessary.
        (when (schema-obsolete-p schema)
          (update-persistent-instance-for-redefined-class
           object
           added-slots
           discarded-slots
           discarded-slot-values)))
      ;;
      (values object most-recent-p))))

(defun find-committed-object-version (object-id current-transaction-id heap)
  "Returns the buffer, id, nr-slots and schema-id of the object
containing the compatible version for the given transaction id.  The
buffer points to the first octet after the standard object fields.
As a fifth value, it returns a boolean that's true when the object
version is the most recent committed object version (i.e. in the head
of the object version list)."
  ;; The object table points to a list of object versions (youngest
  ;; transaction first).
  (let ((block (object-heap-position (object-table heap) object-id))
        (most-recent-p t))
    (loop
     (let ((buffer (load-block heap block :skip-header t)))
       (multiple-value-bind (id nr-slots schema-id transaction-id prev-version)
           (load-object-fields buffer object-id)
         (cond ((<= transaction-id current-transaction-id)
                ;; We found the 'compatible' object version: the most recent
                ;; version that's not younger than the current transaction.
                (return (values buffer id nr-slots schema-id most-recent-p)))
               ((null prev-version)
                ;; Oh oh.
                (internal-rucksack-error "Can't find compatible object
version for object #~D and transaction ~D."
                                         object-id current-transaction-id))
               (t
                ;; Keep trying previous versions.
                (setq block prev-version
                      most-recent-p nil))))))))


(defun load-object-fields (buffer object-id)
  "Returns id, nr-slots, schema-id, transaction-id and prev-version
(as 5 values)."
  (let ((prev-version (deserialize buffer))
        (transaction-id (deserialize buffer))
        (id (deserialize buffer))
        (nr-slots (deserialize buffer))
        (schema-id (deserialize buffer)))
    (unless (= id object-id)
      (internal-rucksack-error "Object-id mismatch (required: ~D; actual: ~D)."
                               object-id id))
    (values id nr-slots schema-id transaction-id prev-version)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating persistent instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When a persistent object must be loaded from disk, Rucksack loads the
;; schema nr and finds the corresponding schema.  If the schema is obsolete
;; (i.e. there is a schema for the same class with a higher version number),
;; Rucksack calls the generic function UPDATE-PERSISTENT-INSTANCE-FOR-REDEFINED-CLASS
;; after calling ALLOCATE-INSTANCE for the current class version.  The generic
;; function is very similar to UPDATE-INSTANCE-FOR-REDEFINED-CLASS: it takes a
;; list of added slots, a list of deleted slots and a property list containing
;; the slot names and values for slots that were discarded and had values.

(defgeneric update-persistent-instance-for-redefined-class
    (instance added-slots discarded-slots property-list &key)
  (:method ((instance persistent-object) added-slots discarded-slots plist
            &key)
   ;; Default method: ignore the discarded slots and initialize added slots
   ;; according to their initforms.  We do this 'by hand' and not by calling
   ;; SHARED-INITIALIZE because slot indexes may need to be updated too.
   (let ((slots (class-slots (class-of instance))))
     (loop for slot-name in added-slots
           for slot = (find slot-name slots :key #'slot-definition-name)
           for initfunction = (and slot
                                   (slot-definition-initfunction slot))
           when initfunction
           ;; NOTE: We don't handle initargs, and I think we don't need to.
           ;; We follow the CLHS description of UPDATE-INSTANCE-FOR-REDEFINED-CLASS,
           ;; which says: "When it is called by the system to update an
           ;; instance whose class has been redefined, no initialization
           ;; arguments are provided." 
           do (setf (slot-value instance slot-name) (funcall initfunction))))))


(defmethod update-instance-for-redefined-class
           ((object persistent-object) added-slots discarded-slots plist
            &rest initargs &key)
  ;; This method exists for updating in-memory persistent objects
  ;; of which the class definition has changed.
  (declare (ignore initargs)) ; there shouldn't be any, anyway
  (cache-touch-object object (rucksack-cache (rucksack object)))
  (update-persistent-instance-for-redefined-class object added-slots
                                                  discarded-slots plist))



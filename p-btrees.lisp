;; $Id: p-btrees.lisp,v 1.19 2008/02/19 22:44:06 alemmens Exp $

(in-package :rucksack)

;; DO: We probably need a lock per btree.  Each btree operation should
;; be wrapped in a WITH-LOCK to make sure that nobody else changes the btree
;; halfway during a btree operation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Btrees: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This is a modified version of the in-memory btrees.  We use p-arrays,
p-conses and persistent-objects.

Basically, a B-tree is a balanced multi-way tree.

The reason for using multi-way trees instead of binary trees is that
the nodes are expected to be on disk; it would be inefficient to have
to execute a disk operation for each tree node if it contains only 2
keys.

The key property of B-trees is that each possible search path has the same
length, measured in terms of nodes.
|#

#|
   ;; Btrees
   #:btree
   #:btree-key< #:btree-key<= #:btree-key= #:btree-key>= #:btree-key>
   #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class
   #:btree-nr-keys #:btree-nr-values

   ;; Nodes
   #:bnode

   ;; Functions
   #:btree-search #:btree-insert #:btree-delete #:btree-delete-key
   #:map-btree #:map-btree-keys

   ;; Conditions
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value
|#

(defgeneric btree-nr-keys (btree)
  (:documentation "Returns the number of keys in a btree."))

(defgeneric btree-nr-values (btree)
  (:documentation "Returns the number of values in a btree."))


(defgeneric btree-search (btree key &key errorp default-value)
  (:documentation
   "Returns the value (or persistent list of values, for btrees that
don't have unique keys) associated with KEY.  If the btree has
non-unique keys and no value is found, the empty list is returned.  If
the btree has unique keys and no value is found, the result depends on
the ERRORP option: if ERRORP is true, a btree-search-error is
signalled; otherwise, DEFAULT-VALUE is returned."))

(defgeneric btree-insert (btree key value &key if-exists)
  (:documentation
   "Adds an association from KEY to VALUE to a btree.

IF-EXISTS can be either :OVERWRITE (default) or :ERROR.

If the btree has unique keys (see BTREE-UNIQUE-KEYS-P) and KEY is
already associated with another (according to BTREE-VALUE=) value, the
result depends on the IF-EXISTS option: if IF-EXISTS is :OVERWRITE,
the old value is overwriten; if IF-EXISTS is :ERROR, a
BTREE-KEY-ALREADY-PRESENT-ERROR is signaled.

For btrees with non-unique keys, the IF-EXISTS option is ignored and
VALUE is just added to the list of values associated with KEY (unless
VALUE is already associated with KEY; in that case nothing
happens)."))


(defgeneric btree-delete (btree key value &key if-does-not-exist)
  (:documentation
   "Removes an association from KEY to VALUE from a btree.
IF-DOES-NOT-EXIST can be either :IGNORE (default) or :ERROR.
If there is no association from KEY to VALUE and IF-DOES-NOT-EXIST
is :ERROR, a BTREE-DELETION-ERROR is signaled."))


(defgeneric btree-delete-key (btree key &key if-does-not-exist)
  (:documentation
   "Removes KEY and all associated values from a btree.
IF-DOES-NOT-EXIST can be either :IGNORE (default) or :ERROR.

For a btree with unique-keys that contains a value for KEY, this
operation is identical to

  (btree-delete btree key (btree-search btree key))

For a btree with non-unique keys, it's identical to

  (dolist (value (unwrap-persistent-list (btree-search btree key)))
    (btree-delete btree key value))"))


(defgeneric map-btree (btree function
                             &key min max include-min include-max order)
  (:documentation
   "Calls FUNCTION for all key/value associations in the btree where
key is in the specified interval (this means that FUNCTION can be
called with the same key more than once for btrees with non-unique
keys). FUNCTION must be a binary function; the first argument is the
btree key, the second argument is an associated value.

MIN, MAX, INCLUDE-MIN and INCLUDE-MAX specify the interval.  The
interval is left-open if MIN is nil, right-open if MAX is nil.  The
interval is inclusive on the left if INCLUDE-MIN is true (and
exclusive on the left otherwise).  The interval is inclusive on the
right if INCLUDE-MAX is true (and exclusive on the right otherwise).

ORDER is either :ASCENDING (default) or :DESCENDING."))


(defgeneric map-btree-keys (btree function
                                  &key min max include-min include-max order)
  (:documentation
   "Calls FUNCTION for all keys in the btree where key is in the
specified interval. FUNCTION must be a binary function; the first
argument is the btree key, the second argument is the btree value (or
persistent list of values, for btrees with non-unique keys).  FUNCTION
will be called exactly once for each key in the btree.

MIN, MAX, INCLUDE-MIN and INCLUDE-MAX specify the interval.  The
interval is left-open if MIN is nil, right-open if MAX is nil.  The
interval is inclusive on the left if INCLUDE-MIN is true (and
exclusive on the left otherwise).  The interval is inclusive on the
right if INCLUDE-MAX is true (and exclusive on the right otherwise).

ORDER is either :ASCENDING (default) or :DESCENDING."))


;;
;; Set btrees
;;
;; A 'set btree' is a special kind of btree that's used to implement sets.
;; With set btrees, the 'value' part of a btree binding is irrelevant, because
;; all information is in the keys themselves.
;;

(defgeneric set-btree-insert (set value)
  (:documentation "Add a value to a set-btree.  This will modify the
set-btree."))

(defgeneric set-btree-delete (set value &key if-does-not-exist)
  (:documentation "Removes a value from a set-btree and returns the
modified set-btree.  If the value is not present in the set, this
function signals an error if IF-DOES-NOT-EXIST is :ERROR (if
IF-DOES-NOT-EXIST is :IGNORE, it returns nil)."))

(defgeneric set-btree-search (set value &key errorp default-value)
  (:documentation
   "Returns VALUE if it is present in the btree-set SET.  Otherwise
the result depends on the ERRORP option: if ERRORP is true, a
btree-search-error is signalled; otherwise, DEFAULT-VALUE is
returned."))

(defgeneric map-set-btree (set function)
  (:documentation
   "Calls a unary function for each value in a btree-set."))

(defgeneric set-btree-empty-p (set)
  (:documentation "Returns true iff a btree-set is empty."))

(defgeneric set-count (set)
  (:documentation "Returns the number of values in a btree-set."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition btree-error (error)
  ((btree :initarg :btree :reader btree-error-btree)))

(define-condition btree-search-error (btree-error)
  ((key :initarg :key :reader btree-error-key))
  (:report (lambda (condition stream)
             (format stream "An entry for the key ~S could not be found."
                     (btree-error-key condition)))))


(define-condition btree-insertion-error (btree-error)
  ((key :initarg :key :reader btree-error-key)
   (value :initarg :value :reader btree-error-value)))

(define-condition btree-key-already-present-error (btree-insertion-error)
  ()
  (:report (lambda (condition stream)
             (format stream "There's already another value for the key ~S."
                     (btree-error-key condition)))))

(define-condition btree-type-error (btree-error type-error)
  ())

(define-condition btree-deletion-error (btree-error)
  ((key :initarg :key :reader btree-error-key)
   (value :initarg :value :reader btree-error-value))
  (:report (lambda (condition stream)
             (format stream "Can't delete the association from ~S to ~S
because it doesn't exist."
                     (btree-error-key condition)
                     (btree-error-value condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass btree ()
  ((key<   :initarg :key< :initform '<)
   (value= :initarg :value= :initform 'p-eql
           :documentation "This is only used for btrees with non-unique keys.")
   (key-key :initarg :key-key :reader btree-key-key :initform 'identity
            :documentation "A unary function that is applied to a
btree key before comparing it to another key with a key comparison
predicate like BTREE-KEY<.")
   (value-key :initarg :value-key :reader btree-value-key :initform 'identity
              :documentation "A unary function that is applied to a
btree value before comparing it to another value with the BTREE-VALUE=
predicate.")

   ;;
   (node-class :initarg :node-class
               :reader btree-node-class
               :initform 'bnode)
   (max-node-size :initarg :max-node-size
                  :reader btree-max-node-size
                  :initform 64
                  :documentation "An integer specifying the preferred
maximum number of keys per btree node.")
   (unique-keys-p :initarg :unique-keys-p
                  :reader btree-unique-keys-p
                  :initform t
                  :documentation
                  "If false, one key can correspond to more than one value.
In that case, the values are assumed to be objects for which the function
OBJECT-ID is defined (and returns a unique integer).")
   (key-type :initarg :key-type
             :reader btree-key-type
             :initform t
             :documentation "The type of all keys.")
   (value-type :initarg :value-type
               :reader btree-value-type
               :initform t
               :documentation "The type of all values.")
   (root :accessor btree-root))
  (:metaclass persistent-class))


(defmethod initialize-instance :around ((btree btree)
                                        &rest initargs
                                        &key key< key-key value= value-key
                                        &allow-other-keys)
  ;; It must be possible to save these btrees in the cache, but
  ;; that will not work for function objects because they can't be
  ;; serialized. This means that you should only specify symbols that
  ;; name a function.  For program-independent databases you should
  ;; only use symbols from the COMMON-LISP or RUCKSACK packages.
  (declare (ignore initargs))
  (if (and (symbolp key<) (symbolp value=)
           (symbolp key-key) (symbolp value-key))
    (call-next-method)
    (error "The :key<, :key-key, :value= and :value-key initargs for
persistent btrees must be symbols naming a function, otherwise they
can't be saved on disk.")))

;;
;; Comparison functions that can be deduced from KEY< (because the
;; btree keys have a total order).
;;

(defmethod btree-key< ((btree btree))
  (let ((key< (slot-value btree 'key<))
        (key-key (btree-key-key btree)))
    (lambda (key1 key2)
      (and (not (eql key1 'key-irrelevant))
           (not (eql key2 'key-irrelevant))
           (funcall key<
                    (funcall key-key key1)
                    (funcall key-key key2))))))

(defmethod btree-key= ((btree btree))
  (let ((key< (slot-value btree 'key<))
        (key-key (btree-key-key btree)))
    (lambda (key1 key2)
      (and (not (eql key1 'key-irrelevant))
           (not (eql key2 'key-irrelevant))
           (let ((key1 (funcall key-key key1))
                 (key2 (funcall key-key key2)))
             (and (not (funcall key< key1 key2))
                  (not (funcall key< key2 key1))))))))

(defmethod btree-key>= ((btree btree))
  (lambda (key1 key2)
    (not (funcall (btree-key< btree) key1 key2))))

(defmethod btree-key<= ((btree btree))
  (let ((key< (slot-value btree 'key<))
        (key-key (btree-key-key btree)))
    (lambda (key1 key2)
      (let ((key1 (funcall key-key key1))
            (key2 (funcall key-key key2)))
        (or (funcall key< key1 key2)
            (not (funcall key< key2 key1)))))))

(defmethod btree-key> ((btree btree))
  (let ((key< (slot-value btree 'key<))
        (key-key (btree-key-key btree)))
    (lambda (key1 key2)
      (let ((key1 (funcall key-key key1))
            (key2 (funcall key-key key2)))
        (and (not (funcall key< key1 key2))
             (funcall key< key2 key1))))))


(defmethod btree-value= ((btree btree))
  (let ((value= (slot-value btree 'value=))
        (value-key (btree-value-key btree)))
    (lambda (value1 value2)
      (let ((value1 (funcall value-key value1))
            (value2 (funcall value-key value2)))
        (funcall value= value1 value2)))))

  
;;
;; Btree nodes (= 'bnodes').
;;

(defclass bnode ()
  ((bindings :initarg :bindings
             :initform '()
             :accessor bnode-bindings
             :documentation "A vector of with alternating keys and
values.  The keys are sorted by KEY<. No two keys can be the same.
For leaf nodes of btrees with non-unique-keys, the value part is
actually a list of values.  For intermediate nodes, the value is a
child node.  All keys in the child node will be KEY<= the child node's
key in the parent node.")
   (nr-bindings :initform 0
                :accessor bnode-nr-bindings
                :documentation "The number of key/value bindings in
the index vector.")
   (leaf-p :initarg :leaf-p :initform nil :reader bnode-leaf-p))
  (:metaclass persistent-class))


(defmethod initialize-instance :after ((node bnode)
                                       &key btree &allow-other-keys)
  (setf (bnode-bindings node) (p-make-array (* 2 (btree-max-node-size btree))
                                            :initial-element nil)
        (bnode-nr-bindings node) 0))


(defmethod print-object ((node bnode) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "with ~D bindings" (bnode-nr-bindings node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set btrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass set-btree (btree)
  ()
  (:default-initargs
   ;; We use a special bnode class because we don't care about the binding
   ;; values (so we can optimize them away later).
   :node-class 'set-bnode
   ;; We use small nodes, because we expect relatively many sets
   ;; with only a few elements.
   :max-node-size 8
   ;; The keys of a set-btree are unique (otherwise it wouldn't be a set
   ;; but a bag).
   :unique-keys-p t)
  (:metaclass persistent-class)
  (:documentation "A persistent set of objects, implemented as a btree."))

(defclass set-bnode (bnode)
  ()
  (:metaclass persistent-class)
  (:documentation "A special kind of btree node, used to implement set btrees."))


;; Sets of persistent objects are implemented as set-btrees.  They're
;; used to represent the values of a btree that maps slot values to
;; one or more persistent objects (i.e. they're used for non-unique
;; slot indexes). They can also be used separately.

(defclass persistent-object-set (set-btree)
  ()
  (:default-initargs
   ;; For sets of persistent-objects we store the objects as keys,
   ;; but we use the object-ids to compare keys.
   :key-key 'object-id)
  (:metaclass persistent-class)
  (:documentation "A persistent set of persistent-objects, implemented
as a btree."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some info functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Counting keys or values
;;

(defmethod btree-nr-keys ((btree btree))
  (if (slot-boundp btree 'root)
      (bnode-nr-keys (btree-root btree))
    0))

(defmethod bnode-nr-keys ((node bnode))
  (if (bnode-leaf-p node)
      (bnode-nr-bindings node)
    (loop for i below (bnode-nr-bindings node)
          sum (bnode-nr-keys (binding-value (node-binding node i))))))


(defmethod btree-nr-values ((btree btree))
  (if (btree-unique-keys-p btree)
      (btree-nr-keys btree)
    (let ((result 0))
      (map-btree-keys btree
                      (lambda (key set)
                        (declare (ignore key))
                        (incf result
                              (etypecase set
                                (persistent-object-set (set-count set))
                                (persistent-cons (p-length set))
                                (null 0)))))
      result)))

;;
;; Depth and balance
;;

(defmethod node-max-depth ((node bnode))
  (if (bnode-leaf-p node)
      0
    (loop for i below (bnode-nr-bindings node)
          for binding = (node-binding node i)
          maximize (1+ (node-max-depth (binding-value binding))))))

(defmethod node-min-depth ((node bnode))
  (if (bnode-leaf-p node)
      0
    (loop for i below (bnode-nr-bindings node)
          for binding = (node-binding node i)
          minimize (1+ (node-min-depth (binding-value binding))))))

(defmethod btree-depths ((btree btree))
  (if (slot-value btree 'root)
      (values (node-min-depth (btree-root btree))
              (node-max-depth (btree-root btree)))
    (values 0 0)))

(defmethod btree-balanced-p ((btree btree))
  (multiple-value-bind (min max)
      (btree-depths btree)
    (<= (- max min) 1)))


;;
;; Debugging
;;

(defun display-node (node)
  (pprint (node-as-cons node)))

(defun node-as-cons (node &optional (unique-keys t))
  (loop with leaf-p = (bnode-leaf-p node)
        for i below (bnode-nr-bindings node)
        for value = (node-binding-value node i)
        collect (list (node-binding-key node i)
                      (if leaf-p
                          (if unique-keys
                              value
                            (unwrap-persistent-list value))
                        (node-as-cons value)))))

(defun btree-as-cons (btree)
  (and (slot-value btree 'root)
       (node-as-cons (btree-root btree) (btree-unique-keys-p btree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct binding
  key
  value)

(defun node-binding (node i)
  ;; A binding used to be a persistent cons, but we want to reduce
  ;; persistent consing so now we use a small struct and try to
  ;; make sure that we persist the relevant info when necessary.
  (let ((vector (bnode-bindings node)))
    (make-binding :key (p-aref vector (* 2 i))
                  :value (p-aref vector (1+ (* 2 i))))))
                          
(defun node-binding-key (node i)
  (p-aref (bnode-bindings node) (* 2 i)))

(defun node-binding-value (node i)
  (p-aref (bnode-bindings node) (1+ (* 2 i))))

(defun (setf node-binding) (binding node i)
  (update-node-binding node i
                       (binding-key binding)
                       (binding-value binding))
  binding)

(defun update-node-binding (node i key value)
  (setf (node-binding-key node i) key
        (node-binding-value node i) value))

(defun (setf node-binding-key) (key node i)
  (setf (p-aref (bnode-bindings node) (* 2 i))
        key))

(defun (setf node-binding-value) (value node i)
  (setf (p-aref (bnode-bindings node) (1+ (* 2 i)))
        value))

;;

(defun make-leaf-value (btree value)
  (if (btree-unique-keys-p btree)
      value
    (p-cons value '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set btrees and persistent object sets: implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-btree-insert ((set set-btree) value)
  (btree-insert set value nil :if-exists :overwrite))

(defmethod set-btree-delete ((set set-btree) value &key (if-does-not-exist nil))
  (btree-delete-key set value :if-does-not-exist if-does-not-exist))

(defmethod set-btree-search ((set set-btree) value &key errorp default-value)
  (btree-search set value
                :errorp errorp
                :default-value default-value))

(defmethod map-set-btree ((set set-btree) function)
  (map-btree-keys set
                  (lambda (key value)
                    (declare (ignore value))
                    (funcall function key))))

(defmethod set-btree-empty-p ((set set-btree))
  (or (not (slot-boundp set 'root))
      (let ((root (slot-value set 'root)))
        (and (bnode-leaf-p root)
             (= 0 (bnode-nr-bindings root))))))

(defmethod set-count ((set set-btree))
  (btree-nr-values set))

;; DO: Change the binding functions for SET-BTREES to optimize the values
;; away.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod btree-search (btree key &key (errorp t) (default-value nil))
  (restart-case 
      (if (slot-boundp btree 'root)
          (node-search btree (btree-root btree) key errorp default-value)
        (not-found btree key errorp default-value))
    (use-value (value)
      :report (lambda (stream)
                (format stream "Specify a value to use this time for key ~S." key))
      :interactive (lambda ()
                     (format t "Enter a value for key ~S: " key)
                     (multiple-value-list (eval (read))))
      value)
    (store-value (value)
      :report (lambda (stream)
                (format stream "Specify a value to set key ~S to." key))
      :interactive (lambda ()
                     (format t "Enter a value for key ~S: " key)
                     (multiple-value-list (eval (read))))
      (btree-insert btree key value))))


(defun not-found (btree key errorp default-value)
  (if (btree-unique-keys-p btree)
      (if errorp
          (error 'btree-search-error :btree btree :key key)
        default-value)
    '()))

;;
;; Node-search
;;

(defgeneric node-search (btree node key errorp default-value)
  (:method ((btree btree) (node bnode) key errorp default-value)
   (let ((binding (node-search-binding btree node key)))
     (if binding
         (binding-value binding)
       (not-found btree key errorp default-value)))))
  
(defgeneric node-search-binding (btree node key)
  (:documentation "Tries to find KEY in NODE or one of its subnodes.
Returns three values if the key was found: the binding, the node
containing the binding and the position of the binding in that node.
Returns nil otherwise.")
  (:method ((btree btree) (node bnode) key)
   (if (bnode-leaf-p node)
       (multiple-value-bind (binding position)
           (find-key-in-node btree node key)
         (and binding
              (values binding node position)))
     (let ((subnode (find-subnode btree node key)))
       (node-search-binding btree subnode key)))))

(defun find-subnode (btree node key)
  "Returns the subnode that contains more information for the given key."
  ;; Find the first binding with a key >= the given key and return
  ;; the corresponding subnode.
  (let ((btree-key< (btree-key< btree))
        (last (1- (bnode-nr-bindings node))))
    (labels ((binary-search (start end)
               (let ((mid (+ start (ash (- end start) -1))))
                 (if (= start mid)
                     (if (funcall btree-key< (node-binding-key node start) key)
                         (node-binding-value node end)
                       (node-binding-value node start))
                   (if (funcall btree-key< (node-binding-key node mid) key)
                       (binary-search mid end)
                     (binary-search start mid))))))
      (if (funcall btree-key< (node-binding-key node (1- last)) key)
          (node-binding-value node last)
          (binary-search 0 last)))))

(defun find-key-in-node (btree node key)
  "Tries to find a binding with the given key in a bnode.  If it
succeeds, it returns the binding (and, as a second value, the position
of that binding).  Otherwise it returns NIL."
  (let ((btree-key< (btree-key< btree))
        (index-count (bnode-nr-bindings node)))
    (labels ((binary-search (start end)
               (let ((mid (+ start (ash (- end start) -1))))
                 (if (= start mid)
                     (let ((start-binding (node-binding node start)))
                       (if (funcall btree-key< (node-binding-key node start) key)
                           (when (< end index-count)
                             (values (node-binding node end) end))
                         (values start-binding start)))
                   (if (funcall btree-key< (node-binding-key node mid) key)
                       (binary-search mid end)
                     (binary-search start mid))))))
      (when (plusp index-count)
        (multiple-value-bind (candidate position)
            (binary-search 0 index-count)
          (when (and candidate
                     (funcall (btree-key= btree) (binding-key candidate) key))
            (values candidate position)))))))

(defun key-position (btree node key)
  "Tries to find a binding with the given key in a bnode.  If it
succeeds, it returns the position of that binding.  Otherwise, it
returns NIL."
  (nth-value 1 (find-key-in-node btree node key)))


(defun find-value-in-node (btree node value &key (test (btree-value= btree)))
  "Tries to find a binding with the given value in a bnode.  If it
succeeds, it returns the binding (and, as a second value, the position
of that binding).  Otherwise it returns NIL."
  ;; The bindings aren't sorted by value, so we have to do
  ;; a plain linear search.
  (loop for i below (bnode-nr-bindings node)
        when (funcall test (node-binding-value node i) value)
        do (return-from find-value-in-node
             (values (node-binding node i) i)))
  ;; Not found: return nil.
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-btree (btree)
  ;; Check that it is completely sorted.
  (let (prev-key)
    (map-btree-keys btree
                    (lambda (key value)
                      (declare (ignore value))
                      (when prev-key
                        (unless (funcall (btree-key< btree) prev-key key)
                          (pprint (btree-as-cons btree))
                          (error "Btree inconsistency between ~D and ~D" prev-key key)))
                      (setq prev-key key))))
  ;; Check that it is balanced
  (unless (btree-balanced-p btree)
    (error "Btree ~S is not balanced." btree)))

(defun check-bnode-keys (tree node)
  "Check a btree node (and its descendants) for consistency.  This is only used
for debugging."
  (car
   (last
    (loop with leaf-p = (bnode-leaf-p node)
          for i below (bnode-nr-bindings node)
          for binding = (node-binding node i)
          collect
          (if leaf-p
              (binding-key binding)
            (progn
              (let ((x (check-bnode-keys tree
                                         (binding-value binding))))
                (when x
                  (unless (or (eq x 'key-irrelevant)
                              (eq (binding-key binding) 'key-irrelevant))
                    (unless (funcall (btree-key= tree)
                                     (funcall (btree-key-key tree)
                                              (binding-key binding))
                                     (funcall (btree-key-key tree)
                                              x))
                      (print "Found error")
                      (describe (binding-key binding))
                      (describe x)
                      (pprint (btree-as-cons tree))
                      (error "Inconsistent bnode key at ~a binding ~a binding-key ~a val ~a X ~a"
                             node binding (binding-key binding)
                             (binding-value binding) x)))))
              (binding-key binding)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod btree-insert ((btree btree) key value &key (if-exists :overwrite))
  ;; Check that key and value are of the right type.
  (unless (typep key (btree-key-type btree))
    (error 'btree-type-error
           :btree btree
           :datum key
           :expected-type (btree-key-type btree)))
  (unless (typep value (btree-value-type btree))
    (error 'btree-type-error
           :btree btree
           :datum value
           :expected-type (btree-value-type btree)))
  ;; Do the real work.
  (if (slot-boundp btree 'root)
      (bnode-insert btree (btree-root btree) (list nil) key value if-exists)
    ;; Create a root.
    (let ((leaf (make-instance (btree-node-class btree)
                               :btree btree
                               :leaf-p t)))
      (insert-new-binding leaf 0 key (make-leaf-value btree value))
      (let* ((empty-leaf (make-instance (btree-node-class btree)
                                        :btree btree
                                        :leaf-p t))
             (root (make-root btree key leaf 'key-irrelevant empty-leaf)))
        (setf (btree-root btree) root))))
  ;; Return the inserted value.
  value)


(defun make-root (btree left-key left-subnode right-key right-subnode)
  (let ((root (make-instance (btree-node-class btree) :btree btree)))
    (update-node-binding root 0 left-key left-subnode)
    (update-node-binding root 1 right-key right-subnode)
    (setf (bnode-nr-bindings root) 2)
    root))


;;
;; Node insert
;;

(defgeneric bnode-insert (btree node parent-stack key value if-exists))

(defmethod bnode-insert ((btree btree) (node bnode)
                              parent-stack key value if-exists)
  (if (bnode-leaf-p node)
      (leaf-insert btree node parent-stack key value if-exists)
    (let ((subnode (find-subnode btree node key)))
      (bnode-insert btree subnode (cons node parent-stack)
                         key value if-exists))))

(defun smallest-key (node)
  "Returns the smallest key in this node or any of its subnodes."
  ;; Walk recursively along the 'left edge' of the tree to find
  ;; the smallest key.
  (if (bnode-leaf-p node)
      (node-binding-key node 0)
    (smallest-key (node-binding-value node 0))))

(defun biggest-key (node)
  "Returns the biggest key in this node or any of its subnodes."
  ;; Walk recursively along the 'right edge' of the tree to find
  ;; the biggest key.
  (let ((end (1- (bnode-nr-bindings node))))
    (if (bnode-leaf-p node)
        (node-binding-key node end)
      (biggest-key (node-binding-value node end)))))


(defun split-bnode (btree node parent-stack key)
  "The node is (almost) full. Create two new nodes and divide the
current node-index over these two new nodes."
  (let* ((split-pos (floor (bnode-nr-bindings node) 2))
         (left (make-instance (btree-node-class btree)
                              :btree btree
                              :leaf-p (bnode-leaf-p node)))
         (right (make-instance (btree-node-class btree)
                               :btree btree
                               :leaf-p (bnode-leaf-p node))))
    ;; Divide the node over the two new nodes.
    (replace-bindings (bnode-bindings left) (bnode-bindings node)
                      :end2 split-pos)
    (replace-bindings (bnode-bindings right) (bnode-bindings node)
                      :start2 split-pos)
    (setf (bnode-nr-bindings left) split-pos
          (bnode-nr-bindings right) (- (bnode-nr-bindings node) split-pos))
    ;;
    (let ((left-key
           ;; The key that splits the two new nodes.
           (biggest-key left)))
      (cond ((p-eql node (btree-root btree))
             ;; Make a new root.
             (setf (btree-root btree)
                   (make-root btree left-key left 'key-irrelevant right)))
            (t
             (let* ((parent (first parent-stack))
                    (node-pos (node-position node parent btree))
                    (parent-binding (node-binding parent node-pos))
                    (old-key (binding-key parent-binding)))
               (when (node-full-p btree parent)
                 (multiple-value-bind (parent1 parent2)
                     (split-bnode btree parent (rest parent-stack) old-key)
                   (setq node-pos (node-position node parent1 btree)
                         parent parent1)
                   (when (null node-pos)
                     (setq node-pos (node-position node parent2 btree)
                           parent parent2))
                   (assert (not (null node-pos)))
                   (setq parent-binding (node-binding parent node-pos)
                         old-key (binding-key parent-binding))))
               ;; Replace the original subnode by the left-child and
               ;; add a new-binding with new-key & right-child.
               (update-node-binding parent node-pos left-key left)
               ;; Insert a new binding for the right node.
               (insert-new-binding parent (1+ node-pos) old-key right))))
      ;; Return the node that's relevant for KEY.
      ;; (And return the other node as a second value; it may be
      ;; needed when recursively splitting parent nodes.)
      (if (or (eq key 'key-irrelevant)
              (funcall (btree-key< btree) left-key key))
          (values right left)
        (values left right)))))


(defun node-position (node parent btree)
  "Returns the position of NODE (as a binding value) in a parent
node."
  (nth-value 1 (find-value-in-node btree parent node :test 'p-eql)))


(defun insert-new-binding (node position key value)
  ;; This function must only be called if we know that the bindings
  ;; vector isn't full already.
  (unless (>= position (bnode-nr-bindings node))
    ;; Make room by moving bindings to the right.
    (let ((bindings (bnode-bindings node))
          (length (bnode-nr-bindings node)))
      (replace-bindings bindings bindings
                        :start1 (1+ position) :end1 (1+ length)
                        :start2 position :end2 length)))
  ;; Insert new binding.
  (update-node-binding node position key value)
  (incf (bnode-nr-bindings node)))

(defun replace-bindings (vector-1 vector-2 &key (start1 0) end1 (start2 0) end2)
  "Like P-REPLACE, but for vectors with bindings instead of
plain vectors (so all indexes must be multiplied by 2)."
  (p-replace vector-1 vector-2
             :start1 (* 2 start1)
             :end1 (and end1 (* 2 end1))
             :start2 (* 2 start2)
             :end2 (and end2 (* 2 end2))))


;;
;; Debugging
;;

(defun check-node (btree node)
  (loop for i below (1- (bnode-nr-bindings node))
        for left-key = (node-binding-key node i)
        for right-key = (node-binding-key node (1+ i))
        do (unless (or (eql right-key 'key-irrelevant)
                       (funcall (btree-key< btree) left-key right-key))
             (display-node node)
             (error "Inconsistent node ~S" node))))


;;
;; Leaf insert
;;

(defun leaf-insert (btree leaf parent-stack key value if-exists)
  (multiple-value-bind (binding position)
      (find-key-in-node btree leaf key)
    (if binding
        ;; Key already exists.
        (if (btree-unique-keys-p btree)
            (ecase if-exists
              (:overwrite
               (setf (node-binding-value leaf position) value))
              (:error
               ;; Signal an error unless the old value happens to be
               ;; the same as the new value.
               (unless (funcall (btree-value= btree) (binding-value binding) value)
                 (error 'btree-key-already-present-error
                        :btree btree
                        :key key
                        :value value))))
          ;; For non-unique keys, we ignore the :IF-EXISTS option and
          ;; just add value to the set of values (unless value is already
          ;; there).
          (let ((set (node-binding-value leaf position)))
            (etypecase set
              (persistent-object-set
               (set-btree-insert set value))
              (persistent-cons
               (if (eql (btree-value-type btree) 'persistent-object)
                   ;; The values are persistent objects, so we know we
                   ;; can put them in a persistent-object-set.  Let's
                   ;; do that, now we know that there are at least two
                   ;; objects in the set.
                   (let ((new-set (make-instance 'persistent-object-set)))
                     (set-btree-insert new-set (p-car set))
                     (set-btree-insert new-set value)
                     (setf (node-binding-value leaf position) new-set))
                 ;; We don't know anything about the values, so we have to
                 ;; resort to a persistent list to store the values.  This
                 ;; will lead to bad performance if few keys map to many
                 ;; values, but we don't have much choice.
                 ;; DO: Use set-btrees for other types for which we can come
                 ;; up with some kind of ordering (like strings, numbers,
                 ;; etcetera).
                 (unless (p-find value set :test (btree-value= btree))
                   (setf (node-binding-value leaf position)
                         (p-cons value (node-binding-value leaf position)))))))))
       ;; The key doesn't exist yet. Create a new binding and add it to the
       ;; leaf index in the right position.
       (progn
         (when (node-full-p btree leaf)
           (setq leaf (split-bnode btree leaf parent-stack key)))
         (let ((new-position (position-of-binding-with-greater-key btree leaf key)))
           (insert-new-binding leaf
                               (or new-position (bnode-nr-bindings leaf))
                               key
                               (make-leaf-value btree value)))))))

(defun position-of-binding-with-greater-key (btree node key)
  "Returns the position of the first binding in NODE with a key
greater than KEY.  Returns nil if there is no such binding."
  (loop for position below (bnode-nr-bindings node)
        when (funcall (btree-key< btree) key (node-binding-key node position))
        do (return-from position-of-binding-with-greater-key position))
  ;; No such binding: return nil.
  nil)

  
(defun node-full-p (btree node)
  (>= (bnode-nr-bindings node) (btree-max-node-size btree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod btree-delete ((btree btree) key value
                         &key (if-does-not-exist :ignore))
  (flet ((forget-it ()
           ;; Signal an error or return quietly.
           (ecase if-does-not-exist
             (:ignore (return-from btree-delete))
             (:error (error 'btree-deletion-error
                            :btree btree
                            :key key
                            :value value)))))
    (multiple-value-bind (binding node position)
        (and (slot-boundp btree 'root)
             (node-search-binding btree (btree-root btree) key))
      (cond ((not binding)
             ;; The binding doesn't exist: forget it.
             (forget-it))
            ((btree-unique-keys-p btree)
             (if (funcall (btree-value= btree) value (binding-value binding))
                 ;; The binding exists and it has the right value: let
                 ;; BTREE-DELETE-KEY do the real work.
                 (btree-delete-key btree key)
               ;; The binding exists but it has the wrong value: forget it.
               (forget-it)))
            (t
             ;; For non-unique keys, we ignore the :IF-EXISTS option and
             ;; just delete the value from the list of values (unless it's
             ;; not there).
             (flet ((check (x) (funcall (btree-value= btree) x value)))
               (let ((set (binding-value binding)))
                 (etypecase set
                   (persistent-object-set
                    (set-btree-delete set value :if-does-not-exist :ignore)
                    (when (set-btree-empty-p set)
                      ;; This was the last value in the set: remove the key.
                      (btree-delete-key btree key)))
                   ((or null persistent-cons)
                    (if (p-find value set :test (btree-value= btree))
                        (if (null (p-cdr set))
                            ;; This is the last value in the list: remove the
                            ;; key.
                            (btree-delete-key btree key)
                          ;; There's more than one value in the list: delete the
                          ;; value that must be deleted and keep the other values.
                          (setf (node-binding-value node position)
                                (p-delete-if #'check (binding-value binding)
                                             :count 1)))
                      ;; The value is not in the list: forget it.
                      (forget-it)))))))))))



 
(defmethod btree-delete-key ((btree btree) key &key (if-does-not-exist :ignore))
  (if (slot-boundp btree 'root)
      (bnode-delete-key btree (btree-root btree) (list nil) key if-does-not-exist)
    (ecase if-does-not-exist
      (:ignore)
      (:error (error 'btree-search-error :btree btree :key key)))))

(defgeneric bnode-delete-key (btree node parent-stack key if-does-not-exist))

(defmethod bnode-delete-key ((btree btree) (node bnode)
                                  parent-stack key if-does-not-exist)
  (if (bnode-leaf-p node)
      (leaf-delete-key btree node parent-stack key if-does-not-exist)
    (let ((subnode (find-subnode btree node key)))
      (bnode-delete-key btree subnode (cons node parent-stack)
                             key if-does-not-exist))))
 
(defun leaf-delete-key (btree leaf parent-stack key if-does-not-exist)
  (let ((binding (find-key-in-node btree leaf key)))
    (unless binding
      (ecase if-does-not-exist
        (:ignore (return-from leaf-delete-key))
        (:error (error 'btree-search-error :btree btree :key key))))

    (let* ((position (key-position btree leaf key))
           (length (bnode-nr-bindings leaf))
           (was-biggest-key-p (= position (1- length))))
      
      (remove-key btree leaf (binding-key binding))
      
      (when was-biggest-key-p
        ;; Parent nodes always keep track of the biggest key in
        ;; their child nodes.  So if we just deleted the biggest
        ;; key from this leaf, the parent node needs to be updated
        ;; with the key that is now the biggest of this leaf.
        (unless (= 0 (bnode-nr-bindings leaf))
          (let ((biggest-key (biggest-key leaf)))
            (update-parents-for-deleted-key btree parent-stack key biggest-key))))
      
      (unless (node-full-enough-p btree leaf)
        (enlarge-node btree leaf parent-stack)))))


(defun enlarge-node (btree node parent-stack)
  ;; NODE is not full enough (less than half full), so we redistribute
  ;; elements over NODE and one of its siblings.  (Unless both sibling
  ;; are only half full; in that case we merge some nodes.)
  (let ((parent (first parent-stack)))
    ;; Don't enlarge root node.
    (when (null parent)
      (return-from enlarge-node))
    (let ((node-pos (node-position node parent btree))
          left-sibling)
      (when (plusp node-pos)
        ;; There is a left sibling.
        (setq left-sibling (node-binding-value parent (1- node-pos)))
        (unless (node-has-min-size-p btree left-sibling)
          (distribute-elements btree left-sibling node parent)
          (return-from enlarge-node)))
      (when (< (1+ node-pos) (bnode-nr-bindings parent))
        ;; There is a right sibling.
        (let ((right-sibling (node-binding-value parent (1+ node-pos))))
          (if (node-has-min-size-p btree right-sibling)
              (join-nodes btree node right-sibling parent-stack)
            (distribute-elements btree node right-sibling parent))
          (return-from enlarge-node)))
      (when left-sibling
        (join-nodes btree left-sibling node parent-stack)
        (return-from enlarge-node))))
  (error "This should not happen."))


(defun update-parents-for-deleted-key (btree parent-stack old-key new-key)
  (when parent-stack
    (let ((node (first parent-stack)))
      (when node
        (let ((position (key-position btree node old-key)))
          (when position
            (setf (node-binding-key node position) new-key)
            (update-parents-for-deleted-key btree (rest parent-stack) old-key new-key)))))))

 
;; The idea is that DISTRIBUTE-ELEMENTS will only be called if the
;; union of the two nodes has enough elements for two nodes that are
;; 'full enough'.  JOIN-NODES, OTOH, makes one node out of two,
;; deletes one key in the parent, and finally checks the parent to see
;; if it has to be enlarged as well.

(defun distribute-elements (btree left-node right-node parent)
  "One of LEFT-NODE and RIGHT-NODE doesn't have enough elements, but
the union of both has enough elements for two nodes, so we
redistribute the elements between the two nodes."
  (let* ((left-bindings (bnode-bindings left-node))
         (left-length (bnode-nr-bindings left-node))
         (right-bindings (bnode-bindings right-node))
         (right-length (bnode-nr-bindings right-node))
         (sum (+ left-length right-length))
         (median (floor sum 2)))
    ;; LEFT-NODE will have MEDIAN elements, RIGHT-NODE will have
    ;; (- SUM MEDIAN) elements.
    (cond ((< left-length median)
           ;; Case 1: move some elements to the left.
           (replace-bindings left-bindings right-bindings
                             :start1 left-length
                             :start2 0 :end2 (- median left-length))
           (replace-bindings right-bindings right-bindings
                             :start1 0
                             :start2 (- median left-length) :end2 right-length))
          ((> left-length median)
           ;; Case 2: move some elements to the right.
           (replace-bindings right-bindings right-bindings
                             :start1 (- left-length median)
                             :start2 0 :end2 right-length)
           (replace-bindings right-bindings left-bindings
                             :start1 0
                             :start2 median :end2 left-length)))
    ;; Set new lengths for both nodes.
    (shorten left-node median)
    (shorten right-node (- sum median))
    ;; Select new separator key.
    (setf (node-binding-key parent (node-position left-node parent btree))
          (biggest-key left-node))))



(defun join-nodes (btree left-node right-node parent-stack)
  "Create one node which contains the elements of both LEFT-NODE and
RIGHT-NODE."
  (let* ((parent (first parent-stack))
         (left-length (bnode-nr-bindings left-node))
         (right-length (bnode-nr-bindings right-node))
         (left-position (node-position left-node parent btree)))
    ;; Move all elements into LEFT-NODE.
    (replace-bindings (bnode-bindings left-node)
                      (bnode-bindings right-node)
                      :start1 left-length
                      :start2 0 :end2 right-length)
    ;; Make binding which pointed to RIGHT-NODE point to LEFT-NODE.
    (setf (node-binding-value parent (1+ left-position)) left-node)
    ;; Remove key which pointed to LEFT-NODE.
    (remove-key btree parent (node-binding-key parent left-position))
    ;; Set new length of LEFT-NODE.
    (setf (bnode-nr-bindings left-node) (+ right-length left-length))
    ;; Check if we have to enlarge the parent as well because we
    ;; removed one key.
    (unless (node-full-enough-p btree parent)
      (enlarge-node btree parent (rest parent-stack)))
    ;; If the parent node is the root node and it has only 1 child,
    ;; make that child the root.
    (when (and (p-eql parent (btree-root btree))
               (= 1 (bnode-nr-bindings parent)))
      (setf (btree-root btree)
            (node-binding-value parent 0)))))


(defun remove-key (btree node key)
  (let ((position (key-position btree node key))
        (length (bnode-nr-bindings node)))
    (unless (>= position (1- length))
      ;; Move bindings to the left.
      (let ((bindings (bnode-bindings node)))
        (replace-bindings bindings bindings
                          :start1 position :end1 (1- length)
                          :start2 (1+ position) :end2 length)))
    (shorten node (1- length))))

(defun shorten (node new-length)
  ;; Set length of NODE to NEW-LENGTH, set bindings behind NEW-LENGTH
  ;; to NIL, so the GC can throw them away.
  (loop for i from new-length below (bnode-nr-bindings node)
        do (update-node-binding node i nil nil))
  (setf (bnode-nr-bindings node) new-length))
    


(defun node-full-enough-p (btree node)
  (>= (bnode-nr-bindings node)
      (floor (btree-max-node-size btree) 2)))

(defun node-has-min-size-p (btree node)
  (<= (bnode-nr-bindings node)
      (floor (btree-max-node-size btree) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod map-btree ((btree btree) function
                      &key min max include-min include-max (order :ascending))
  (let ((fn (if (btree-unique-keys-p btree)
                function
              (lambda (key set)
                ;; Call FUNCTION once for each value associated with KEY.
                (etypecase set
                  (persistent-object-set
                   (map-set-btree set
                                  (lambda (value) (funcall function key value))))
                  (persistent-cons
                   (p-mapc (lambda (value) (funcall function key value))
                           set))
                  (null 'ignore))))))
    (map-btree-keys btree fn
                    :min min
                    :max max
                    :include-min include-min
                    :include-max include-max
                    :order order)))


(defmethod map-btree-keys ((btree btree) function
                           &key min max include-min include-max (order :ascending))
  (when (slot-boundp btree 'root)
    (map-btree-keys-for-node btree (slot-value btree 'root) function
                             min max include-min include-max order)))

(defgeneric map-btree-keys-for-node (btree node function
                                     min max include-min include-max
                                     order)
  (:method ((btree btree) (node bnode) function
            min max include-min include-max
            order)
     (if (bnode-leaf-p node)
         ;; Leaf node.
         (let ((too-small-p
                (if min
                    (if include-min
                        (lambda (key) (funcall (btree-key< btree) key min))
                      (lambda (key) (funcall (btree-key<= btree) key min)))
                  (constantly nil)))
               (too-big-p
                (if max
                    (if include-max
                        (lambda (key) (funcall (btree-key> btree) key max))
                      (lambda (key) (funcall (btree-key>= btree) key max)))
                  (constantly nil))))
           (ecase order
             (:ascending
              (loop for i below (bnode-nr-bindings node)
                    for key = (node-binding-key node i)
                    ;; If the current key is too big, all remaining keys
                    ;; will also be too big.
                    while (not (funcall too-big-p key))
                    do (unless (funcall too-small-p key)
                         (funcall function key (node-binding-value node i)))))
             (:descending
              (loop for i from (1- (bnode-nr-bindings node)) downto 0
                    for key = (node-binding-key node i)
                    ;; If the current key is too small, all remaining keys
                    ;; will also be too small.
                    while (not (funcall too-small-p key))
                    do (unless (funcall too-big-p key)
                         (funcall function key (node-binding-value node i)))))))
       ;; Intermediate node.
       (ecase order
         (:ascending
          (loop for i below (bnode-nr-bindings node)
                for key = (node-binding-key node i)
                ;; All child keys will be less than or equal to the current key
                ;; and greater than the key to the left (if there is one).
                ;; So if MAX is less than the left neighbour key, we're done.
                until (and max
                           (plusp i)
                           (funcall (btree-key< btree)
                                    max
                                    (node-binding-key node (1- i))))
                ;; And if MIN is greater than the current key, we can skip this
                ;; child.
                unless (and min
                            (not (eql key 'key-irrelevant))
                            (funcall (btree-key> btree) min key))
                do (map-btree-keys-for-node btree (node-binding-value node i)
                                            function min max include-min include-max
                                            order)))
         (:descending
          (loop for i from (1- (bnode-nr-bindings node)) downto 0
                for key = (node-binding-key node i)
                ;; All child keys will be less than or equal to the current key
                ;; and greater than the key to the left (if there is one).
                ;; So if MIN is greater than the current key, we're done.
                until (and min
                           (not (eql key 'key-irrelevant))
                           (funcall (btree-key> btree) min key))
                ;; And if MAX is less than the left neighbour key, we can skip
                ;; this child.
                unless (and max
                            (plusp i)
                            (funcall (btree-key< btree)
                                     max
                                     (node-binding-key node (1- i))))
                do (map-btree-keys-for-node btree (node-binding-value node i)
                                            function min max include-min include-max
                                            order)))))))


;; This is an in-memory version of btrees.  At the moment it's not used
;; by the rest of the system.

(defpackage :btree
  (:use :cl)
  (:export
   ;; Btrees
   #:btree
   #:btree-key< #:btree-key= #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class

   ;; Nodes
   #:btree-node

   ;; Functions
   #:btree-search #:btree-insert #:map-btree

   ;; Conditions
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value))

(in-package :btree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; B-trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Basically, a B-tree is a balanced multi-way tree.

The reason for using multi-way trees instead of binary trees is that the nodes
are expected to be on disk; it would be inefficient to have to execute
a disk operation for each tree node if it contains only 2 keys.

The key property of B-trees is that each possible search path has the same
length, measured in terms of nodes.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition btree-error (error)
  ((btree :initarg :btree :reader btree-error-btree)))

(define-condition btree-search-error (btree-error)
  ((key :initarg :key :reader btree-error-key)))

(define-condition btree-insertion-error (btree-error)
  ((key :initarg :key :reader btree-error-key)
   (value :initarg :value :reader btree-error-value)))

(define-condition btree-key-already-present-error (btree-insertion-error)
  ())

(define-condition btree-type-error (btree-error type-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass btree ()
  ((key<   :initarg :key<   :reader btree-key<   :initform '<)
   (key=   :initarg :key=   :reader btree-key=   :initform 'eql)
   (value= :initarg :value= :reader btree-value= :initform 'eql)
   ;;
   (node-class :initarg :node-class
               :reader btree-node-class
               :initform 'btree-node)
   (max-node-size :initarg :max-node-size
                  :reader btree-max-node-size
                  :initform 100
                  :documentation "An integer specifying the preferred maximum number
of keys per btree node.")
   (unique-keys-p :initarg :unique-keys-p
                 :reader btree-unique-keys-p
                 :initform t
                 :documentation "If false, one key can correspond to more than one value.")
   (key-type :initarg :key-type
             :reader btree-key-type
             :initform t
             :documentation "The type of all keys.")
   (value-type :initarg :value-type
               :reader btree-value-type
               :initform t
               :documentation "The type of all values.")
   (root :accessor btree-root)))



;;
;; The next two classes are for internal use only, so we don't bother
;; with fancy long names.
;;

(defclass btree-node ()
  ((index :initarg :index
          :initform '()
          :accessor btree-node-index
          :documentation "A vector of key/value pairs.  The keys are
sorted by KEY<. No two keys can be the same.  For leaf nodes of btrees
with non-unique-keys, the value part is actually a list of values.
For intermediate nodes, the value is a child node.  All keys in the
child node will be KEY< the child node's key in the parent node.")
   (index-count :initform 0
                :accessor btree-node-index-count
                :documentation "The number of key/value pairs in the index vector.")
   (leaf-p :initarg :leaf-p :initform nil :reader btree-node-leaf-p)))

(defun node-binding (node i)
  (svref (btree-node-index node) i))

(defun (setf node-binding) (binding node i)
  (setf (svref (btree-node-index node) i)
        binding))

(defmethod initialize-instance :after ((node btree-node) &key btree &allow-other-keys)
  (setf (btree-node-index node) (make-array (btree-max-node-size btree)
                                      :initial-element nil)
        (btree-node-index-count node) 0))


(defmethod print-object ((node btree-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "with ~D pairs" (btree-node-index-count node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric btree-search (btree key &key errorp default-value)
  (:documentation "Returns the value (or list of values, for btrees
that don't have unique keys) corresponding to KEY.  If the btree has
non-unique keys and no value is found, the empty list is returned.  If
the btree has unique keys and no value is found, the result depends on
ERRORP option: if ERRORP is true, a btree-search-error is signalled;
otherwise, DEFAULT-VALUE is returned."))


(defmethod btree-search (btree key &key (errorp t) (default-value nil))
  (if (slot-boundp btree 'root)
      (node-search btree (slot-value btree 'root) key errorp default-value)
    (not-found btree key errorp default-value)))


(defun not-found (btree key errorp default-value)
  (if (btree-unique-keys-p btree)
      (if errorp
          ;; DO: Provide restarts here (USE-VALUE, STORE-VALUE, ...).
          (error 'btree-search-error :btree btree :key key)
        default-value)
    '()))

;;
;; Node-search
;;

(defgeneric node-search (btree node key errorp default-value)
  (:method ((btree btree) (node btree-node) key errorp default-value)
   (if (btree-node-leaf-p node)
       (let ((binding (find key (btree-node-index node)
                            :key #'car
                            :test (btree-key= btree)
                            :end (btree-node-index-count node))))
         (if binding
             (cdr binding)
           (not-found btree key errorp default-value)))
     (let ((subnode (find-subnode btree node key)))
       (node-search btree subnode key errorp default-value)))))


(defun find-subnode (btree node key)
  "Returns the subnode that contains more information for the given key."
  ;; Find the first binding with a key >= the given key and return
  ;; the corresponding subnode.
  ;; DO: We should probably use binary search for this.
  (loop for i below (btree-node-index-count node)
        for binding across (btree-node-index node)
        do (cond ((= i (1- (btree-node-index-count node)))
                  ;; We're at the last binding.
                  (return-from find-subnode (cdr binding)))
                 ((funcall (btree-key< btree) key (car binding))
                  (let ((next-binding (node-binding node (1+ i))))
                    (if (funcall (btree-key= btree) key (car next-binding))
                        (return-from find-subnode (cdr next-binding))
                        (return-from find-subnode (cdr binding)))))))
  (error "This shouldn't happen."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric btree-insert (btree key value &key if-exists))

(defmethod btree-insert ((btree btree) key value &key (if-exists :overwrite))
  ;; Check that key and value are of the right type.
  (unless (typep key (btree-key-type btree))
    (error 'btree-type-error
           :btree btree
           :datum key
           :expected-type (btree-key-type btree)))
  (unless (typep value (btree-key-type btree))
    (error 'btree-type-error
           :btree btree
           :datum value
           :expected-type (btree-value-type btree)))
  ;; Do the real work.
  (if (slot-boundp btree 'root)
      (btree-node-insert btree (slot-value btree 'root) nil key value if-exists)
    ;; Create a root.
    (let ((leaf (make-instance (btree-node-class btree) :btree btree :leaf-p t)))
      (setf (node-binding leaf 0) (make-leaf-binding btree key value)
            (btree-node-index-count leaf) 1)
      (let* ((empty-leaf (make-instance (btree-node-class btree) :btree btree :leaf-p t))
             (root (make-root btree key empty-leaf 'key-irrelevant leaf)))
        (setf (btree-root btree) root))))
  ;; Return the inserted value.
  value)

(defun check-btree (btree)
  ;; Check that it is completely sorted.
  (let (prev-key)
    (map-btree btree
               (lambda (key value)
                 (declare (ignore value))
                 (when prev-key
                   (unless (funcall (btree-key< btree) prev-key key)
                     (error "Btree inconsistency between ~D and ~D" prev-key key)))
                 (setq prev-key key)))))
                   

(defun make-root (btree left-key left-subnode right-key right-subnode)
  (let* ((root (make-instance (btree-node-class btree) :btree btree)))
    (setf (node-binding root 0) (make-binding left-key left-subnode)
          (node-binding root 1) (make-binding right-key right-subnode)
          (btree-node-index-count root) 2)
    root))

(defun make-binding (key value)
  (cons key value))

(defun make-leaf-binding (btree key value)
  (cons key
        (if (btree-unique-keys-p btree) value (list value))))

;;
;; Node insert
;;

(defgeneric btree-node-insert (btree node parent key value if-exists))

(defmethod btree-node-insert ((btree btree) (node btree-node) parent key value if-exists)
  (cond ((node-almost-full-p btree node)
         (split-btree-node btree node parent)
         (btree-insert btree key value :if-exists if-exists))
        ((btree-node-leaf-p node)
         (leaf-insert btree node key value if-exists))
        (t (let ((subnode (find-subnode btree node key)))
             (btree-node-insert btree subnode node key value if-exists)))))


(defun smallest-key (node)
  (if (btree-node-leaf-p node)
      (car (node-binding node 0))
    (smallest-key (cdr (node-binding node 0)))))

(defun biggest-key (node)
  (if (btree-node-leaf-p node)
      (car (node-binding node (1- (btree-node-index-count node))))
    (biggest-key (cdr (node-binding node (1- (btree-node-index-count node)))))))


(defun split-btree-node (btree node parent)
  ;; The node is (almost) full.
  ;; Create two new nodes and divide the current node-index over
  ;; these two new nodes.
  (let* ((split-pos (floor (btree-node-index-count node) 2))
         (left (make-instance (btree-node-class btree)
                              :parent parent
                              :btree btree
                              :leaf-p (btree-node-leaf-p node)))
         (right (make-instance (btree-node-class btree)
                               :parent parent
                               :btree btree
                               :leaf-p (btree-node-leaf-p node))))
    ;; Divide the node over the two new nodes.
    (setf (subseq (btree-node-index left)  0) (subseq (btree-node-index node) 0 split-pos)
          (btree-node-index-count left) split-pos
          (subseq (btree-node-index right) 0) (subseq (btree-node-index node) split-pos)
          (btree-node-index-count right) (- (btree-node-index-count node) split-pos))
    ;;
    (let* ((node-pos (and parent (node-position node parent)))
           (parent-binding (and parent (node-binding parent node-pos)))
           (left-key
            ;; The key that splits the two new nodes.
            (smallest-key right))
           (right-key
            (if (null parent)
                'key-irrelevant
              (car parent-binding))))
      (if (eql node (btree-root btree))
          ;; Make a new root.
          (setf (btree-root btree) (make-root btree left-key left right-key right))
        ;; Replace the original subnode by the left-child and
        ;; add a new-binding with new-key & right-child.
        (progn
          (setf (car parent-binding) left-key
                (cdr parent-binding) left)
          ;; Insert a new binding for the right node.
          (insert-new-binding parent
                              (1+ node-pos)
                              (cons right-key right)))))))

(defun parent-binding (node parent)
  (node-binding parent (node-position node parent)))

(defun node-position (node parent)
  (position node (btree-node-index parent)
            :key #'cdr
            :end (btree-node-index-count parent)))


(defun insert-new-binding (node position new-binding)
  (unless (>= position (btree-node-index-count node))
    ;; Make room by moving bindings to the right.
    (setf (subseq (btree-node-index node) (1+ position) (1+ (btree-node-index-count node)))
          (subseq (btree-node-index node) position (btree-node-index-count node))))
  ;; Insert new binding.
  (setf (node-binding node position) new-binding)
  (incf (btree-node-index-count node)))


(defun check-node (btree node)
  (loop for i below (1- (btree-node-index-count node))
        for left-key = (car (node-binding node i))
        for right-key = (car (node-binding node (1+ i)))
        do (unless (or (eql right-key 'key-irrelevant)
                       (funcall (btree-key< btree) left-key right-key))
             (error "Inconsistent node ~S" node))))



(defun leaf-insert (btree leaf key value if-exists)
  (let ((binding (find key (btree-node-index leaf)
                       :key #'car
                       :test (btree-key= btree)
                       :end (btree-node-index-count leaf))))
    (if binding
        ;; Key already exists.
        (if (btree-unique-keys-p btree)
            (ecase if-exists
              (:overwrite
               (setf (cdr binding) value))
              (:error
               ;; Signal an error unless the old value happens to be
               ;; the same as the new value.
               (unless (funcall (btree-value= btree) (cdr binding) value)
                 (error 'btree-key-already-present-error
                        :btree btree
                        :key key
                        :value value))))
          ;; For non-unique keys, we ignore the :if-exists options and
          ;; just add value to the list of values (unless value is already
          ;; there).
          (unless (find value (cdr binding) :test (btree-value= btree))
            (push value (cdr binding))))
      ;; The key doesn't exist yet. Create a new binding and add it to the
      ;; leaf index in the right position.
      (let ((new-binding (make-leaf-binding btree key value))
            (new-position (position key (btree-node-index leaf)
                                    :test (btree-key< btree)
                                    :key #'car
                                    :end (btree-node-index-count leaf))))
        (insert-new-binding leaf
                            (or new-position (btree-node-index-count leaf))
                            new-binding)))))



(defun node-almost-full-p (btree node)
  (>= (btree-node-index-count node) (1- (btree-max-node-size btree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric map-btree (btree function
                             &key min max include-min include-max order)
  (:documentation "Calls FUNCTION for all key/value pairs in the btree where key
is in the specified interval. FUNCTION must be a binary function; the first
argument is the btree key, the second argument is the btree value (or list of
values, for btrees with non-unique keys).

MIN, MAX, INCLUDE-MIN and INCLUDE-MAX specify the interval.  The interval is
left-open if MIN is nil, right-open if MAX is nil.  The interval is inclusive
on the left if INCLUDE-MIN is true (and exclusive on the left otherwise).
The interval is inclusive on the right if INCLUDE-MAX is true (and exclusive
on the right otherwise).

ORDER is either :ASCENDING (default) or :DESCENDING."))

(defmethod map-btree ((btree btree) function
                      &key min max include-min include-max (order :ascending))
  (when (slot-boundp btree 'root)
    (map-btree-node btree (slot-value btree 'root) function
              min max include-min include-max order)))

(defgeneric map-btree-node (btree node function min max include-min include-max order)
  ;; DO: This only works when MIN, MAX, INCLUDE-MIN and INCLUDE-MAX are all nil.
  ;; Implement the general case. Also implement the descending order.
  (:method ((btree btree) (node btree-node) function min max include-min include-max order)
   (loop repeat (btree-node-index-count node)
         for binding across (btree-node-index node)
         do (if (btree-node-leaf-p node)
                (funcall function (car binding) (cdr binding))
              (map-btree-node btree (cdr binding) function min max include-min include-max order)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test ()
  (let ((btree (make-instance 'btree)))
    (btree-insert btree 0 'zero)
    (btree-insert btree 15 'fifteen)
    (btree-insert btree 10 'ten)
    ;;
    (list (btree-search btree 0)
          (btree-search btree 10)
          (btree-search btree 15)
          (btree-search btree 42 :errorp nil))
    btree))


(defun test-insert (&optional (n 1000))
  (let ((btree (make-instance 'btree :value= 'string-equal :max-node-size 100)))
    (loop for i from 1 to n
          for key = (random n)
          do (btree-insert btree key (format nil "~R" key)))
    btree))

(defun test-map (&optional (n 2000))
  (let ((btree (make-instance 'btree :value= 'string-equal :max-node-size 10)))
    (loop for i from 1 to n
          for key = i
          do (btree-insert btree key (format nil "~R" key)))
    (map-btree btree
               (lambda (key value)
                 (when (< n 10000)
                   (format t "~&~D is '~A' in Roman notation.~%" key value))))
    btree))

(defun test-map-2 (&optional (n 100000))
  (let ((btree (make-instance 'btree :value= 'string-equal :max-node-size 1000)))
    (loop for i from 1 to n
          do (let* ((key (random n))
                    (value "") #+nil(format nil "~R" key))
               (btree-insert btree key value)))
    (when (< n 10000)
      (map-btree btree
                 (lambda (key value)
                   (when (< n 10000)
                     (format t "~&~D is '~A' in Roman notation.~%" key value)))))
    btree))

(defun timing-test (&optional (n 10000))
  (let ((btree (make-instance 'btree)))
    (time (loop for i from 1 to n
                do (btree-insert btree i i)))
    (time (let ((sum 0))
            (map-btree btree
                       (lambda (key value) (setq sum (+ key value))))
            (values btree sum)))))

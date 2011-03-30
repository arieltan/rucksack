;; $Id: rucksack.lisp,v 1.27 2009/05/27 14:26:25 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksacks: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open-rucksack [Function]
;; close-rucksack [Function]
;; with-rucksack [Macro]
;; current-rucksack [Function]

;; commit [Function]
;; rollback [Function]

(defgeneric add-rucksack-root (object rucksack)
  (:documentation
 "Adds an object to the root set of a rucksack."))

(defgeneric delete-rucksack-root (object rucksack)
  (:documentation
 "Delete an object from the root set of a rucksack."))

(defgeneric map-rucksack-roots (function rucksack)
  (:documentation
 "Applies a function to all objects in the root set of a rucksack."))

(defgeneric rucksack-roots (rucksack)
  (:documentation
 "Returns a list with all objects in the root set of a rucksack.  You
shouldn't modify this list."))

(defgeneric rucksack-root-p (object rucksack)
  (:documentation
   "Returns true iff OBJECT is a member of the root set of a rucksack."))

(defgeneric rucksack-cache (rucksack)
  (:documentation "Returns the cache for a rucksack."))

(defgeneric rucksack-directory (rucksack)
  (:documentation
 "Returns a pathname for the directory that contains all files of a
rucksack."))

(defgeneric rucksack-commit (rucksack)
  (:documentation
 "Ensures that all in-memory data is saved to disk."))

(defgeneric rucksack-rollback (rucksack)
  ;; DO: What does rollback mean exactly here?
  (:documentation "...."))

;;
;;  Class and slot indexing
;;

;; add-class-index (class-designator &key errorp)  [Function]
;; add-slot-index (class-designator slot index-spec &key errorp) [Function]
;; remove-class-index (class-designator &key errorp) [Function]
;; remove-slot-index (class-designator slot &key errorp) [Function]
;; map-class-indexes (function) [Function]
;; map-slot-indexes (function &key class include-subclasses) [Function]


(defgeneric rucksack-update-class-index (rucksack class)
  (:documentation 
   "Compares the current class index for CLASS to the class index
that's specified in the :INDEX class option of CLASS.  An obsolete
class index (i.e. a class index that's specified anymore in the class
option) is removed, new class indexes are added."))

(defgeneric rucksack-update-slot-indexes (rucksack class)
  (:documentation 
   "Compares the current slot indexes for CLASS to the slot indexes
that are specified in the slot options for the direct slots of CLASS.
Obsolete slot indexes (i.e. slot indexes that are not specified
anymore in the slot options or indexes for slots that don't exist
anymore) are removed, new slot indexes are added."))

(defgeneric rucksack-add-class-index (rucksack class-designator &key errorp))

(defgeneric rucksack-remove-class-index (rucksack class-designator
                                                  &key errorp))

(defgeneric rucksack-class-index (rucksack class-designator &key errorp)
  (:documentation "Returns the class index for a class designator."))

(defgeneric rucksack-map-class-indexes (rucksack function)
  (:documentation
   "FUNCTION must take two arguments: a class name and a class index.
It is called for all class indexes in the specified rucksack."))

(defgeneric rucksack-make-class-index (rucksack class &key index-spec)
  (:documentation
   "Creates a new class index and returns that index.  INDEX-SPEC
specifies the kind of index that must be created (if not supplied, the
rucksack's default class index spec will be used."))


(defgeneric rucksack-add-slot-index (rucksack class-designator slot index-spec
                                     unique-p &key errorp)
  (:documentation
  "Creates a new slot index for the slot designated by
CLASS-DESIGNATOR and SLOT.  The type of index is specified by
INDEX-SPEC.  Returns the new index.  Signals an error if ERRORP is T
and there already is an index for the designated slot."))

(defgeneric rucksack-remove-slot-index (rucksack class-designator slot
                                        &key errorp))



(defgeneric rucksack-slot-index (rucksack class-designator slot
                                 &key errorp include-superclasses)
  (:documentation
 "Returns the slot index for the slot specified by CLASS-DESIGNATOR
and SLOT."))


(defgeneric rucksack-map-slot-indexes (rucksack function
                                       &key class include-subclasses)
  (:documentation
   "FUNCTION must take three arguments: a class name, a slot name and
a slot index.  It is called for all slot indexes in the specified
rucksack.
  CLASS defaults to T, meaning all classes.
  INCLUDE-SUBCLASSES defaults to T."))

(defgeneric rucksack-maybe-index-changed-slot (rucksack 
                                               class object slot
                                               old-value new-value
                                               old-boundp new-boundp)
  (:documentation
 "This function is called after a slot has changed.  OLD-VALUE is the
slot's value before the change, NEW-VALUE is the current value.
OLD-BOUNDP is true iff the slot was bound before the change,
NEW-BOUNDP is true iff the slot is currently bound."))

(defgeneric rucksack-maybe-index-new-object (rucksack class-designator object)
  (:documentation
 "Adds the object id of OBJECT to the class index for the class
designated by CLASS-DESIGNATOR.  If there is no such class index, it
does nothing."))

(defgeneric rucksack-map-class (rucksack class function
                                &key id-only include-subclasses)
  (:documentation
 "  FUNCTION is a unary function that gets called for all instances of
the specified class.  Unindexed classes (i.e. classes for which the
:indexed class option is nil) will be skipped.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))

(defmacro rucksack-do-class ((instance-var class
                              &key
                              (rucksack '*rucksack*)
                              id-only
                              (include-subclasses t))
                             &body body)
  "Evaluate BODY for each instance of CLASS, with INSTANCE-VAR
successively bound to each instance.  See the documentation of
RUCKSACK-MAP-CLASS for more details."
  (check-type instance-var symbol)
  `(rucksack-map-class ,rucksack ,class
                       (lambda (,instance-var) ,@body)
                       :id-only ,id-only
                       :include-subclasses ,include-subclasses))
                                 
                                          
(defgeneric rucksack-map-slot (rucksack class slot function
                              &key equal min max include-min include-max order
                              include-subclasses)
  (:documentation
 " FUNCTION is a unary function that gets called for all instances of
the specified class that have a slot value matching the EQUAL, MIN,
MAX INCLUDE-MIN and INCLUDE-MAX arguments (see the documentation of
MAP-INDEX for a description of these arguments).
  ORDER can be either :ASCENDING (default) or :DESCENDING; currently,
the specified order will be respected for instances of one class but
not across subclasses.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))

(defmacro rucksack-do-slot ((instance-var class slot
                             &rest args
                             &key (rucksack '*rucksack*)
                             equal min max include-min include-max
                             order include-subclasses)
                            &body body)
  "Evaluate BODY for each instance of CLASS where SLOT has the
specified value. INSTANCE-VAR will be bound successively to each
instance.  See the documentation of RUCKSACK-MAP-SLOT for more
details."
  (declare (ignorable equal min max include-min include-max order
                      include-subclasses))
  (check-type instance-var symbol)
  `(rucksack-map-slot ,rucksack ,class ,slot
                      (lambda (,instance-var) ,@body)
                      ,@(sans args ':rucksack)))


#+later
(defgeneric rucksack-map-objects (rucksack class-designator function
                                           slots filter order)
  (:documentation
 " Applies FUNCTION to all instances of the class designated by
CLASS-DESIGNATOR for which the criteria specified by SLOTS and
CRITERIA hold.
  SLOTS is a list of slot names.  FILTER is a filter expression that can
refer to the slot names.
  Example of a filter expression: (and (= age 20) (string= city \"Hamburg\"))
"))


(defgeneric rucksack-delete-object (rucksack object)
  (:documentation
   "Removes OBJECT from RUCKSACK, i.e. removes object from the
rucksack roots (if it is a root) and from all class and slot indexes
in which it appears."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-lock (&key (name "lock"))
  #+allegro
  (mp:make-process-lock :name name)
  #+lispworks
  (mp:make-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+openmcl
  (ccl:make-lock name)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'make-lock))


(defmacro with-lock ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock) ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #-(or allegro lispworks sbcl openmcl)
  (not-implemented 'with-lock))

(defun process-lock (lock)
  #+lispworks
  (mp:process-lock lock)
  #+sbcl
  (sb-thread:get-mutex lock)
  #+openmcl
  (ccl:try-lock lock)
  #-(or sbcl lispworks openmcl)
  (not-implemented 'process-lock))


(defun process-unlock (lock)
  #+lispworks
  (mp:process-unlock lock)
  #+sbcl
  (sb-thread:release-mutex lock)
  #+openmcl
  (ccl:release-lock lock)
  #-(or sbcl lispworks openmcl)
  (not-implemented 'process-unlock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WITH-TRANSACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It would be prettier if we could put this macro in TRANSACTIONS.LISP, but
;; we need it here already.

(defparameter *transaction* nil
  "The currently active transaction.")
 
(defmacro with-transaction ((&rest args
                             &key
                             (rucksack '(current-rucksack))
                             (inhibit-gc nil inhibit-gc-supplied-p)
                             &allow-other-keys)
                            &body body)
  (let ((committed (gensym "COMMITTED"))
        (transaction (gensym "TRANSACTION"))
        (result (gensym "RESULT")))
    `(let ((,transaction nil)
           (*collect-garbage-on-commit* (if ,inhibit-gc-supplied-p
                                            ,(not inhibit-gc)
                                            *collect-garbage-on-commit*)))
       (loop named ,transaction do         
          (with-simple-restart (retry "Retry ~S" ,transaction)
            (let ((,committed nil)
                  (,result nil))
              (unwind-protect
                   (progn
                     ;; Use a local variable for the transaction so that nothing
                     ;; can replace it from underneath us, and only then bind
                     ;; it to *TRANSACTION*. 
                     (setf ,transaction (transaction-start :rucksack ,rucksack
                                                           ,@(sans args :rucksack)))
                     (let ((*transaction* ,transaction))
                       (with-simple-restart (abort "Abort ~S" ,transaction)
                         (setf ,result (progn ,@body))
                         (transaction-commit ,transaction)
                         (setf ,committed t)))
                     ;; Normal exit from the WITH-SIMPLE-RESTART above -- either
                     ;; everything went well or we aborted -- the ,COMMITTED will tell
                     ;; us. In either case we jump out of the RETRY loop.
                     (return-from ,transaction (values ,result ,committed)))
                (unless ,committed
                  (transaction-rollback ,transaction)))))
            ;; Normal exit from the above block -- we selected the RETRY restart.
            ))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass rucksack ()
  ())

(defclass standard-rucksack (rucksack)
  ((cache :reader rucksack-cache)
   (directory :initarg :directory :reader rucksack-directory)
   (roots :initform '()
          :documentation "A list with the object ids of all root
objects, i.e.  the objects from which the garbage collector can reach
all live objects.")
   (roots-changed-p :initform nil :accessor roots-changed-p)
   (highest-transaction-id :initform 0
                           :accessor highest-transaction-id
                           :type integer
                           :documentation "The highest transaction ID
in the entire rucksack.  This is saved together with the roots.")
   ;; Indexes
   (class-index-table
    :documentation "The object id of a btree mapping class names to
class indexes.  Each class index contains the ids of all instances
from a class; it maps object ids to objects.")
   (slot-index-tables
    :documentation "The object id of a btree mapping class names to
slot index tables, where each slot index table is a btree mapping slot
names to slot indexes.  Each slot index maps slot values to
objects.")))

(defmethod print-object ((rucksack rucksack) stream)
  (print-unreadable-object (rucksack stream :type t :identity t)
    (format stream "in ~S with ~D root~:P"
            (rucksack-directory rucksack)
            (length (slot-value rucksack 'roots)))))

(defmethod rucksack-roots-pathname ((rucksack standard-rucksack))
  (merge-pathnames "roots" (rucksack-directory rucksack)))


(defmethod class-index-table ((rucksack standard-rucksack))
  ;; Create class-index-table if it doesn't exist yet.
  (flet ((do-it ()
           (unless (slot-boundp rucksack 'class-index-table)
             ;; Create a btree mapping class names to class
             ;; indexes.
             (let ((btree (make-instance 'btree
                                         :rucksack rucksack
                                         :key< 'string<
                                         :value= 'p-eql
                                         :unique-keys-p t
                                         :dont-index t)))
               (setf (slot-value rucksack 'class-index-table) (object-id btree)
                     (roots-changed-p rucksack) t)))
           (cache-get-object (slot-value rucksack 'class-index-table)
                             (rucksack-cache rucksack))))
    (if (current-transaction)
        (do-it)
      (with-transaction (:rucksack rucksack)
        (do-it)))))


(defmethod slot-index-tables ((rucksack standard-rucksack))
  ;; Create slot-index-tables if they don't exist yet.
  (flet ((do-it ()
           (unless (slot-boundp rucksack 'slot-index-tables)
             ;; Create a btree mapping class names to slot
             ;; index tables.
             (let ((btree (make-instance 'btree
                                         :rucksack rucksack
                                         :key< 'string<
                                         :value= 'p-eql
                                         :unique-keys-p t
                                         :dont-index t)))
               (setf (slot-value rucksack 'slot-index-tables) (object-id btree)
                     (roots-changed-p rucksack) t)))
           ;;
           (cache-get-object (slot-value rucksack 'slot-index-tables)
                             (rucksack-cache rucksack))))
    (if (current-transaction)
        (do-it)
      (with-transaction (:rucksack rucksack)
        (do-it)))))


(defmethod initialize-instance :after ((rucksack standard-rucksack)
                                       &key
                                       (cache-class 'lazy-cache)
                                       (cache-args '())
                                       &allow-other-keys)
  ;; Open cache.
  (setf (slot-value rucksack 'cache)
        (apply #'open-cache (rucksack-directory rucksack)
               :class cache-class
               :rucksack rucksack
               cache-args))
  (load-roots rucksack))



(defun load-roots (rucksack)
  ;; Read roots (i.e. object ids) from the roots file (if there is
  ;; one).  Also load the highest transaction id and the (object ids
  ;; of the) class and slot index tables.
  (let ((roots-file (rucksack-roots-pathname rucksack)))
    (when (probe-file roots-file)
      (destructuring-bind (root-list class-index slot-index
                                     &optional
                                     ;; Added in version 0.1.20.
                                     highest-transaction)
          (load-objects roots-file)
        (with-slots (roots class-index-table slot-index-tables highest-transaction-id)
            rucksack
          (setf roots root-list)
          (when class-index
            (setf class-index-table class-index))
          (when slot-index
            (setf slot-index-tables slot-index))
          (when highest-transaction
            (setf highest-transaction-id highest-transaction))))))
  rucksack)


(defun save-roots (rucksack)
  (save-objects (list (slot-value rucksack 'roots)
                      (and (slot-boundp rucksack 'class-index-table)
                           (slot-value rucksack 'class-index-table))
                      (and (slot-boundp rucksack 'slot-index-tables)
                           (slot-value rucksack 'slot-index-tables))
                      (slot-value rucksack 'highest-transaction-id))
                (rucksack-roots-pathname rucksack))
  (setf (roots-changed-p rucksack) nil))

(defun save-roots-if-necessary (rucksack)
  (when (roots-changed-p rucksack)
    (save-roots rucksack)))
  
(defmethod add-rucksack-root (object (rucksack standard-rucksack))
  (add-rucksack-root-id (object-id object) rucksack))

(defun add-rucksack-root-id (object-id rucksack)
  (push object-id (slot-value rucksack 'roots))
  (setf (roots-changed-p rucksack) t))

(defmethod delete-rucksack-root (object (rucksack standard-rucksack))
  (with-slots (roots)
      rucksack
    (setf roots (delete (object-id object) roots)
          (roots-changed-p rucksack) t)))

(defmethod map-rucksack-roots (function (rucksack standard-rucksack))
  (loop for root-id in (slot-value rucksack 'roots)
        do (funcall function
                    (cache-get-object root-id (rucksack-cache rucksack)))))


(defmethod rucksack-roots ((rucksack standard-rucksack))
  (let ((result '()))
    (map-rucksack-roots (lambda (root) (push root result))
                        rucksack)
    ;; We don't need to nreverse the list, because the order isn't specified.
    result))

(defmethod rucksack-root-p (object (rucksack standard-rucksack))
  (member (object-id object)
          (slot-value rucksack 'roots)))

;;
;; Opening
;;

(defparameter *rucksack-opening-lock*
  (make-lock :name "Rucksack opening lock"))
 
(defun open-rucksack (directory-designator 
                      &rest args
                      &key 
                      (class 'serial-transaction-rucksack)
                      (if-exists :overwrite)
                      (if-does-not-exist :create)
                      (cache-class 'lazy-cache)
                      (cache-args '())
                      &allow-other-keys)
  "Opens the rucksack in the directory designated by DIRECTORY-DESIGNATOR.
  :IF-DOES-NOT-EXIST can be either :CREATE (creates a new rucksack if the
it does not exist; this is the default) or :ERROR (signals an error if
the rucksack does not exist).
  :IF-EXISTS can be either :OVERWRITE (loads the rucksack if it exists;
this is the default), :SUPERSEDE (deletes the existing rucksack and creates
a new empty rucksack) or :ERROR (signals an error if the rucksack exists)."
  (declare (ignorable cache-class cache-args))
  (check-type directory-designator (or string pathname))
  (check-type if-exists (member :overwrite :supersede :error))
  (check-type if-does-not-exist (member :create :error))
  (let ((directory (if (stringp directory-designator)  
                      (pathname directory-designator)
                      directory-designator)))
    (with-lock (*rucksack-opening-lock*)
      (setq *rucksack*
            (if (probe-file (merge-pathnames "roots" directory))
                ;; Rucksack already exists.
                (ecase if-exists
                  (:error
                   (error "Can't create rucksack in ~S: the directory
already seems to contain a rucksack."
                          directory))
                  (:supersede
                   ;; Remove all rucksack files from the directory.
                   (loop for file in (rucksack-files-in-directory directory)
                         do (delete-file file))
                   ;; And create a fresh rucksack.
 		   (apply #'make-instance class :directory directory args))
                  (:overwrite
                   ;; This is the normal case.
                   (apply #'make-instance class :directory directory args)))
              ;; Rucksack doesn't seem to exist.
              (ecase if-does-not-exist
                (:error
                 (error "Can't open rucksack in ~S: the rucksack roots
file is missing."
                        directory))
                (:create
                 (ensure-directories-exist directory)
                 (apply #'make-instance class :directory directory args))))))))


(defun rucksack-files-in-directory (directory-pathname)
  "Returns a list with the pathnames of all Rucksack files
in the specified directory."
  (list (merge-pathnames "roots" directory-pathname)
        (merge-pathnames "objects" directory-pathname)
        (merge-pathnames "heap" directory-pathname)
        (merge-pathnames "schemas" directory-pathname)))


(defun close-rucksack (rucksack &key (commit t))
  (when commit
    (rucksack-commit rucksack))
  ;; If :COMMIT is true, the cache and transaction handler are already
  ;; committed by the rucksack-commit, so we close them without committing.
  (close-cache (rucksack-cache rucksack) :commit nil))

;;
;; Commit
;;

(defun commit (&key (rucksack (current-rucksack)))
  (rucksack-commit rucksack))

(defmethod rucksack-commit ((rucksack standard-rucksack))
  (when (or (roots-changed-p rucksack)
            (not (slot-boundp rucksack 'class-index-table))
            (not (slot-boundp rucksack 'slot-index-tables)))
    (save-roots rucksack))
  (cache-commit (rucksack-cache rucksack)))

;;
;; Rollback
;;

(defun rollback (&key (rucksack (current-rucksack)))
  (rucksack-rollback rucksack))

(defmethod rucksack-rollback ((rucksack standard-rucksack))
  ;; Rollback the cache.
  (cache-rollback (rucksack-cache rucksack))
  ;; Rollback the roots by loading them back from file.
  (load-roots rucksack)
  (setf (roots-changed-p rucksack) nil))

;;
;; Some small stuff
;;

(defmacro with-rucksack ((rucksack directory &rest args) &body body)
   `(let* ((*rucksack* *rucksack*)
           (,rucksack (open-rucksack ,directory ,@args)))
      (unwind-protect (progn ,@body)
        (close-rucksack ,rucksack))))


(defun test-garbage-collector (rucksack)
  (collect-garbage (heap (rucksack-cache rucksack))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rucksack-update-class-index ((rucksack standard-rucksack)
                                        (class persistent-class))
  (let ((old-index (rucksack-class-index rucksack class :errorp nil))
        (needs-index-p (class-index class)))
    (cond ((and old-index (not needs-index-p))
           (rucksack-remove-class-index rucksack class :errorp t))
          ((and (not old-index) needs-index-p)
           ;; Create a class index now.
           ;; NOTE: If there are existing instances of this class,
           ;; they're *not* automatically indexed at this point.
           ;; (In fact, the only way to do this would be to iterate
           ;; over *all* objects in the rucksack, which would be rather
           ;; expensive.  Then again, it's exactly what the garbage
           ;; collector does anyway, so it may be an option to have the
           ;; garbage collector index them automatically.  But I'm not
           ;; sure if that's a good idea.)
           (rucksack-add-class-index rucksack class :errorp t))
          (t
           ;; We don't need to change anything
           :no-change))))



(defmethod rucksack-update-slot-indexes ((rucksack standard-rucksack)
                                         (class persistent-class))
  (let ((direct-slots (class-direct-slots class))
        (indexed-slot-names (rucksack-indexed-slots-for-class rucksack class)))
    ;; Remove indexes for slots that don't exist anymore.
    (loop for slot-name in indexed-slot-names
          unless (find slot-name direct-slots :key #'slot-definition-name)
          do (rucksack-remove-slot-index rucksack class slot-name :errorp nil))
    ;; Update indexes for the current set of direct slots.
    (dolist (slot direct-slots)
      (let ((index-spec (and (slot-persistence slot)
                             (or (find-index-spec (slot-index slot) :errorp nil)
                                 (slot-index slot))))
            (unique-p (slot-unique slot))
            (slot-name (slot-definition-name slot)))
        (let* ((current-index (rucksack-slot-index rucksack class slot-name
                                                   :errorp nil
                                                   :include-superclasses nil))
               (current-index-spec (and current-index (index-spec current-index)))
               (current-unique-p (and current-index (index-unique-keys-p current-index))))
          (cond ((and (index-spec-equal index-spec current-index-spec)
                      (eql unique-p current-unique-p))
                 ;; We keep the same index: no change needed.
                 :no-change)
                ((and current-index-spec (null index-spec))
                 ;; The index is not wanted anymore: remove it.
                 (rucksack-remove-slot-index rucksack class slot :errorp t))
                ((and (null current-index-spec) index-spec)
                 ;; We didn't have an index but we need one now: add one.
                 (add-and-fill-slot-index rucksack class slot index-spec unique-p))
                ((and current-index-spec index-spec)
                 ;; We have an index but need a different one now.
                 (replace-slot-index rucksack class slot index-spec unique-p))))))))


(defun add-and-fill-slot-index (rucksack class slot index-spec unique-p)
  ;; We didn't have an index but we need one now: add one.
  (let ((index (rucksack-add-slot-index rucksack class slot index-spec unique-p
                                        :errorp t))
        (slot-name (slot-definition-name slot)))
    ;; Index all instances for the new index.
    ;; NOTE: This will only work if the class is indexed, otherwise there is no
    ;; affordable way to find all instances of the class.
    (when (class-index class)
      (rucksack-map-class rucksack class
                          (lambda (object)
                            (when (slot-boundp object slot-name)
                              (index-insert index (slot-value object slot-name)
                                            object)))))))


(defun replace-slot-index (rucksack class slot index-spec unique-p)
  ;; We have an index but need a different one now.  This requires
  ;; some care because we need to re-index all objects from the old
  ;; index.
  (let ((current-index (rucksack-slot-index rucksack class slot))
        (new-index (rucksack-add-slot-index rucksack class slot
                                            index-spec
                                            unique-p
                                            :errorp nil)))
    ;; Re-index all objects for the new index.
    ;; DO: This re-indexing can cause an error (e.g. if the old
    ;; index has non-unique keys, the new index has unique keys
    ;; and some keys occur more than once).  We need to handle
    ;; that error here and offer some decent restarts (e.g.
    ;; remove the index entirely, or go back to the old index).
    (map-index current-index
               (lambda (slot-value object)
                 (index-insert new-index slot-value object)))
    ;; We don't need to remove the old index explicitly, because
    ;; RUCKSACK-ADD-SLOT-INDEX already did that for us.
    ))

(defun find-old-index-spec (slot-name old-slots)
  (let ((slot (find slot-name old-slots :key #'slot-definition-name)))
    (and slot
         (with-slots (index unique)
             slot
           (values (or (find-index-spec index :errorp nil) index)
                   unique)))))


             
;;
;; Some simple dispatchers.
;;

;; Q: Are these really necessary?

(defun add-class-index (class-designator &key errorp)
  (rucksack-add-class-index (current-rucksack) class-designator
                            :errorp errorp))

(defun add-slot-index (class-designator slot index-spec &key (errorp nil))
  (rucksack-add-slot-index (current-rucksack) class-designator slot index-spec
                           :errorp errorp))

(defun remove-class-index (class-designator &key (errorp nil))
  (rucksack-remove-class-index (current-rucksack) class-designator
                               :errorp errorp))

(defun remove-slot-index (class-designator slot &key (errorp nil))
  (rucksack-remove-slot-index (current-rucksack) class-designator slot
                              :errorp errorp))

(defun map-class-indexes (function)
  (rucksack-map-class-indexes (current-rucksack) function))

(defun map-slot-indexes (function &key (class t) (include-subclasses t))
  (rucksack-map-slot-indexes (current-rucksack) function
                             :class class
                             :include-subclasses include-subclasses))

;;
;; Class indexes
;;

(defmethod rucksack-add-class-index ((rucksack standard-rucksack) class
                                     &key (errorp nil))
  ;; Create and add a class index to the class index table.
  (unless (symbolp class)
    (setq class (class-name class)))
  (when (and errorp (btree-search (class-index-table rucksack) class
                                  :errorp nil :default-value nil))
    (simple-rucksack-error "Class index for ~S already exists in ~A."
                           class
                           rucksack))
  (let ((index (rucksack-make-class-index rucksack class)))
    (btree-insert (class-index-table rucksack) class index
                  :if-exists :overwrite)
    index))

(defmethod rucksack-make-class-index 
           ((rucksack standard-rucksack) class
            &key (index-spec '(btree :key< < :value= p-eql)))
  ;; A class index maps object ids to objects.
  (declare (ignore class))
  (make-index index-spec t))

(defmethod rucksack-remove-class-index ((rucksack standard-rucksack) class
                                        &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (handler-bind ((btree-deletion-error
                  ;; Translate a btree error to something that makes more sense
                  ;; in this context.
                  (lambda (error)
                    (declare (ignore error))
                    (simple-rucksack-error "Class index for ~S doesn't exist in ~A."
                                           class
                                           rucksack))))
    (btree-delete-key (class-index-table rucksack) class
                      :if-does-not-exist (if errorp :error :ignore))))


(defmethod rucksack-map-class-indexes (rucksack function)
  (map-btree (class-index-table rucksack) function))

(defmethod rucksack-class-index ((rucksack standard-rucksack) class
                                 &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (handler-bind ((btree-search-error
                  ;; Translate a btree error to something that makes more sense
                  ;; in this context.
                  (lambda (error)
                    (declare (ignore error))
                    (simple-rucksack-error "Can't find class index for ~S in ~A."
                                           class
                                           rucksack))))
    (btree-search (class-index-table rucksack) class
                  :errorp errorp
                  :default-value nil)))


(defmethod rucksack-maybe-index-new-object ((rucksack standard-rucksack)
                                            class object)
  (let ((index (rucksack-class-index rucksack class :errorp nil)))
    (when index
      (index-insert index (object-id object) object
                    :if-exists :error))))


(defmethod rucksack-map-class ((rucksack standard-rucksack) class function
                               &key (id-only nil) (include-subclasses t))
  ;; EFFICIENCY: Follow Sean Ross' suggestion and implement ID-ONLY
  ;; by defining a function MAP-INDEX-KEYS and then calling
  ;; that function here (so that we don't need to load any objects
  ;; that we don't want to load yet).
  (let ((visited-p (make-hash-table)))
    (labels ((map-instances (class)
               (let ((index (rucksack-class-index rucksack class :errorp nil)))
                 (when index
                   (map-index index
                              (lambda (id object)
                                (if id-only
                                    (funcall function id)
                                  (funcall function object))))
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-instances class))))))
      (map-instances class))))

;;
;; Slot indexing
;;

(defmethod rucksack-add-slot-index ((rucksack standard-rucksack)
                                    class slot index-spec unique-p
                                    &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  ;; Find the slot index table for CLASS, create a slot index and add that
  ;; index to the table.
  (let* ((slot-index-tables (slot-index-tables rucksack))
         (slot-index-table
          (or (btree-search slot-index-tables class :errorp nil)
              (let ((table (make-instance 'btree
                                          :key< 'string<
                                          :value= 'p-eql
                                          :unique-keys-p t)))
                (btree-insert slot-index-tables class table :if-exists :error)
                table)))
         (new-slot-index (make-index index-spec unique-p)))
    (handler-bind ((btree-key-already-present-error
                    (lambda (error)
                      (declare (ignore error))
                      (simple-rucksack-error "Slot index for slot ~S of class ~S
already exists in ~S."
                                             slot
                                             class
                                             rucksack))))
      (btree-insert slot-index-table slot new-slot-index
                    :if-exists (if errorp :error :overwrite)))
    new-slot-index))


(defmethod rucksack-remove-slot-index (rucksack class slot &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (flet ((oops (error)
           (declare (ignore error))
           (simple-rucksack-error "Attempt to remove non-existing slot
index for slot ~S of class ~S in ~S."
                                  slot
                                  class
                                  rucksack)))
    ;; Return the slot name if everything went fine; otherwise, return
    ;; NIL (or signal an error).
    (and (handler-bind ((btree-search-error #'oops))
           (let ((slot-index-table (btree-search (slot-index-tables rucksack) class
                                                 :errorp errorp)))
             (and slot-index-table
                  (handler-bind ((btree-deletion-error #'oops))
                    (btree-delete-key slot-index-table slot
                                      :if-does-not-exist (if errorp :error :ignore))))))
         slot)))


(defmethod rucksack-map-slot-indexes ((rucksack standard-rucksack) function
                                      &key (class t) (include-subclasses t))
  (if (eql class t)
      (map-btree (slot-index-tables rucksack)
                 (lambda (class slot-index-table)
                   (map-btree slot-index-table
                              (lambda (slot slot-index)
                                (funcall function class slot slot-index)))))
    (let ((visited-p (make-hash-table)))
      (labels ((map-indexes (class)
                 (unless (gethash class visited-p)
                   (let ((slot-index-table (btree-search (slot-index-tables rucksack)
                                                         (class-name class)
                                                         :errorp nil)))
                     (when slot-index-table
                       (map-btree slot-index-table
                                  (lambda (slot slot-index)
                                    (funcall function (class-name class)
                                             slot
                                             slot-index)))))
                   (setf (gethash class visited-p) t)
                   (when include-subclasses
                     (mapc #'map-indexes
                           (class-direct-subclasses class))))))
        (map-indexes (if (symbolp class) (find-class class) class))))))


(defmethod rucksack-maybe-index-changed-slot ((rucksack standard-rucksack)
                                              class object slot
                                              old-value new-value
                                              old-boundp new-boundp)
  ;; SLOT is a slot-definition, not a slot name.
  (when (slot-index slot)
    (let ((index (rucksack-slot-index rucksack class slot
                                      :errorp nil
                                      :include-superclasses t)))
      (when index
        (when old-boundp
          (index-delete index old-value object
                        :if-does-not-exist :ignore))
        (when new-boundp
          (index-insert index new-value object
                        :if-exists (if (slot-unique slot)
                                       :error
                                     :overwrite)))))))


(defmethod rucksack-slot-index ((rucksack standard-rucksack) class slot
                                &key (errorp nil) (include-superclasses nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (let ((slot-index-tables (slot-index-tables rucksack)))
    (flet ((find-index (class)
             (let ((slot-index-table (btree-search slot-index-tables class
                                                   :errorp nil)))
 	       (and slot-index-table
                    (btree-search slot-index-table slot :errorp nil)))))
      (or (find-index class)
          (and include-superclasses
               (loop for superclass in (class-precedence-list (find-class class))
                     thereis (find-index (class-name superclass))))
          (and errorp
               (simple-rucksack-error
                "Can't find slot index for slot ~S of class ~S in ~S."
                slot
                class
                rucksack))))))


(defmethod rucksack-map-slot ((rucksack standard-rucksack) class slot function
                              &key min max include-min include-max
                              (equal nil equal-supplied)
                              (order :ascending) (include-subclasses t))
  (let ((visited-p (make-hash-table)))
    (labels ((map-slot (class)
               (let ((index (rucksack-slot-index rucksack class slot
                                                 :errorp nil)))
                 (when index
                   ;; The index maps slot values to objects.
                   (apply #'map-index
                          index
                          (lambda (slot-value object)
                            (declare (ignore slot-value))
                            (funcall function object))
                          :min min
                          :max max
                          :include-min include-min
                          :include-max include-max
                          :order order
                          (if equal-supplied (list :equal equal) '()))
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-slot class))))))
      (map-slot (if (symbolp class) (find-class class) class)))))


(defun rucksack-indexed-slots-for-class (rucksack class)
  "Returns a list with the names of the indexed direct slots of CLASS."
  (unless (symbolp class)
    (setq class (class-name class)))
  (let ((result '()))
    (rucksack-map-slot-indexes rucksack
                               (lambda (class-name slot-name slot-index)
                                 (declare (ignore slot-index))
                                 (when (eql class-name class)
                                   (push slot-name result))))
    result))


;;
;; Debugging
;;

(defun rucksack-list-slot-indexes (rucksack)
  (let ((result '()))
    (rucksack-map-slot-indexes rucksack
                               (lambda (class-name slot-name slot-index)
                                 (declare (ignore slot-index))
                                 (push (cons class-name slot-name)
                                       result)))
    result))

(defun rucksack-list-class-indexes (rucksack)
  (let ((result '()))
    (rucksack-map-class-indexes rucksack
                                (lambda (class-name index)
                                  (declare (ignore index))
                                  (push class-name result)))
    result))


                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rucksack-delete-object ((rucksack standard-rucksack) object)
  (let ((class-name (class-name (class-of object))))
    ;; Remove object from class index if necessary.
    (let ((class-index (rucksack-class-index rucksack (class-of object)
                                             :errorp nil)))
      (when class-index
        (index-delete class-index (object-id object) object)))
    ;; Remove object from slot indexes if necessary.
    (let ((indexed-slot-names (rucksack-indexed-slots-for-class rucksack
                                                                (class-of object))))
      (loop for slot-name in indexed-slot-names do
            (index-delete (rucksack-slot-index rucksack class-name slot-name)
                          (slot-value object slot-name)
                          object
                          :if-does-not-exist :ignore)))
    ;; Remove object from roots if necessary.
    (when (rucksack-root-p object rucksack)
      (delete-rucksack-root object rucksack))))


(defun rucksack-delete-objects (rucksack objects)
  (dolist (object objects)
    (rucksack-delete-object rucksack object)))



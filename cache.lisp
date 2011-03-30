;; $Id: cache.lisp,v 1.16 2009/05/27 14:26:25 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cache: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun open-cache (directory
;;                   &key (class 'standard-cache) (if-exists :overwrite)
;;                   (if-does-not-exist :create) (size 10000)
;;                   &allow-other-keys)
;;
;; Creates or opens a cache in the given directory and returns that
;; cache. SIZE is the number of objects that may be kept in memory.

(defgeneric close-cache (cache &key commit)
  (:documentation "Closes the cache.  If COMMIT is true (which is the
default), the objects in the cache will be written to disk before
closing the cache."))

(defgeneric cache-size (cache)
  (:documentation "Returns the number of non-dirty objects that the
cache may keep in memory."))

(defgeneric cache-count (cache)
  (:documentation "Returns the number of objects (both dirty and
non-dirty) in the cache."))

(defgeneric cache-create-object (object cache)
  (:documentation "Adds a new object to the cache and returns an
object id that can be used to retrieve the object from the cache.
Don't use this function twice for the same object."))

(defgeneric cache-get-object (object-id cache)
  (:documentation "Retrieves the object with the given id from the
cache and returns that object."))

(defgeneric cache-delete-object (object-id cache)
  (:documentation "Removes an object-id from the cache and from
the object table, so the object-id can be reused for another object
later."))

(defgeneric cache-commit (cache)
  (:documentation "Makes sure that all changes to the cache are
written to disk."))

(defgeneric cache-rollback (cache)
  (:documentation "Undoes all cache changes that were made since the
last cache-commit."))

(defgeneric cache-recover (cache)
  (:documentation "Undoes partially committed transactions to ensure
that the cache is in a consistent state."))


(defgeneric open-transaction (cache transaction)
  (:documentation "Adds a transaction to the set of open
transactions."))

(defgeneric close-transaction (cache transaction)
  (:documentation "Removes a transaction from the set of open
transactions."))

(defgeneric map-transactions (cache function)
  (:documentation "Applies a function to each open transaction in a
cache."))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cache ()
  ())

(defclass standard-cache (cache)
  ;; The cache uses a heap to manage the object memory and a schema table to
  ;; keep track of different class versions for objects in the heap.
  ((heap :initarg :heap :reader heap)
   (schema-table :initarg :schema-table :reader schema-table)
   (rucksack :initarg :rucksack :reader rucksack
             :documentation "Back pointer to the rucksack.")
   ;; Clean objects
   (objects :initarg :objects
            :reader objects
            :documentation "A hash-table \(from id to object)
containing the youngest committed version of all objects that are
currently kept in memory but are not dirty.  \('The youngest version'
means the version belonging to the youngest committed transaction.)")
   (queue :initform (make-instance 'queue) :reader queue
          :documentation "A queue of the ids of all non-dirty objects
that are currently in the cache memory.  Whenever an object is
retrieved (i.e. read), it's added to the queue.  If an object-id is
in this queue, it is not necessarily in the OBJECTS hash-table.")
   (last-timestamp :initform (get-universal-time)
                   :accessor last-timestamp)
   (transaction-id-helper :initform -1
                          :accessor transaction-id-helper)
   (transactions :initform (make-hash-table)
                 :reader transactions
                 :documentation "A mapping from transaction ids to
transactions.  Contains only open transactions, i.e. transactions that
haven't been rolled back or committed.")
   ;;
   (size :initarg :size :accessor cache-size
         :documentation "The maximum number of non-dirty objects that
will be kept in the cache memory.")
   (shrink-ratio :initarg :shrink-ratio
                 :initform 0.7
                 :accessor cache-shrink-ratio
                 :documentation "A number between 0 and 1.  When the
cache is full, i.e. when there are at least SIZE (non-dirty) objects
in the queue, it will be shrunk by removing (1 - SHRINK-RATIO) * SIZE
objects.")))


(defclass lazy-cache (standard-cache)
  ()
  (:documentation "A lazy cache doesn't bother with fancy mechanisms
for deciding which objects to remove from the cache.  It just fills
the cache until maximum capacity (i.e. CACHE-SIZE) and then clears
the entire cache at once.  Advantages of this could be that it uses
less time and less memory to do its work.  Disadvantage is that it's
very stupid about the objects it should try to keep in memory."))


(defmethod print-object ((cache standard-cache) stream)
  (print-unreadable-object (cache stream :type t :identity nil)
    (format stream "of size ~D, heap ~S and ~D objects in memory."
            (cache-size cache)
            (pathname (heap-stream (heap cache)))
            (cache-count cache))))



;;
;; Open/close/initialize
;;

(defvar *cache* nil)

(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  ;; From Usenet posting <3247672165664225@naggum.no> by Erik
  ;; Naggum.
  (let ((sans ()))
    (loop
      (let ((tail (nth-value 2 (get-properties plist keys))))
        ;; this is how it ends
        (unless tail
          (return (nreconc sans plist)))
        ;; copy all the unmatched keys
        (loop until (eq plist tail) do
              (push (pop plist) sans)
              (push (pop plist) sans))
        ;; skip the matched key
        (setq plist (cddr plist))))))

(defun open-cache (directory &rest args
                             &key (class 'standard-cache)
                             &allow-other-keys)
  (setq *cache*
        (apply #'make-instance class :directory directory
               (sans args :class))))


(defmethod close-cache ((cache standard-cache) &key (commit t))
  (when commit
    (cache-commit cache))
  (close-heap (heap cache))
  (close-schema-table (schema-table cache))
  'ok)

(defmacro with-cache ((cache directory &rest options) &body body)
  `(let ((,cache (open-cache ,directory ,@options)))
     (unwind-protect (progn ,@body)
       (close-cache ,cache))))

(defmethod initialize-instance :after ((cache standard-cache)
                                       &key
                                       directory
                                       (heap-class 'mark-and-sweep-heap)
                                       (heap-options '())
                                       (if-exists :overwrite)
                                       (if-does-not-exist :create)
                                       (size 100000)
                                       &allow-other-keys)
  (ensure-directories-exist directory)
  (let ((object-table (open-object-table (merge-pathnames "objects" directory)
                                         :if-exists if-exists
                                         :if-does-not-exist if-does-not-exist)))
    (setf (cache-size cache) size)
    (with-slots (heap schema-table objects)
        cache
      (setq heap (open-heap (merge-pathnames "heap" directory)
                            :class heap-class
                            :if-exists if-exists
                            :if-does-not-exist if-does-not-exist
                            :rucksack (rucksack cache)
                            :options (list* :object-table object-table
                                            heap-options))
            schema-table (open-schema-table (merge-pathnames "schemas" directory)
                                            :if-exists if-exists
                                            :if-does-not-exist if-does-not-exist)
            objects (make-hash-table :size size))
      (when (and (eql if-exists :overwrite) (probe-file (commit-filename cache)))
        ;; We're trying to work with an existing cache but the
        ;; commit file exists, so there may be a partially committed
        ;; transaction that we need to undo.
        (cache-recover cache)))))



(defun commit-filename (cache)
  (merge-pathnames "commit"
                   (pathname (heap-stream (heap cache)))))


;;
;; Cache info
;;

(defmethod cache-count ((cache standard-cache))
  (+ (hash-table-count (objects cache))
     (loop for transaction being the hash-value of (transactions cache)
           sum (transaction-nr-dirty-objects transaction))))

(defmethod cache-full-p ((cache cache))
  ;; Don't count dirty objects.
  (>= (hash-table-count (objects cache)) (cache-size cache)))

(defmethod cache-room ((cache cache))
  (- (cache-size cache) (cache-count cache)))

;;
;; Create/get/touch/delete
;;

(defmethod cache-create-object (object (cache standard-cache))
  ;; This is called by a before method on SHARED-INITIALIZE and
  ;; by MAKE-PERSISTENT-DATA.
  (let ((id (new-object-id (object-table (heap cache)))))
    ;; Add to dirty objects.
    (transaction-touch-object (current-transaction) object id)
    id))


(defmethod cache-touch-object (object (cache standard-cache))
  "Checks for transaction conflicts and signals a transaction conflict
if necessary.  Change the object's status to dirty.  If the object is
already dirty, nothing happens."
  ;; This function is called by (SETF SLOT-VALUE-USING-CLASS),
  ;; SLOT-MAKUNBOUND-USING-CLASS and P-DATA-WRITE.
  (let ((object-id (object-id object))
        (transaction (current-transaction)))
    ;; Check for transaction conflict.
    (let ((old-transaction
           (find-conflicting-transaction object-id cache transaction)))
      (when old-transaction
        (rucksack-error 'transaction-conflict
                        :object-id object-id
                        :new-transaction transaction
                        :old-transaction old-transaction)))
    ;;
    (unless (transaction-changed-object transaction object-id) ; already dirty
      ;; Remove object from the 'clean objects' hash table.
      ;; It would be nice to remove the object from the 'clean' queue too,
      ;; but that's too expensive.  We'll let MAKE-ROOM-IN-CACHE take care
      ;; of that.
      (remhash object-id (objects cache))
      ;; Let the transaction keep track of the dirty object.
      (transaction-touch-object transaction object object-id))))



(defmethod cache-get-object (object-id (cache standard-cache))
  (let* ((transaction (current-transaction))
         (result
          (or
           ;; Unmodified, already loaded and compatible with the
           ;; current transaction?  Fine, let's use it.
           (let ((object (gethash object-id (objects cache))))
             (and object
                  (or (null transaction)
                      (<= (transaction-id object) (transaction-id transaction)))
                  object))
           ;; Modified by an open transaction?  Try to find the
           ;; 'compatible' version.
           (find-object-version object-id transaction cache)
           ;; Not in memory at all? Then load the compatible version
           ;; from disk.
           (multiple-value-bind (object most-recent-p)
               (load-object object-id transaction cache)
             (when most-recent-p
               ;; Add to in-memory cache if the loaded object is
               ;; the most recent version of the object.
               (when (cache-full-p cache)
                 (make-room-in-cache cache))
               (setf (gethash object-id (objects cache)) object))
             object))))
    ;; Put it (back) in front of the queue, so we know which
    ;; objects were recently used when we need to make room
    ;; in the cache.
    ;; DO: If this object was already in the queue, we should remove it
    ;; from the old position.  But that's too expensive: so we actually
    ;; need a better data structure than a simple queue.
    (add-to-queue object-id cache)
    result))


(defun find-object-version (object-id current-transaction cache)
  "Returns the object version for OBJECT-ID that's compatible with
CURRENT-TRANSACTION, or NIL if there's no such version in the cache
memory."
  ;; The compatible object version for a transaction T is the version that
  ;; was modified by the youngest open transaction that's older than or
  ;; equal to T; if there is no such transaction, the compatible object
  ;; version is the most recent (committed) version on disk.
  ;; EFFICIENCY: Maybe we should use another data structure than a
  ;; hash table for faster searching in the potentially relevant
  ;; transactions?  An in-memory btree might be good...
  (and current-transaction
       (or 
        ;; Modified by the current-transaction itself?  Then use that version.
        (transaction-changed-object current-transaction object-id)
        ;; Otherwise iterate over all open transactions, keeping track
        ;; of the best candidate.
        (let ((result-transaction nil)
              (result nil))
          (loop for transaction being the hash-value of (transactions cache)
                for object = (transaction-changed-object transaction object-id)
                when (and object
                          (transaction-older-p transaction current-transaction)
                          (or (null result-transaction)
                              (transaction-older-p result-transaction transaction)))
                do (setf result-transaction transaction
                         result object))
          result))))


(defmethod cache-delete-object (object-id (cache standard-cache))
  (remhash object-id (objects cache)))


;;
;; Queue operations
;;

(defmethod make-room-in-cache ((cache standard-cache))
  ;; We need to remove some objects from the in-memory cache (both
  ;; from the hash table and from the queue).
  ;; We do this by removing the objects that have been used least
  ;; recently.  We don't do anything with dirty objects, because
  ;; they contain changes that must still be committed to disk.
  (let ((queue (queue cache))
        (nr-objects-to-remove (* (- 1.0 (cache-shrink-ratio cache))
                                 (cache-size cache)))
        (nr-objects-removed 0))
    (loop until (or (= nr-objects-removed nr-objects-to-remove)
                    (queue-empty-p queue))
          do (let ((id (queue-remove queue)))
               (when (remhash id (objects cache))
                 (incf nr-objects-removed))))))


(defmethod add-to-queue (object-id (cache standard-cache))
  ;; Add an object to the end of the queue.
  (let ((queue (queue cache)))
    (when (cache-full-p cache)
      (queue-remove queue))
    (queue-add queue object-id)))

;;
;; Queue operations for lazy caches
;;

(defmethod make-room-in-cache ((cache lazy-cache))
  (clrhash (objects cache)))

(defmethod add-to-queue (object-id (cache lazy-cache))
  ;; We're not adding anything to the queue, because we're too lazy.
  object-id)


;;
;; Open/close/map transactions
;;

(defmethod open-transaction ((cache standard-cache) transaction)
  ;; Add to open transactions.
  (setf (gethash (transaction-id transaction) (transactions cache))
        transaction))

(defmethod close-transaction ((cache standard-cache) transaction)
  (remhash (transaction-id transaction) (transactions cache)))

(defmethod map-transactions ((cache standard-cache) function)
  ;; FUNCTION may be a function that closes the transaction (removing
  ;; it from the hash table), so we create a fresh list with transactions
  ;; before doing the actual iteration.
  (let ((transactions '()))
    (loop for transaction being the hash-value of (transactions cache)
          do (push transaction transactions))
    ;; Now we can iterate safely.
    (mapc function transactions)))


;;
;; Commit/rollback
;;

(defmethod cache-rollback ((cache standard-cache))
  ;; Roll back by rolling back all transactions and removing
  ;; all objects from the cache, so they'll be reloaded
  ;; from disk the next time.
  (map-transactions cache #'transaction-rollback)
  (clrhash (objects cache))
  (queue-clear (queue cache))
  ;; DO: Reverse the schema table to its previous state?
  )


(defmethod cache-commit ((cache standard-cache))
  ;; Commit all transactions.
  (map-transactions cache #'transaction-commit)
  ;; Save the schema table.
  (save-schema-table (schema-table cache)))

;;
;; Recovery
;;

(defmethod cache-recover ((cache standard-cache))
  ;; NOTE: This code assumes there's at most one partial commit
  ;; at any time.
  (multiple-value-bind (transaction-id object-ids)
      ;; There's a possibility that the transaction was
      ;; aborted while it was writing to the commit file
      ;; (so before actually committing anything).
      ;; In that case, LOAD-OBJECTS will probably fail
      ;; with an error and transaction-id will be
      ;; nil.  This is fine, because we won't need to
      ;; undo anything in that case anyway.
      (ignore-errors (load-commit-file cache))
    (when (and transaction-id object-ids)
      (loop for object-id in object-ids
            do (undo-object-commit cache transaction-id object-id)))))

(defgeneric undo-object-commit (cache transaction-id object-id)
  (:documentation "If the object version list contains a version with
the given transaction-id, unhook that version from the list.  Returns
T if the object was already comitted, otherwise nil."))

(defmethod undo-object-commit ((cache standard-cache)
                               partial-transaction-id
                               object-id)
  ;; OBJECT-ID is the id of an object that may have been committed by
  ;; PARTIAL-TRANSACTION-ID, but it's also possible that it hasn't
  ;; been committed yet.

  (let ((object-table (object-table cache))
        (heap (heap cache)))

    (when (eql :reserved (object-info object-table object-id))
      ;; It hasn't been committed yet, so we don't need to
      ;; do anything.
      (return-from undo-object-commit nil))

    ;; Walk along the version list, looking for a version
    ;; that was committed by partial-transaction-id.
    (let ((block (object-heap-position object-table object-id))
          (younger nil))
      (loop
       (let ((buffer (load-block heap block :skip-header t)))
         (multiple-value-bind (id nr-slots schema transaction-id older)
             (load-object-fields buffer object-id)
           ;; DO: Don't load id, nr-slots, schema at all!
           (declare (ignore id nr-slots schema)) 
           (cond ((= transaction-id partial-transaction-id)
                  ;; Got it.  Remove from the version list.
                  (if younger
                      (setf (object-version-list younger heap) older)
                    ;; There is no younger version so we're the first
                    ;; in the version list.  If there's an older version,
                    ;; let the object table point to that older version.
                    ;; Otherwise, remove the object table entry.
                    (if older
                        (setf (object-heap-position object-table object-id)
                              older)
                      (delete-object-id object-table object-id)))
                  (return-from undo-object-commit t))
                 ((null older)
                  ;; It hasn't been committed yet, so we don't need to
                  ;; do anything.
                  (return-from undo-object-commit nil))
                 (t
                  ;; Keep trying older versions.
                  (setq younger block
                        block older)))))))))





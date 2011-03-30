;; $Id: garbage-collector.lisp,v 1.22 2008/02/03 12:32:16 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass garbage-collector ()
  ((object-table :initarg :object-table :reader object-table)
   (buffer :initform (make-instance 'serialization-buffer)
           :reader serialization-buffer)
   (rucksack :initarg :rucksack :reader rucksack)
   ;; Some state used for incremental garbage collection.
   (roots :initarg :roots :initform '() :accessor roots
          :documentation "A list of object-ids of roots that must be kept alive.")
   (state :initform :ready
          :type (member :starting
                        :finishing
                        :ready
                        ;; For copying collector
                        :copying
                        ;; For mark-and-sweep collector
                        :marking-object-table
                        :scanning
                        :sweeping-heap
                        :sweeping-object-table)
          :accessor state)
   (doing-work :initform nil :accessor gc-doing-work
               ;; NOTE: This flag is probably not necessary anymore and
               ;; should probably be removed.
               :documentation
               "A flag to prevent recursive calls to COLLECT-SOME-GARBAGE.")))


(defgeneric scan (buffer garbage-collector)
  (:documentation "Scans the object in the serialization buffer, marking or
evacuating (depending on garbage collector type) any child objects."))

(defmethod scan (buffer (gc garbage-collector))
  ;; Read serialize marker and dispatch.
  (let ((marker (read-next-marker buffer)))
    (unless marker
      (cerror "Ignore the error and continue."
              "Garbage collection error: can't find next scan marker.")
      (return-from scan))
    ;; Most of the SCAN-CONTENTS methods are in serialize.lisp.
    (scan-contents marker buffer gc)))



(defmethod gc-work-for-size ((heap heap) size)
  ;; The garbage collector needs to be ready when there's no more free space
  ;; left in the heap. So when SIZE octets are allocated, the garbage collector
  ;; needs to collect a proportional amount of bytes:
  ;;
  ;;     Size / Free = Work / WorkLeft
  ;;
  ;; or: Work = (Size / Free) * WorkLeft
  ;;
  (if (zerop size)
      0
    (let* ((free (free-space heap))
           (work-left (work-left heap)))
      (if (>= size free)
          work-left
        (floor (* size work-left) free)))))

(defmethod free-space ((heap heap))
  ;; Returns an estimate of the number of octets that can be
  ;; allocated until the heap is full (i.e. heap-end >= heap-max-end).
  ;; For a copying collector, this number is very close to the truth.
  ;; But for mark-and-sweep collectorsestimate it is a very conservative
  ;; estimate, because we only count the heap space that hasn't been
  ;; reserved by one of the free lists (because you can't be sure that
  ;; a free list block can actually be used to allocate an arbitrary-sized
  ;; block).
  (- (max-heap-end heap) (heap-end heap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mark and sweep collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mark-and-sweep-heap (garbage-collector free-list-heap serializer)
  (;; Some counters that keep track of the amount of work done by
   ;; the garbage collector.
   (nr-object-bytes-marked :initform 0 :accessor nr-object-bytes-marked)
   (nr-heap-bytes-scanned :initform 0 :accessor nr-heap-bytes-scanned)
   (nr-heap-bytes-sweeped :initform 0 :accessor nr-heap-bytes-sweeped)
   (nr-object-bytes-sweeped :initform 0 :accessor nr-object-bytes-sweeped)
   ;; Heap growth related slots.
   (max-heap-end :accessor max-heap-end
                 :documentation "The maximum acceptable value for heap-end
during the current garbage collection.")
   (grow-size :initarg :grow-size
              :initform nil
              :accessor grow-size
              :documentation
 "Specifies a minimum amount to grow the heap when it needs to grow.
If 'grow size' is an integer, the expected growth rate is additive and
the integer is the number of octets to add; if it is a float, the
expected growth rate for the heap is multiplicative and the float is
the ratio of the new size to the old size.  (The actual size might be
rounded up.)")))


(defparameter *initial-heap-size* (* 10 1024 1024)
  "The default initial heap size is 10 MB. ")

(defmethod initialize-instance :after ((heap mark-and-sweep-heap)
                                       &key size &allow-other-keys)
  ;; Give max-heap-end its initial value (depending on the :size initarg).
  (let ((proposed-size (or size *initial-heap-size*)))
    (setf (max-heap-end heap) (if (> proposed-size (heap-size heap))
                                  (+ (heap-start heap) proposed-size)
                                (heap-end heap))
          (grow-size heap) (or (grow-size heap)
                               (max-heap-end heap))))
  ;; GC should begin in the :ready state.  It will switch to :starting
  ;; state when the heap is expanded.
  (setf (state heap) :ready))


(defmethod close-heap :after ((heap mark-and-sweep-heap))
  (close-heap (object-table heap)))

(defmethod initialize-block (block block-size (heap mark-and-sweep-heap))
  ;; This is called by a free list heap while creating free blocks.
  ;; Write the block size (as a negative number) in the start of the
  ;; block (just behind the header) to indicate that this is a free
  ;; block.  This is necessary for the sweep phase of a mark-and-sweep
  ;; collector to distinguish it from a block that contains an object.
  (file-position (heap-stream heap) (+ block (block-header-size heap)))
  (serialize (- block-size) (heap-stream heap)))


(defmethod handle-written-object (object-id block (heap mark-and-sweep-heap))
  ;; (This is called just after a (version of an) object has been
  ;; written to the heap.) Mark the object entry dead if the collector
  ;; is in the marking-object-table or scanning phase, and live otherwise.
  (setf (object-info (object-table heap) object-id)
        (case (state heap)
          ((:starting :marking-object-table :scanning)
           :dead-object)
          (otherwise
           :live-object)))
  ;; In the scanning phase, the object id must be added to the root set to
  ;; guarantee that it will be marked and scanned.
  (when (eql (state heap) :scanning)
    (push object-id (roots heap))))

;;
;; Hooking into free list methods
;;




(defmethod expand-heap :after ((heap mark-and-sweep-heap) block-size)
  ;; If the GC is ready but the heap must be expanded because the free
  ;; list manager can't find a free block, we know that we should start
  ;; collecting garbage.
  (when (eql (state heap) :ready)
    (setf (state heap) :starting)))


;;
;; Counting work
;;

(defmethod work-left ((heap mark-and-sweep-heap))
  "Returns the amount of work that needs to be done (i.e. octets that must be
'collected') before the current garbage collection has finished."
  (- (max-work heap) (work-done heap)))

(defmethod work-done ((heap mark-and-sweep-heap))
  (+ (nr-object-bytes-marked heap)
     (nr-heap-bytes-scanned heap)
     (nr-heap-bytes-sweeped heap)
     (nr-object-bytes-sweeped heap)))

(defmethod max-work ((heap mark-and-sweep-heap))
  "Returns the maximum possible amount of work that the garbage
collector needs to do for one complete garbage collection."
  (+ 
   ;; Mark and sweep the object table
   (* 2 (nr-object-bytes heap))
   ;; Mark and sweep the heap
   (* 2 (nr-heap-bytes heap))))

(defmethod nr-object-bytes ((heap mark-and-sweep-heap))
  "Returns the number of object bytes that must be handled by the garbage
collector."
  (* (object-table-size (object-table heap))
     (min-block-size (object-table heap))))

(defmethod nr-heap-bytes ((heap mark-and-sweep-heap))
  "Returns the number of heap bytes that must be handled by the garbage
collector."
  (heap-size heap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collect some garbage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod collect-garbage ((heap mark-and-sweep-heap))
  ;; A simple test of COLLECT-SOME-GARBAGE: keep collecting 1024 bytes of
  ;; garbage until the garbage collector is ready.
  (setf (state heap) :starting)
  (loop until (eql (state heap) :ready)
        do (collect-some-garbage heap 1024)))

(defmethod finish-garbage-collection ((heap mark-and-sweep-heap))
  ;; Make sure that the garbage collector is in the :ready state.
  (loop until (eql (state heap) :ready)
        do (collect-some-garbage heap (* 512 1024))))

(defmethod collect-some-garbage ((heap mark-and-sweep-heap) amount)
  ;; Collect at least the specified amount of garbage
  ;; (i.e. mark or sweep at least the specified amount of octets).
  ;; DO: We probably need a heap lock here?
  (unless (gc-doing-work heap) ; Don't do recursive GCs.
    (unwind-protect
        (progn
          (setf (gc-doing-work heap) t)
          (loop until (or (eql (state heap) :ready) (<= amount 0))
                do (ecase (state heap)
                     (:starting
                      (let ((rucksack (rucksack heap)))
                        ;; We were not collecting garbage; start doing that now.
                        (setf (nr-object-bytes-marked heap) 0
                              (nr-heap-bytes-scanned heap) 0
                              (nr-heap-bytes-sweeped heap) 0
                              (nr-object-bytes-sweeped heap) 0
                              ;; We don't need to copy the roots, because we're not
                              ;; going to modify the list (just push and pop).
                              ;; But we do need to add the btrees for the class-index-table
                              ;; and slot-index-tables to the GC roots.
                              (roots heap) (append (and (slot-boundp rucksack 'class-index-table)
                                                        (list (slot-value rucksack 'class-index-table)))
                                                   (and (slot-boundp rucksack 'slot-index-tables)
                                                        (list (slot-value rucksack 'slot-index-tables)))
                                                   (slot-value (rucksack heap) 'roots))))
                      (setf (state heap) :marking-object-table))
                     (:marking-object-table
                      (decf amount (mark-some-objects-in-table heap amount)))
                     (:scanning
                      (decf amount (mark-some-roots heap amount)))
                     (:sweeping-heap
                      (decf amount (sweep-some-heap-blocks heap amount)))
                     (:sweeping-object-table
                      (decf amount (sweep-some-object-blocks heap amount)))
                     (:finishing
                      ;;  Grow the heap by the specified GROW-SIZE.
                      (if (integerp (grow-size heap))
                          (incf (max-heap-end heap) (grow-size heap))
                        (setf (max-heap-end heap)
                              (round (* (grow-size heap) (max-heap-end heap)))))
                      ;;
                      (setf (state heap) :ready)))))
      (setf (gc-doing-work heap) nil))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marking the object table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mark-some-objects-in-table ((heap mark-and-sweep-heap) amount)
  ;; Mark all 'live' objects in the object table as dead (temporarily).
  ;; Returns the amount of work done.
  (let* ((object-table (object-table heap))
         (object-block-size (min-block-size object-table))
         (first-object-id (floor (nr-object-bytes-marked heap)
                                 object-block-size))
         (work-done 0))
    (loop for object-id from first-object-id
          while (and (< object-id (object-table-size object-table))
                     (< work-done amount))
          do (progn 
               (when (eql (object-info object-table object-id) :live-object)
                 ;; Don't touch free or reserved blocks.
                 (setf (object-info object-table object-id) :dead-object))
               (incf (nr-object-bytes-marked heap) object-block-size)
               (incf work-done object-block-size)))
    (when (>= (nr-object-bytes-marked heap) (nr-object-bytes heap))
      ;; We've finished this stage.  Move to the next step.
      (setf (state heap) :scanning))
    ;; Return the amount of work done.
    work-done))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marking roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mark-some-roots ((heap mark-and-sweep-heap) amount)
  ;; Mark some roots and their descendants as alive.
  ;; (This may add new roots.)
  (let ((work-done 0))
    (loop while (and (roots heap) (< work-done amount))
          do (let ((root (pop (roots heap))))
               (incf work-done (mark-root heap root))))
    (when (null (roots heap))
      ;; We've finished marking roots.  Move to the next step.
      (setf (state heap) :sweeping-heap))
    ;; Return the amount of work done.
    work-done))


(defmethod mark-root ((heap mark-and-sweep-heap) (object-id integer))
  ;; Returns the number of octets scanned.
  (let ((object-table (object-table heap)))
    (if (member (object-info object-table object-id) '(:reserved :live-object))
        ;; Reserved objects aren't written to the heap yet (they just
        ;; have an object table entry), so we don't need to scan them
        ;; for child objects.  And live objects were already marked earlier,
        ;; so don't need to be scanned again now.
        0
      (let* ((block (object-heap-position object-table object-id))
             (buffer (load-block heap block :skip-header t)))
        (setf (object-info object-table object-id) :live-object)
        (scan-object object-id buffer heap)
        ;; Keep track of statistics.
        (let ((block-size (block-size block heap)))
          (incf (nr-heap-bytes-scanned heap) block-size)
          ;; Return the amount of work done.
          block-size)))))


(defmethod load-block ((heap mark-and-sweep-heap) block
                       &key (buffer (serialization-buffer heap))
                       (skip-header nil))
  ;; Loads the block at the specified position into the
  ;; serialization buffer.  If SKIP-HEADER is T, the block
  ;; header is not included.  Returns the buffer.
  (load-buffer buffer
               (heap-stream heap)
               (block-size block heap)
               :eof-error-p nil
               :file-position (if skip-header
                                  (+ block (block-header-size heap))
                                block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sweeping the heap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod sweep-some-heap-blocks ((heap mark-and-sweep-heap)
                                   (amount integer))
  (let* ((object-table (object-table heap))
         (block (+ (heap-start heap) (nr-heap-bytes-sweeped heap)))
         (work-done 0))
    ;; Sweep across the heap, looking for dead blocks.
    (loop
     while (and (< work-done amount)
                (< block (heap-end heap)))
     do (multiple-value-bind (block-header block-start)
            (read-block-start heap block)
          ;; For non-free blocks, the block start contains a previous-pointer,
          ;; which can be either nil or a positive integer.
          ;; A negative block-start means the block already belongs to
          ;; a free list. In that case, the block size is the abs of
          ;; the block start.
          ;; A non-negative (or nil) block-start means the block is occupied.
          ;; In that case, the block size is in the header.
          (let* ((free-p (and (integerp block-start) (minusp block-start)))
                 (block-size (if free-p (- block-start) block-header)))
            ;; Reclaim dead blocks.
            (when (not free-p) ; only non-free blocks
              (let* ((heap-stream (heap-stream heap))
                     (object-id (progn
                                  (deserialize heap-stream)
                                  (deserialize heap-stream))))
                (when (not (block-alive-p object-table object-id block))
                  ;; The block is dead (either because the object is dead
                  ;; or because the block contains an old version): return
                  ;; the block to its free list.
                  (deallocate-block block heap))))
            ;;
            (incf work-done block-size)
            ;; Move to next block (if there is one).
            (incf block block-size))))
    ;;
    (incf (nr-heap-bytes-sweeped heap) work-done)
    (when (>= block (heap-end heap))
      ;; We've finished sweeping the heap: move to the next state.
      (setf (state heap) :sweeping-object-table))
    ;; Return the amount of work done.
    work-done))

(defmethod block-alive-p ((object-table object-table) object-id block)
  "Returns true iff the object in the block is alive."
  ;; DO: Some versions of this object may not be reachable anymore.
  ;; Those should be considered dead.
  (member (object-info object-table object-id) '(:reserved :live-object)))

(defun read-block-start (heap position)
  ;; All blocks start the same way: 8 bytes for the block header
  ;; (containing the size or a pointer to the next free block),
  ;; followed by the previous version pointer (a serialized positive
  ;; integer or nil) or the block size (a serialized negative integer; for
  ;; free blocks).
  (let ((stream (heap-stream heap)))
    (file-position stream position)
    (let ((block-header (read-unsigned-bytes (cell-buffer heap) stream)))
      (file-position stream (+ 8 position))
      (let ((block-start (deserialize stream)))
        (values block-header block-start)))))

(defun (setf object-alive-p) (value object-table object-id)
  (setf (object-info object-table object-id) 
        (if value :live-object :dead-object))
  value)

(defun object-alive-p (object-table object-id)
  (eql (object-info object-table object-id) :live-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sweeping the object table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sweep-some-object-blocks ((heap mark-and-sweep-heap)
                                     (amount integer))
  ;; Deallocate some dead object blocks.
  (let* ((object-table (object-table heap))
         (object-block-size (min-block-size object-table))
         (current-id (floor (nr-object-bytes-sweeped heap)
                            object-block-size))
         (work-done 0))
    (loop for object-id from current-id
          while (and (< work-done amount)
                     (< object-id (object-table-size object-table)))
          do (progn
               ;; Hook dead object blocks back into the free list.
               (when (eql (object-info object-table object-id) :dead-object)
                 (delete-object-id object-table object-id)
                 ;; Don't forget to remove the id->object mapping from
                 ;; the cache!  (This was a difficult bug to find.)
                 (cache-delete-object object-id (rucksack-cache (rucksack heap))))
               (incf (nr-object-bytes-sweeped heap) object-block-size)))
    ;;
    (when (>= (nr-object-bytes-sweeped heap) (nr-object-bytes heap))
      ;; We've finished sweeping the object table: move to the next state.
      (setf (state heap) :finishing))
    ;; Return the amount of work done.
    work-done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
;;; Parameters to control GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *collect-garbage-on-commit* t
  "A flag to indicate whether or not transaction-commit collects garbage")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
;;; MAYBE LATER: MERGING DEAD BLOCKS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; The object is dead: try to merge neighbouring dead blocks
;; with this block.
(note-dead-object gc object-table object-id)
(loop (let ((next-block-position (+ position block-size)))
        (when (>= next-block-position heap-size)
          (return))
        (multiple-value-bind (next-block-size next-object-id)
            (read-block-start heap next-block-position)
          (if (object-alive-p object-table next-object-id)
              (return)
            (progn
              (note-dead-object gc object-table object-id)
              ;; Merge dead blocks.
              (incf block-size next-block-size))))))
           ;; Give the merged block back to the free-list manager.
           (deallocate-block heap position block-size)
           ;; Keep track of statistics.
           (incf (nr-dead-bytes gc) block-size))))

(defun note-dead-object (gc object-table object-id)
  ;; Keep track of statistics.
  (incf (nr-dead-objects gc))
  ;; Return the object-id to the object-table free list.
  (delete-object-id object-table object-id))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copying garbage collector
;;;
;;; This code is incomplete.  At the moment we use a mark & sweep collector.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| MAYBE LATER


(defclass copying-heap (garbage-collector serializer)
  ((space-0 :initarg :space-0 :reader space-0)
   (space-1 :initarg :space-1 :reader space-1)
   (from-space :accessor from-space)
   (to-space :accessor to-space)
   (evac-pointer :initform 0 :accessor evac-pointer
                 :documentation "The position in to-space where the next object
can be evacuated.")))

(defmethod collect-some-garbage ((heap copying-collector) amount)
  'DO-THIS)

(defmethod gc-work-for-size ((heap copying-collector) nr-allocated-octets)
  'DO-THIS)

(defmethod close-heap :after ((heap copying-heap))
  (close-heap (object-table heap)))

(defmethod deserialize-byte ((gc copying-collector)
                             &optional (eof-error-p t))
  ;; Hook into the serializer: deserializing a byte
  ;; means reading a byte from to-space at the trace-pointer.
  (file-position (to-space gc) (trace-pointer gc))
  (let ((result (read-byte (to-space gc) eof-error-p)))
    (incf (trace-pointer gc))
    result))



(defmethod initialize-instance :after ((gc copying-collector)
                                       &key &allow-other-keys)
  (setf (from-space gc) (space-0 gc)
        (to-space gc) (space-1 gc)))

(defmethod flip ((gc copying-collector))
  (rotatef (from-space gc) (to-space gc)))

(defun to-space-number (gc)
  (if (eql (to-space gc) (space-0 gc))
      0
    1))


;;
;; Evacuate
;;

(defgeneric evacuate (garbage-collector object-id object-table)
  (:documentation "Moves the specified object from from-space to to-space."))

(defmethod evacuate ((gc copying-collector) object-id object-table)
  (let ((heap-position (heap-position-for-id object-id object-table))
        (from-space (from-space gc))
        (to-space (to-space gc)))
    (file-position from-space heap-position)
    (let ((block-size (read-unsigned-bytes (cell-buffer gc) from-space)))
      ;; Read the block in from-space.
      (fill-heap-buffer gc from-space (- block-size +nr-block-header-bytes+))
      ;; And write it to to-space.
      (write-unsigned-bytes block-size (cell-buffer gc) to-space)
      (flush-heap-buffer gc (to-space gc) (evac-pointer to-space))
      ;; Update the object-table
      (setf (heap-position-for-id object-id object-table (to-space-number gc))
            (evac-pointer to-space))
      ;; Update evacuation pointer.
      (incf (evac-pointer to-space) block-size))))


;;
;; Trace
;; (The real work is done by trace-contents in serialize.lisp.)
;;


(defmethod scan-contents ((marker (eql +cached-object+))
                          buffer
                          (gc copying-collector))
  ;; Hook into the scanner: when the scanner finds a cached-object,
  ;; it evacuates that object and returns.
  (let ((object-id (deserialize gc)))
    (evacuate gc object-id (object-table (cache gc)))))

|#

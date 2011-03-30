;; $Id: heap.lisp,v 1.16 2008/01/22 17:02:07 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heaps: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
* heap [Class]

* open-heap [Function]

* close-heap [Function]
|# 


(defgeneric heap-stream (heap)
  (:documentation "Returns the heap's stream."))

(defgeneric heap-start (heap)
  (:documentation "Returns the position of the first block in the heap."))

(defgeneric heap-end (heap)
  (:documentation "Returns the end of the heap."))

(defgeneric (setf heap-end) (value heap)
  (:documentation "Modifies the end of the heap."))

(defgeneric allocate-block (heap &key size expand)
  (:documentation "Allocates a block of the requested size and returns
the heap position of that block.  If the free list is full and EXPAND
is true, the system will try to expand the free list; otherwise it
returns nil.
  As a second value, ALLOCATE-BLOCK returns the number of octets that
were allocated.
Note: both the requested size and the returned heap position include
the block's header."))

;; DO: Many more generic functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +pointer-size+ 8
  "The number of octets for a heap pointer.  A heap pointer is a number that
must be able to span the entire heap.  It is used for block sizes, pointers
to other blocks, object ids and object heap positions.")

(defclass heap ()
  ((stream :initarg :stream :accessor heap-stream)
   (cell-buffer :initform (make-array +pointer-size+
                                      :element-type '(unsigned-byte 8))
                ;; Just a buffer for 1 cell.
                :reader cell-buffer)
   (end :accessor heap-end
        :documentation "The end of the heap.  For free-list heaps, this number
is stored in the first heap cell. For appending heaps, it's stored in the
end of the file.")
   (max-size :initarg :max-size
             :initform nil :accessor max-heap-size
             :documentation "The maximum size (in octets) for the heap.
If nil, the heap is allowed to expand indefinitely.")
   (nr-allocated-octets :initform 0
                        :accessor nr-allocated-octets
                        :documentation "The number of octets that have been
allocated by ALLOCATE-BLOCK since the last time that RESET-ALLOCATION-COUNTER
was called.")))
                        


;;
;; Open/close/initialize
;;

(defun open-heap (pathname
                  &key (class 'heap) rucksack (options '())
                  (if-exists :overwrite) (if-does-not-exist :create))
  (let ((stream (open pathname
                      :element-type '(unsigned-byte 8)
                      :direction :io
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist
                      #+openmcl :sharing #+openmcl :external)))
    (apply #'make-instance
           class
           :stream stream
           :rucksack rucksack
           options)))


(defmethod close-heap ((heap heap))
  (close (heap-stream heap)))

(defmethod finish-heap-output ((heap heap))
  (finish-output (heap-stream heap)))


(defmethod heap-size ((heap heap))
  (- (heap-end heap) (heap-start heap)))

;;
;; Pointers
;;

(defun pointer-value (pointer heap)
  (file-position (heap-stream heap) pointer)
  (read-unsigned-bytes (cell-buffer heap) (heap-stream heap)
                       +pointer-size+))

(defun (setf pointer-value) (value pointer heap)
  (file-position (heap-stream heap) pointer)
  (write-unsigned-bytes value (cell-buffer heap) (heap-stream heap)
                        +pointer-size+)
  value)

;;
;; Expanding the heap
;;

(defmethod expand-heap ((heap heap) block-size)
  ;; Creates (and initializes) a block of the specified size by expanding
  ;; the heap.  The block is not hooked into the free list yet.  Returns
  ;; the new block (but signals a continuable error if expanding the heap
  ;; would make it exceed its maximum size.
  (let ((new-block (heap-end heap))
        (max-size (max-heap-size heap)))
    (when (and max-size (> (+ new-block block-size) max-size))
      (cerror "Ignore the maximum heap size and expand the heap anyway."
              (format nil
                      "Can't expand the heap with ~D octets: it would grow beyond
the specified maximum heap size of ~D octets."
                      block-size
                      max-size)))
    ;;
    (incf (heap-end heap) block-size)
    ;; Initialize and return the new block.
    (initialize-block new-block block-size heap)
    new-block))

;;
;; Keeping track of allocations
;;

(defmethod allocate-block :around ((heap heap) &key &allow-other-keys)
  (multiple-value-bind (block nr-octets)
      (call-next-method)
    (incf (nr-allocated-octets heap) nr-octets)
    (values block nr-octets)))

(defmethod reset-allocation-counter ((heap heap))
  ;; Resets the allocation counter (and returns the old value of the counter).
  (let ((old-value (nr-allocated-octets heap)))
    (setf (nr-allocated-octets heap) 0)
    old-value))

(defmacro with-allocation-counter ((heap) &body body)
  (let ((heap-var (gensym "HEAP"))
        (old-counter (gensym "COUNTER")))
    `(let* ((,heap-var ,heap)
            (,old-counter (reset-allocation-counter ,heap-var)))
       (unwind-protect (progn ,@body)
         (setf (nr-allocated-octets ,heap-var) ,old-counter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Free list heap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass free-list-heap (heap)
  ((nr-free-lists :initarg :nr-free-lists :initform 32 :reader nr-free-lists)
   (starts :documentation "An array with the starts of each free-list.  This
is an in-memory version of the array that's in the beginning of the heap.")
   (min-block-size :initarg :min-block-size
                   :initform 16 :reader min-block-size
                   :documentation "The size of the smallest blocks.  This must
be a power of 2.")
   (expansion-size :initarg :expansion-size
                   :initform (* 32 1024) :reader expansion-size
                   :documentation "The minimum number of bytes that will be
used to expand a free-list."))

  (:documentation "This heap uses a 'segregated free list' system: the
first list contains 16-octet blocks (including the header), the second
list contains 32-octet blocks, the third has 64-octet blocks, etc.  When
there are N free lists, the last is for blocks of 16*2^(N-1) octets.

Each block starts with an 8-octet header.  If a block is in use, the
header contains the block's size.  If a block is still free, the header
contains a pointer to the next block on the same free list."))


(defmethod initialize-instance :after ((heap free-list-heap)
                                       &key &allow-other-keys)
  ;; Initialize the heap end.
  (if (zerop (file-length (heap-stream heap)))
      (setf (heap-end heap) +pointer-size+)
    (setf (slot-value heap 'end) (pointer-value 0 heap)))
  ;; Load or create the array of free list pointers.
  (setf (slot-value heap 'starts)
        (make-array (nr-free-lists heap)))
  (cond ((< (heap-end heap) (heap-start heap))
         ;; The free list array doesn't exist yet: create free lists.
         ;; Initialize the free list array by letting the free-list pointers
         ;; point to themselves (meaning that the free list is empty).
         (loop for size-class below (nr-free-lists heap)
               do (setf (free-list-start heap size-class)
                        (free-list-pointer size-class)))
         ;; Set heap-end just after the free list array.
         (setf (heap-end heap) (heap-start heap)))
        (t
         ;; Heap exists: load free lists.
         (let ((array (slot-value heap 'starts)))
           (loop for size-class below (nr-free-lists heap)
                 do (setf (aref array size-class)
                          (pointer-value (free-list-pointer size-class)
                                         heap)))))))
           
(defun free-list-pointer (size-class)
  "Returns a pointer to the cell containing the free list start."
  (+ +pointer-size+ ; skip heap end cell
     (* size-class +pointer-size+)))


(defmethod heap-start ((heap free-list-heap))
  ;; A free-list-heap starts with an array of pointers to the first element
  ;; of each free list; the heap blocks start after that array.
  (free-list-pointer (nr-free-lists heap)))

(defmethod (setf heap-end) :after (end (heap free-list-heap))
  ;; Store the heap end in the file.
  (setf (pointer-value 0 heap) end))

;;
;;

(defmethod size-class (size (heap free-list-heap))
  "Returns the (zero-indexed) number of a free-list that has blocks
with sizes at least as big as the specified size."
  ;; Assuming a min-block-size of 16, we want:
  ;; - class 0 for blocks of 1..16
  ;; - class 1 for blocks of 17..32
  ;; - class 2 for blocks of 33..64
  ;; - etc.
  ;; So we subtract 1, shift right by 3 and then look at the most
  ;; significant 1 bit.
  (integer-length (ash (1- size)
                       (- 1 (integer-length (min-block-size heap))))))

(defmethod size-class-block-size (size-class (heap free-list-heap))
  (* (min-block-size heap) (ash 1 size-class)))

;;
;;

(defmethod free-list-start ((heap free-list-heap) &optional (size-class 0))
  "Returns the first block on the free list of the specified size class."
  (aref (slot-value heap 'starts) size-class))

(defmethod (setf free-list-start) (pointer (heap free-list-heap)
                                           &optional (size-class 0))
  (setf (pointer-value (free-list-pointer size-class) heap) pointer
        ;; Keep copy in memory
        (aref (slot-value heap 'starts) size-class) pointer))

(defmethod free-list-empty-p (size-class (heap free-list-heap))
  ;; A free list is empty when the start points to itself.
  (let ((start (free-list-start heap size-class)))
    (= start (free-list-pointer size-class))))

;;
;;

(defmethod block-header-size ((heap free-list-heap))
  +pointer-size+)

(defmethod block-header (block (heap free-list-heap))
  (pointer-value block heap))

(defmethod (setf block-header) (value block (heap free-list-heap))
  (setf (pointer-value block heap) value))

(defmethod (setf block-size) (size block (heap free-list-heap))
  (setf (block-header block heap) size))

(defgeneric block-size (block heap)
  (:documentation "Returns the size of the block starting at the
specified position.  This includes the size of the block header."))

(defmethod block-size (block (heap free-list-heap))
  ;; Actually, the header only contains the block size when
  ;; the block is occupied.
  (block-header block heap))


;;
;; Allocating and deallocating blocks
;;

(defmethod allocate-block ((heap free-list-heap) 
                           &key (size (min-block-size heap)) (expand t))
  ;; We don't bother to do something with the unused part of the block.
  ;; Each block will be at least half full anyway (otherwise a block
  ;; from another free list would have been allocated).  On average,
  ;; I suppose each block will be 75% full. It would be possible to
  ;; give the remaining 25% to a free list of a lower size class, but
  ;; I'm not sure that is worth the extra complexity (or the extra time).
  (let* ((size-class (size-class size heap))
         (block (free-list-start heap size-class)))
    ;; Expand free list when it's empty.
    (when (free-list-empty-p size-class heap)
      (if expand
          (setq block (expand-free-list size-class heap))
        (return-from allocate-block
          (values nil 0))))
    ;; Unhook the block from the free list
    ;; (the block header of an unused block contains a pointer to the
    ;; next unused block).
    (let ((next-block (block-header block heap)))
      (setf (free-list-start heap size-class) next-block))
    ;; Put block size (including the size of header and unused part)
    ;; into header.
    (setf (block-size block heap) (size-class-block-size size-class heap))
    ;; Return the block.
    (values block size)))


(defmethod deallocate-block (block (heap free-list-heap))
  ;; Push the block on the front of its free list.
  (let* ((size (block-size block heap))
         (size-class (size-class size heap)))
    (if (free-list-empty-p size-class heap)
        ;; Let free list start point to the block and vice versa.
        (setf (block-header block heap) (free-list-pointer size-class)
              (free-list-start heap size-class) block)
      ;; Normal case: let free list start point to the block,
      ;; the block to the old block that the free list start pointed to.
      (let ((old-first-block (free-list-start heap size-class)))
        (setf (block-header block heap) old-first-block
              (free-list-start heap size-class) block)))
    ;;
    (initialize-block block size heap)))


;;
;; Expanding free lists
;;

(defmethod expand-free-list (size-class (heap free-list-heap))
  ;; Try to find a block that's at least EXPANSION-SIZE big on
  ;; one of the bigger free lists.  If there is such a block,
  ;; carve it up.  If there isn't, expand the heap if possible.
  (let ((min-size
         (if (< (1+ size-class) (nr-free-lists heap))
             (max (expansion-size heap)
                  ;; Make sure we only try bigger free lists than
                  ;; the current one.
                  (size-class-block-size (1+ size-class) heap))
           (expansion-size heap))))
    (multiple-value-bind (block size)
        (find-block min-size heap)
      (unless block
        (setq size (max (expansion-size heap)
                        (size-class-block-size size-class heap))
              block (expand-heap heap size)))
      (carve-up-block-for-free-list size-class block size heap)
      ;; Return the first new block.
      block)))

(defmethod find-block (min-size (heap free-list-heap))
  ;; Tries to find a block of a size that's at least the specified
  ;; minimum size.  If there is such a block, the block and the
  ;; block's size are returned.  Otherwise it returns nil.
  (let ((size-class (size-class min-size heap)))
    (loop for size-class from size-class below (nr-free-lists heap)
          do (let ((block (allocate-block heap :size min-size :expand nil)))
               (when block
                 (return (values block
                                 (size-class-block-size size-class heap))))))))


(defmethod carve-up-block-for-free-list (size-class block size
                                                    (heap free-list-heap))
  "Carves up a block of the given size to build a free list for the
specified size-class.  Returns the first block of the created free
list."
  (let* ((sub-block-size (size-class-block-size size-class heap))
         (nr-sub-blocks (floor size sub-block-size)))
    ;; Create sub-blocks, each pointing to the next.
    (loop for i below (1- nr-sub-blocks)
          for sub-block from block by sub-block-size
          do (let ((next-sub-block (+ sub-block sub-block-size)))
               ;; Let the sub-block point to its neighbour.
               (setf (block-header sub-block heap) next-sub-block)
               (initialize-block sub-block sub-block-size heap)))
    ;; Let the last sub-block point to the start of the free list.
    (let ((last-block (+ block (* sub-block-size (1- nr-sub-blocks)))))
      (setf (block-header last-block heap) (free-list-pointer size-class))
      (initialize-block last-block sub-block-size heap))
    ;;
    block))


(defgeneric initialize-block (block block-size heap)
  ;; This is used by mark and sweep collectors to write something into
  ;; the block that distinguishes free blocks from occupied blocks.
  (:method (block block-size (heap free-list-heap))
   ;; Default: do nothing
   (declare (ignore block-size))
   block))

;;
;; Heap info
;;

(defmethod heap-info ((heap free-list-heap))
  ;; Returns the total number of free octets in the heap.
  ;; As a second value it returns a list with, for each free list
  ;; that is not empty, a plist with info about that free list.
  (let* ((info (loop for size-class below (nr-free-lists heap)
                     unless (free-list-empty-p size-class heap)
                     collect (free-list-info size-class heap)))
         (total (loop for plist in info
                      sum (or (getf plist :nr-free-octets) 0))))
    (values total info)))


(defmethod free-list-info (size-class (heap free-list-heap))
  (let ((start (free-list-pointer size-class))
        (block-size (size-class-block-size size-class heap)))
    #+debug (format t "~%~%Block size ~D: " block-size)
    (loop for block = start then (block-header block heap)
          #+debug do #+debug (format t "#x~X " block)
          until (eql (block-header block heap) start)
          sum 1 into nr-blocks
          sum block-size into total-block-size
          finally (return (list :block-size block-size
                                :nr-free-blocks nr-blocks
                                :nr-free-octets total-block-size)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple free list heap with fixed size blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-free-list-heap (free-list-heap)
  ()
  (:default-initargs
   :nr-free-lists 1
   :min-block-size 16)
  (:documentation "All blocks in a simple free list heap have the same size."))

(defmethod block-size (block (heap simple-free-list-heap))
  ;; Don't bother to actually read it.  All blocks have the same size anyway.
  (declare (ignore block))
  (min-block-size heap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appending heap (as used by copying garbage collector)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass appending-heap (heap)
  ;; For an APPENDING-HEAP, all writes take place to the heap's end.
  ;; The last 7 octets of the file always contain a serialized version
  ;; of the heap's end.
  ())

(defmethod allocate-block ((heap appending-heap) &key size &allow-other-keys)
  (let ((block (heap-end heap)))
    ;; Put block size (including the size of header) into header.
    (setf (block-size block heap) size)
    ;;
    (incf (heap-end heap) size)
    (values block size)))

(defmethod (setf heap-end) :after (end (heap appending-heap))
  (let ((stream (heap-stream heap)))
    (file-position stream end)
    ;; Write new end to the end of the file.
    (serialize-marker +positive-byte-48+ stream)
    (serialize-byte-48 end stream)))

(defmethod heap-start ((heap appending-heap))
  0)

(defmethod load-heap-end ((heap appending-heap))
  (let* ((stream (heap-stream heap))
         ;; 7 octets: one for a marker, 6 for a byte-48.
         (pos (- (file-length stream) 7)))
    (file-position stream pos)
    (let ((end (deserialize stream)))
      (unless (= end pos)
        (error "Heap may be corrupt (heap-end info is missing."))
      (setf (slot-value heap 'end) end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Little utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun write-unsigned-bytes (integer buffer stream
                                     &optional (nr-octets +pointer-size+))
  (declare (ignore buffer nr-octets))
  (serialize integer stream))


(defun read-unsigned-bytes (buffer stream
                                   &optional (nr-octets +pointer-size+))
  (declare (ignore buffer nr-octets))
  (deserialize stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-buffer-size* 
  (* 64 1024)
  "The number of octets that are allocated initially for a buffer.")

(defclass buffer ()
  ((contents :initform (make-array *default-buffer-size*
                                   :adjustable t
                                   :fill-pointer 0
                                   :element-type '(unsigned-byte 8))
             :accessor contents)))

(defmethod buffer-count ((buffer buffer))
  ;; The number of relevant octets in the buffer.
  (fill-pointer (contents buffer)))

(defmethod reset-buffer ((buffer buffer))
  (setf (fill-pointer (contents buffer)) 0))

;;
;; Serialization buffer
;;

(defclass serialization-buffer (buffer)
  ((scan-pointer :initform 0
                 :accessor scan-pointer)))

(defmethod serialize-byte (byte (stream serialization-buffer))
  (vector-push-extend byte (contents stream) #.(* 8 1024)))

(defmethod deserialize-byte ((stream serialization-buffer)
                             &optional (eof-error-p t))
  (with-slots (contents scan-pointer)
      stream
    (if (< scan-pointer (length contents))
        (let ((result (aref contents scan-pointer)))
          (incf scan-pointer)
          result)
      (and eof-error-p
           (error "Unexpected end of serialization buffer at ~D."
                  scan-pointer)))))

(defmethod scan-byte ((stream serialization-buffer) &optional gc)
  (declare (ignore gc))
  (deserialize-byte stream t))

;;
;; Loading/saving buffers
;;

(defmethod save-buffer ((buffer buffer) stream &key file-position)
  (let ((contents (contents buffer)))
    (when file-position
      (file-position stream file-position))
    (write-sequence contents stream :end (buffer-count buffer))))

(defmethod load-buffer ((buffer buffer) stream nr-octets
                        &key file-position eof-error-p)
  (with-slots (contents)
      buffer
    ;; If the buffer isn't big enough, make a bigger buffer.
    ;; We can't use LENGTH instead of ARRAY-DIMENSION, because
    ;; LENGTH looks at the fill pointer instead of the entire
    ;; buffer.
    (when (< (array-dimension contents 0) nr-octets)
      (setf contents
            (make-array nr-octets
                        :adjustable t
                        :fill-pointer 0
                        :element-type '(unsigned-byte 8))))
    ;;
    (when file-position
      (file-position stream file-position))
    (setf (fill-pointer contents) nr-octets)
    (when (and (< (read-sequence contents stream :end nr-octets) nr-octets)
               eof-error-p)
      (error "Unexpected end of file while loading a buffer of ~D octets."
             nr-octets)))
  buffer)

(defmethod load-buffer :after ((buffer serialization-buffer) stream nr-octets
                               &key file-position)
  (declare (ignore stream nr-octets file-position))
  (setf (scan-pointer buffer) 0))


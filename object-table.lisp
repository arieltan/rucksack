;; $Id: object-table.lisp,v 1.5 2008/02/03 12:32:16 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The object table maps object-ids to their (file) positions in the heap.
;;; It's implemented as a simple-free-list-heap, with blocks of 16 octets
;;  (including the header that's used by the free list).
;;; Each block contains the object's heap position, plus an extra octet
;;; for stuff like garbage collection info (e.g. a mark bit).


(defclass object-table (simple-free-list-heap)
  ()
  (:documentation "A file mapping object identifiers to their file-positions in
the 'real' heap."))

(defmethod initialize-block (block block-size (object-table object-table))
  ;; Initialize a free block.
  ;; Put a marker in the start of the block to show that the block belongs
  ;; to the free list.
  (declare (ignore block-size))
  (setf (object-info object-table (block-to-object-id block object-table))
        :free-block))

(defun open-object-table (pathname &key (if-exists :overwrite)
                                   (if-does-not-exist :create))
  (open-heap pathname
             :class 'object-table
             :if-exists if-exists
             :if-does-not-exist if-does-not-exist))


(defun close-object-table (object-table)
  (close-heap object-table))

;;
;; Mappings blocks to/from object ids.
;;

(defun block-to-object-id (block object-table)
  (floor (- block (heap-start object-table))
         (min-block-size object-table)))

(defun object-id-to-block (id object-table)
  (+ (heap-start object-table)
     (* id (min-block-size object-table))))

;;
;; Creating/deleting object ids.
;;

(defun new-object-id (object-table)
  "Returns an OBJECT-ID that is not in use."
  (let* ((block (allocate-block object-table :expand t))
         (id (block-to-object-id block object-table)))
    (setf (object-info object-table id) :reserved)
    id))

(defun delete-object-id (object-table object-id)
  "Returns object-id's cell to the free-list."
  (deallocate-block (object-id-to-block object-id object-table)
                    object-table))

;;
;; Heap-position and object-info
;;

;; The heap-position is in the least significant octets of an object-table cell.
;; The other object-info is in the most significant octet(s).

(defconstant +nr-object-info-octets+ 1)
(defconstant +nr-object-position-octets+
  ;; We have 7 octets for the serialized heap position.
  ;; The first of those octets will be an integer marker (for the
  ;; serializer); that leaves 6 octets for the actual heap position.
  ;; So the max heap size is 2^48 = 256 terabytes.
  (- +pointer-size+ +nr-object-info-octets+))

(defun (setf object-heap-position) (position object-table id)
  (let ((stream (heap-stream object-table)))
    (file-position stream
                   (+ (block-header-size object-table)
                      +nr-object-info-octets+
                      (object-id-to-block id object-table)))
    (serialize position stream))
  position)

(defun object-heap-position (object-table id)
  (let ((stream (heap-stream object-table)))
    (file-position stream
                   (+ (block-header-size object-table)
                      +nr-object-info-octets+
                      (object-id-to-block id object-table)))
    (deserialize stream)))


(defun object-info (object-table id)
  "Returns either :free-block, :dead-object, :live-object or :reserved."
  (let ((stream (heap-stream object-table)))
    (file-position stream
                   (+ (block-header-size object-table)
                      (object-id-to-block id object-table)))
    (deserialize stream)))


(defun (setf object-info) (info object-table id)
  (let ((stream (heap-stream object-table)))
    (file-position stream
                   (+ (block-header-size object-table)
                      (object-id-to-block id object-table)))
    (let ((marker (ecase info
                    (:free-block +free-block+)
                    (:dead-object +dead-object+)
                    (:live-object +live-object+)
                    (:reserved +reserved-object+))))
      (serialize-marker marker stream)))
  info)


;;
;; Size of object table.
;;

(defun object-table-size (object-table)
  "Returns the potential number of objects in an object-table.
The first potential object-id is number 0."
  (floor (heap-size object-table) (min-block-size object-table)))


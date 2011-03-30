;; $Id: serialize.lisp,v 1.10 2007/01/22 10:23:14 alemmens Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
This is a modified version of my stand-alone serialization library.
The most important modification is that we don't keep track of any
shared objects (like CLOS objects, symbols, struct classes) anymore.
That's supposed to be handled by the database library on top of this.

This file also contains the garbage collection code for scanning objects,
because that's very similar to deserializing them.

What do we do when we serialize an object and it turns out to contain
other objects?  There are a few options:
1. Don't allow it: this should be dealt with at a higher level
2. Automatically add the child object to the cache: that means it
   will be saved and we'll get an object-id for the child.  But what if
   the child was already in the cache?  We have no way of knowing that
   and we'll probably create a mess.
3. Just serialize the contents.  This basically assumes that this is the
   only reference to this objects; or, if it isn't, that it doesn't matter
   if we create more than one copy of this object when we deserialize
   it (and that object identity is irrelevant).
I think I'll go for option 3.
|#

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric save-slots (object serializer))
(defgeneric load-slots (object serializer))

(defmethod saved-slots (object)
  ;; Default: use the MOP to return a list of the names all effective slots.
  (mapcar #'slot-definition-name
    #+lispworks(clos:class-effective-slots (class-of object))
    #-lispworks(class-slots (class-of object))))


(defun save-objects (objects pathname)
  "Saves a list with objects to a file, creating the file if necessary.
If the file exists, it will be superseded."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((serializer (make-instance 'serializer :stream stream)))
      (serialize-list objects serializer))))

(defun load-objects (pathname)
  "Returns a list of objects from a file created by SAVE-OBJECTS."
  (with-open-file (stream pathname
                          :element-type '(unsigned-byte 8)
                          :direction :input)
    (let ((serializer (make-instance 'serializer :stream stream)))
      (deserialize-list serializer))))


(defun open-serializer (stream)
  "Creates and returns a serializer for a stream. The stream must have
element-type (UNSIGNED-BYTE 8))."
  (make-instance 'serializer :stream stream))

(defun close-serializer (serializer &key abort)
  (close (serializer-stream serializer) :abort abort))

(defun force-serializer-output (serializer)
  (force-output (serializer-stream serializer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +illegal-marker+ 0
  "This should never be read as a marker.")

(defconstant +ignore+ 1
  "This marker is automatically skipped when read.  Handy if you need
fixed width fields.")

;; Booleans

(defconstant +nil+ 2)
(defconstant +t+ 3)

;; Integers

(defconstant +minus-one+ #x09)
(defconstant +zero+      #x0A)
(defconstant +one+       #x0B)
(defconstant +two+       #x0C)


(defconstant +positive-byte-8+  #x10)
(defconstant +negative-byte-8+  #x11)
(defconstant +positive-byte-16+ #x12)
(defconstant +negative-byte-16+ #x13)
(defconstant +positive-byte-24+ #x14)
(defconstant +negative-byte-24+ #x15)
(defconstant +positive-byte-32+ #x16)
(defconstant +negative-byte-32+ #x17)
(defconstant +positive-byte-48+ #x18)
(defconstant +negative-byte-48+ #x19)
(defconstant +positive-byte-64+ #x1A)
(defconstant +negative-byte-64+ #x1B)
(defconstant +positive-integer+ #x1C)
(defconstant +negative-integer+ #x1D)


;; Other numbers

(defconstant +rational+     #x20)
(defconstant +float+        #x21)
(defconstant +short-float+  #x22)
(defconstant +single-float+ #x23)
(defconstant +double-float+ #x24)
(defconstant +long-float+   #x25)
(defconstant +complex+      #x26)

;; Strings and characters

(defconstant +character+    #x30)   ; also used as element-type marker for strings
(defconstant +character-8+  #x31)
(defconstant +character-16+ #x32)
(defconstant +character-24+ #x33)
(defconstant +character-32+ #x34)

(defconstant +base-char+     #x35)  ; used as element-type marker for strings   
(defconstant +extended-char+ #x36)  ; used as element-type marker for strings

(defconstant +string+           #x40)
(defconstant +string-8+         #x41)
(defconstant +string-16+        #x42)
(defconstant +string-24+        #x43)
(defconstant +string-32+        #x44)
(defconstant +simple-string+    #x45)
(defconstant +simple-string-8+  #x46)
(defconstant +simple-string-16+ #x47)
(defconstant +simple-string-24+ #x48)
(defconstant +simple-string-32+ #x49)

;; Symbols and packages

(defconstant +symbol+            #x50)
(defconstant +keyword+           #x51)
(defconstant +uninterned-symbol+ #x52)
(defconstant +symbol-reference+  #x53)
(defconstant +package+           #x54)


;; Lists, conses, structures

(defconstant +cons+              #x60)
(defconstant +proper-list+       #x61)
(defconstant +struct+            #x62)
(defconstant +struct-definition+ #x63)
(defconstant +dotted-list+       #x64)

;; Objects and slots 

(defconstant +object+                   #x70)
(defconstant +unbound-slot+             #x71)
(defconstant +shared-object-definition+ #x72)
(defconstant +shared-object-reference+  #x73)
(defconstant +structure-object+         #x77)

;; Rest

(defconstant +hash-table+ #x80)
(defconstant +pathname+  #x90)
(defconstant +array+     #xA0)

;; Garbage collector marks
(defconstant +free-block+  #xB0)
(defconstant +live-object+ #xB1)
(defconstant +dead-object+ #xB2)
(defconstant +reserved-object+ #xB3
  "Used for entries in the object table that belong to objects that haven't
been committed to disk yet.")

(defconstant +extension-0+ #xC0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass serializer ()
  ((stream :initarg :stream :reader serializer-stream
           :documentation "An (unsigned-byte 8) stream.")))

(defgeneric serialize-byte (byte serializer)
  (:documentation "Writes an unsigned-byte to a serializer.")
  (:method ((byte integer) (serializer serializer))
   (write-byte byte (serializer-stream serializer)))
  (:method ((byte integer) (stream stream))
   (write-byte byte stream)))

(defgeneric deserialize-byte (serializer &optional eof-error-p)
  (:documentation "Reads an unsigned-byte from a serializer.  EOF-ERROR-P
defaults to T.")
  (:method ((serializer serializer) &optional (eof-error-p t))
   (read-byte (serializer-stream serializer) eof-error-p nil))
  (:method ((stream stream) &optional (eof-error-p t))
   (read-byte stream eof-error-p nil)))

(defgeneric scan-byte (serializer &optional gc)
  (:documentation "Skips an unsigned byte from the serializer.")
  (:method ((serializer serializer) &optional gc)
   (declare (ignore gc))
   (read-byte (serializer-stream serializer) t nil))
  (:method ((stream stream) &optional gc)
   (declare (ignore gc))
   (read-byte stream t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERIALIZE/DESERIALIZE/SCAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialize (object serializer)
  (:documentation "Writes a serialized version of an object to the
stream in a serializer."))

(defgeneric scan-contents (marker serializer garbage-collector))

(defmethod scan-contents (marker serializer gc)
  ;; Default: just deserialize the contents but don't evacuate anything.
  (declare (ignore gc))
  (deserialize-contents marker serializer))


(defun serialize-marker (marker serializer)
  (serialize-byte marker serializer))

(defun read-next-marker (serializer)
  "Returns the next marker (or NIL if we're at the end of the
serializer stream)."
  (loop (let ((marker (deserialize-byte serializer nil)))
          (if (null marker)
              (return nil)
            (unless (eql marker +ignore+)
              (return marker))))))


(defun deserialize (serializer &optional (eof-error-p t) (eof-value nil))
  "Reads the next object from the serializer stream.  Signals an end-of-file
error or returns EOF-VALUE when the end of the stream is reached."
  (let ((marker (read-next-marker serializer)))
    (if marker
        (deserialize-contents marker serializer)
      ;; End of file
      (if eof-error-p
          (error 'end-of-file :stream serializer)
        eof-value))))

(defun serialize-list (list stream &optional (length (length list)))
  "Serializes a proper list by first serializing its length and then all the
elements of the list."
  (serialize length stream)
  (dolist (elt list)
    (serialize elt stream)))

(defun deserialize-list (stream)
  (let ((length (deserialize stream)))
    (loop repeat length
          collect (deserialize stream))))


(defun serialize-dotted-list (list stream &optional (length (length list)))
  "Serializes a dotted list by first serializing its length and then all the
elements of the list."
  (serialize length stream)
  (loop for elt on list do
        (serialize (car elt) stream)
        (when (atom (cdr elt))
          ;; The last element
          (serialize (cdr elt) stream))))

(defun deserialize-dotted-list (stream)
  "Serializes a dotted list by first serializing its length and then all the
elements of the list."
  ;; EFFICIENCY: This walks the list one more time to add the final element.
  ;; That should be optimized.
  (let* ((length (deserialize stream))
         (list (loop repeat (1- length)
                     collect (deserialize stream)))
         (final-elt (deserialize stream)))
    (setf (cdr (last list)) final-elt)
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Illegal marker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod deserialize-contents ((marker (eql +illegal-marker+)) stream)
  (cerror "Ignore the marker and continue."
          "There's an illegal marker in stream ~A."
          stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((object (eql nil)) stream)
  (serialize-marker +nil+ stream))

(defmethod serialize ((object (eql t)) stream)
  (serialize-marker +t+ stream))

(defmethod deserialize-contents ((marker (eql +nil+)) stream)
  (declare (ignore stream))
  nil)

(defmethod deserialize-contents ((marker (eql +t+)) stream)
  (declare (ignore stream))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Serializing multiple bytes
;;

(defun serialize-byte-16 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream))

(defun serialize-byte-24 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream)
  (serialize-byte (ldb (byte 8 16) integer) stream))

(defun serialize-byte-32 (integer stream)
  (serialize-byte (ldb (byte 8 0) integer) stream)
  (serialize-byte (ldb (byte 8 8) integer) stream)
  (serialize-byte (ldb (byte 8 16) integer) stream)
  (serialize-byte (ldb (byte 8 24) integer) stream))

(defun serialize-byte-48 (integer stream)
  (multiple-value-bind (most-significant least-significant)
      (floor integer #x1000000)
    (serialize-byte-24 least-significant stream)
    (serialize-byte-24 most-significant stream)))

(defun serialize-byte-64 (integer stream)
  (multiple-value-bind (most-significant least-significant)
      (floor integer #x100000000)
    (serialize-byte-32 least-significant stream)
    (serialize-byte-32 most-significant stream)))


;;
;; Deserializing multiple bytes
;;

(defun deserialize-byte-16 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) 256)))

(defun deserialize-byte-24 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) #x100)
     (* (deserialize-byte stream) #x10000)))

(defun deserialize-byte-32 (stream)
  (+ (deserialize-byte stream)
     (* (deserialize-byte stream) #x100)
     (* (deserialize-byte stream) #x10000)
     (* (deserialize-byte stream) #x1000000)))

(defun deserialize-byte-48 (stream)
  (+ (deserialize-byte-24 stream)
     (* (deserialize-byte-24 stream) #x1000000)))

(defun deserialize-byte-64 (stream)
  (+ (deserialize-byte-32 stream)
     (* (deserialize-byte-32 stream) #x100000000)))

;;
;; Scanning multiple bytes
;;

(defun scan-byte-16 (stream &optional gc)
  (declare (ignore gc))
  (scan-byte stream)
  (scan-byte stream))

(defun scan-byte-24 (stream &optional gc)
  (declare (ignore gc))
  (dotimes (i 3)
    (scan-byte stream)))

(defun scan-byte-32 (stream &optional gc)
  (declare (ignore gc))
  (scan-byte-16 stream)
  (scan-byte-16 stream))

(defun scan-byte-48 (stream &optional gc)
  (declare (ignore gc))
  (scan-byte-24 stream)
  (scan-byte-24 stream))

(defun scan-byte-64 (stream &optional gc)
  (declare (ignore gc))
  (scan-byte-32 stream)
  (scan-byte-32 stream))


;;
;; Serializing integers
;;

(defmethod serialize ((obj integer) stream)
  ;; Serialize integers with least-significant bytes first.
  (cond ((zerop obj) (serialize-marker +zero+ stream))
        ((= obj 1) (serialize-marker +one+ stream))
        ((= obj -1) (serialize-marker +minus-one+ stream))
        ((= obj 2) (serialize-marker +two+ stream))
        (t (let* ((positive-p (>= obj 0) )
                  (unsigned (abs obj))
                  (nr-octets (nr-octets unsigned)))
             (serialize-integer positive-p unsigned nr-octets stream)))))

(defun serialize-integer (positive-p unsigned nr-octets stream)
  (case nr-octets
    (1 (serialize-marker (if positive-p +positive-byte-8+ +negative-byte-8+)
                         stream)
       (serialize-byte unsigned stream))
    (2 (serialize-marker (if positive-p +positive-byte-16+ +negative-byte-16+)
                         stream)
       (serialize-byte-16 unsigned stream))
    (3 (serialize-marker (if positive-p +positive-byte-24+ +negative-byte-24+)
                         stream)
       (serialize-byte-24 unsigned stream))
    (4 (serialize-marker (if positive-p +positive-byte-32+ +negative-byte-32+)
                         stream)
       (serialize-byte-32 unsigned stream))
    ((5 6)
     (serialize-marker (if positive-p +positive-byte-48+ +negative-byte-48+)
                       stream)
     (serialize-byte-48 unsigned stream))
    ((7 8)
     (serialize-marker (if positive-p +positive-byte-64+ +negative-byte-64+)
                       stream)
     (serialize-byte-64 unsigned stream))
    (otherwise
     (let ((nr-bits (* 8 nr-octets)))
       (serialize-marker (if positive-p +positive-integer+ +negative-integer+)
                         stream)
       (serialize nr-octets stream)
       (loop for position from (- nr-bits 8) downto 0 by 8
             do (serialize-byte (ldb (byte 8 position) unsigned) stream))))))


;;
;; Scanning integers
;;

(defmethod scan-contents ((marker (eql +positive-byte-8+)) stream gc)
  (declare (ignore gc))
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +negative-byte-8+)) stream gc)
  (declare (ignore gc))
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +positive-byte-16+)) stream gc)
  (declare (ignore gc))
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +negative-byte-16+)) stream gc)
  (declare (ignore gc))
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +positive-byte-24+)) stream gc)
  (declare (ignore gc))
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +negative-byte-24+)) stream gc)
  (declare (ignore gc))
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +positive-byte-32+)) stream gc)
  (declare (ignore gc))
  (scan-byte-32 stream))

(defmethod scan-contents ((marker (eql +negative-byte-32+)) stream gc)
  (declare (ignore gc))
  (scan-byte-32 stream))

(defmethod scan-contents ((marker (eql +positive-byte-48+)) stream gc)
  (declare (ignore gc))
  (scan-byte-48 stream))

(defmethod scan-contents ((marker (eql +negative-byte-48+)) stream gc)
  (declare (ignore gc))
  (scan-byte-48 stream))

(defmethod scan-contents ((marker (eql +positive-byte-64+)) stream gc)
  (declare (ignore gc))
  (scan-byte-64 stream))

(defmethod scan-contents ((marker (eql +negative-byte-64+)) stream gc)
  (declare (ignore gc))
  (scan-byte-64 stream))

(defmethod scan-contents ((marker (eql +positive-integer+)) stream gc)
  (declare (ignore gc))
  (let ((nr-bytes (deserialize stream)))
    (assert (integerp nr-bytes))
    (dotimes (i nr-bytes)
      (scan-byte stream))))

(defmethod scan-contents ((marker (eql +negative-integer+)) stream gc)
  (scan-contents +positive-integer+ stream gc))


;;
;; Deserializing integers
;;

(defun nr-octets (n)
  (ceiling (integer-length n) 8))

(defmethod deserialize-contents ((marker (eql +minus-one+)) stream)
  (declare (ignore stream))
  -1)

(defmethod deserialize-contents ((marker (eql +zero+)) stream)
  (declare (ignore stream))
  0)

(defmethod deserialize-contents ((marker (eql +one+)) stream)
  (declare (ignore stream))
  1)

(defmethod deserialize-contents ((marker (eql +two+)) stream)
  (declare (ignore stream))
  2)

(defmethod deserialize-contents ((marker (eql +positive-byte-8+)) stream)
  (deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-8+)) stream)
  (- (deserialize-byte stream)))

(defmethod deserialize-contents ((marker (eql +positive-byte-16+)) stream)
  (deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-16+)) stream)
  (- (deserialize-byte-16 stream)))

(defmethod deserialize-contents ((marker (eql +positive-byte-24+)) stream)
  (deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-24+)) stream)
  (- (deserialize-byte-24 stream)))

(defmethod deserialize-contents ((marker (eql +positive-byte-32+)) stream)
  (deserialize-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-32+)) stream)
  (- (deserialize-byte-32 stream)))

(defmethod deserialize-contents ((marker (eql +positive-byte-48+)) stream)
  (deserialize-byte-48 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-48+)) stream)
  (- (deserialize-byte-48 stream)))

(defmethod deserialize-contents ((marker (eql +positive-byte-64+)) stream)
  (deserialize-byte-64 stream))

(defmethod deserialize-contents ((marker (eql +negative-byte-64+)) stream)
  (- (deserialize-byte-64 stream)))


(defmethod deserialize-contents ((marker (eql +positive-integer+)) stream)
  (let ((nr-bytes (deserialize stream)))
    (assert (integerp nr-bytes))
    (let ((result 0))
      (loop for i below nr-bytes
            do (setf result (+ (ash result 8) (deserialize-byte stream))))
      result)))

(defmethod deserialize-contents ((marker (eql +negative-integer+)) stream)
  (- (deserialize-contents +positive-integer+ stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rationals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((number rational) stream)
  (serialize-marker +rational+ stream)
  (serialize (numerator number) stream)
  (serialize (denominator number) stream))

(defmethod deserialize-contents ((marker (eql +rational+)) stream)
  (/ (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +rational+)) stream gc)
  (scan stream gc)
  (scan stream gc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Floats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serialize-float (number stream)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float number)
    (serialize significand stream)
    (serialize exponent stream)
    (serialize sign stream)))

(defmethod serialize ((number float) stream)
  ;; NOTE: This may not be the most compact way, but at least it's portable.
  (serialize-marker +float+ stream)
  (serialize-float number stream))

(defmethod deserialize-contents ((marker (eql +float+)) stream)
  ;; Reconstruct LONG floats (1.0L0) by default.
  (let* ((significand (deserialize stream))
         (exponent (deserialize stream))
         (sign (deserialize stream)))
    (* sign (scale-float (float significand 1.0L0) exponent))))

(defmethod scan-contents ((marker (eql +float+)) stream gc)
  ;; significand, exponent, sign
  (dotimes (i 3)
    (scan stream gc)))


#|
For more efficient ways of serializing floats, we may want to use
something like the following (from Pascal Bourguignon on comp.lang.lisp).
 
(defmacro gen-ieee-encoding (name type exponent-bits mantissa-bits)
  ;; Thanks to ivan4th (~ivan_iv@nat-msk-01.ti.ru) for correcting an off-by-1
  `(progn
    (defun ,(with-standard-io-syntax 
             (intern (format nil "~A-TO-IEEE-754" name)))  (float)
      (multiple-value-bind (mantissa exponent sign) 
          (integer-decode-float float)
        (dpb (if (minusp sign) 1 0)
             (byte 1 ,(1- (+ exponent-bits mantissa-bits)))
             (dpb (+ ,(+ (- (expt 2 (1- exponent-bits)) 2) mantissa-bits)
                     exponent)
                  (byte ,exponent-bits ,(1- mantissa-bits))
                  (ldb (byte ,(1- mantissa-bits) 0) mantissa)))))
    (defun ,(with-standard-io-syntax 
             (intern (format nil "IEEE-754-TO-~A" name)))  (ieee)
      (let ((aval (scale-float (coerce
                                (dpb 1 (byte 1 ,(1- mantissa-bits))
                                     (ldb (byte ,(1- mantissa-bits) 0) ieee))
                                ,type)
                               (- (ldb (byte ,exponent-bits ,(1- mantissa-bits))
                                       ieee) 
                                  ,(1- (expt 2 (1- exponent-bits)))
                                  ,(1- mantissa-bits)))))
        (if (zerop (ldb (byte 1 ,(1- (+ exponent-bits mantissa-bits))) ieee))
            aval
            (- aval))))))

(gen-ieee-encoding float-32 'single-float  8 24)
(gen-ieee-encoding float-64 'double-float 11 53)
|#
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((number complex) stream)
  (serialize-marker +complex+ stream)
  (serialize (realpart number) stream)
  (serialize (imagpart number) stream))

(defmethod deserialize-contents ((marker (eql +complex+)) stream)
  (complex (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +complex+)) stream gc)
  (scan stream gc)
  (scan stream gc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DO: Implement serializing of circular lists.

(defun analyze-list (x)  
  "Returns two values.  The first value is one of :PROPER-LIST,
:DOTTED-LIST or :CIRCULAR-LIST.  The second value is the length of
the list.  For dotted lists, the final item is included in the
length; for circular lists, the length is NIL."
  ;; This is an adapatation of the algorithm in the Hyperspec
  ;; (see LIST-LENGTH).
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast x (cddr fast))             ;Fast pointer: leaps by 2.
       (slow x (cdr slow)))             ;Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (cond ((null fast)
           (return (values :proper-list n)))
          ((atom fast)
           (return (values :dotted-list (+ n 1))))
          ((null (cdr fast))
           (return (values :proper-list (1+ n))))
          ((atom (cdr fast))
           (return (values :dotted-list (+ n 2))))
          ;; If fast pointer eventually equals slow pointer,
          ;;  then we must be stuck in a circular list.
          ;; (A deeper property is the converse: if we are
          ;;  stuck in a circular list, then eventually the
          ;;  fast pointer will equal the slow pointer.
          ;;  That fact justifies this implementation.)
          ((and (eq fast slow) (> n 0))
           (return (values :circular-list nil))))))

(defun test-analyze-list ()
  (flet ((check (list type length)
           (multiple-value-bind (result-type result-length)
               (analyze-list list)
             (assert (and (eql type result-type) (eql length result-length))))))
    (check '(1 2 3) :proper-list 3)
    (check '(1 . 2) :dotted-list 2)
    (check '() :proper-list 0)
    (let ((circular-list (list 1 2 3)))
      (setf (cdr circular-list) circular-list)
      (check circular-list :circular-list nil)))
  'OK)
 

(defmethod serialize ((cons cons) stream)
  (multiple-value-bind (list-type length)
      (analyze-list cons)
    (ecase list-type
      (:proper-list
       (serialize-marker +proper-list+ stream)
       (serialize-list cons stream length))
      (:dotted-list
       (serialize-marker +dotted-list+ stream)
       (serialize-dotted-list cons stream length))
      (:circular-list (error "Serializing circular lists isn't implemented yet.")))))

(defmethod deserialize-contents ((marker (eql +proper-list+)) stream)
  (deserialize-list stream))

(defmethod deserialize-contents ((marker (eql +dotted-list+)) stream)
  (deserialize-dotted-list stream))

(defmethod deserialize-contents ((marker (eql +cons+)) stream)
  (cons (deserialize stream) (deserialize stream)))

(defmethod scan-contents ((marker (eql +proper-list+)) stream gc)
  (scan-list stream gc))

(defmethod scan-contents ((marker (eql +dotted-list+)) stream gc)
  (scan-list stream gc))

(defmethod scan-contents ((marker (eql +cons+)) stream gc)
  ;; Scan car and cdr.
  (scan stream gc)
  (scan stream gc))

(defun scan-list (stream gc)
  ;; Scan all children.
  (let ((length (deserialize stream)))
    (loop repeat length
          do (scan stream gc))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings and characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Serializing characters
;;

(defmethod serialize ((char character) stream)
  (unless (= (char-code char) (char-int char))
    (cerror "Serialize it anyway (without the attributes)."
            "The character ~S can't be serialized because it has
implementation-dependent attributes."
            char))
  (let ((code (char-code char)))
    (cond ((<= code #xFF)
           (serialize-marker +character-8+ stream)
           (serialize-byte code stream))
          ((<= code #xFFFF)
           (serialize-marker +character-16+ stream)
           (serialize-byte-16 code stream))
          ((<= code #xFFFFFF)
           (serialize-marker +character-24+ stream)
           (serialize-byte-24 code stream))
          ((<= code #xFFFFFFFF)
           (serialize-marker +character-32+ stream)
           (serialize-byte-32 code stream))
          (t (serialize-marker +character+ stream)
             (serialize (char-code char) stream)))))

;;
;; Deserializing characters
;;

(defmethod deserialize-contents ((marker (eql +character+)) stream)
  (code-char (deserialize stream)))

(defmethod deserialize-contents ((marker (eql +character-8+)) stream)
  (code-char (deserialize-byte stream)))

(defmethod deserialize-contents ((marker (eql +character-16+)) stream)
  (code-char (deserialize-byte-16 stream)))

(defmethod deserialize-contents ((marker (eql +character-24+)) stream)
  (code-char (deserialize-byte-24 stream)))

(defmethod deserialize-contents ((marker (eql +character-32+)) stream)
  (code-char (deserialize-byte-32 stream)))

;;
;; Scanning characters
;;

(defmethod scan-contents ((marker (eql +character+)) stream gc)
  (scan stream gc))

(defmethod scan-contents ((marker (eql +character-8+)) stream gc)
  (declare (ignore gc))
  (scan-byte stream))

(defmethod scan-contents ((marker (eql +character-16+)) stream gc)
  (declare (ignore gc))
  (scan-byte-16 stream))

(defmethod scan-contents ((marker (eql +character-24+)) stream gc)
  (declare (ignore gc))
  (scan-byte-24 stream))

(defmethod scan-contents ((marker (eql +character-32+)) stream gc)
  (declare (ignore gc))
  (scan-byte-32 stream))


;;
;; Serializing strings
;;

(defun max-character-code (string)
  "Returns the highest character code in string."
  (loop for char across string
        maximize (char-code char)))

(defmethod serialize ((string string) stream)
  ;; A string is serialized as:
  ;; - a string marker (e.g. +simple-string-8+ or +string+)
  ;; - an element-type marker (one of +base-char+, +extended-char+ or +character+)
  ;; - for non-simple strings: the fill-pointer (an integer)
  ;; - for non-simple strings: a boolean telling if it is adjustable
  ;; - a length (integer)
  ;; - the characters.
  ;; We analyze the string to determine the maximum character code, and then
  ;; determine the serialized string type, e.g. +simple-string-8+, +string-16+ 
  ;; or +string+.
  (let ((max-code (max-character-code string))
        (simple-p (simple-string-p string)))
    (multiple-value-bind (marker writer)
        (cond ((<= max-code #xFF) 
               (values (if simple-p +simple-string-8+ +string-8+)
                       #'serialize-byte))
              ((<= max-code #xFFFF) 
               (values (if simple-p +simple-string-16+ +string-16+)
                       #'serialize-byte-16))
              ((<= max-code #xFFFFFF) 
               (values (if simple-p +simple-string-24+ +string-24+)
                       #'serialize-byte-24))
              ((<= max-code #xFFFFFFFF) 
               (values (if simple-p +simple-string-32+ +string-32+)
                       #'serialize-byte-32))
              (t
               (values (if simple-p +simple-string+ +string+)
                       #'serialize)))
      (serialize-marker marker stream)
      ;; Translate the element-type of the string to a marker.
      ;; NOTE: This is not portable between implementations!!
      (let* ((type (array-element-type string))
             (type-marker
              (cond ((subtypep type 'base-char) +base-char+)
                    ((subtypep type 'extended-char) +extended-char+)
                    ((subtypep type 'character) +character+)
                    (t (error "Unrecognized element-type ~S for strings."
                              type)))))
        (serialize-marker type-marker stream))
      (unless simple-p
        (serialize (fill-pointer string) stream)
        (serialize (adjustable-array-p string) stream))
      (serialize (length string) stream)
      (loop for char across string
            for code = (char-code char)
            do (funcall writer code stream)))))


;;
;; Scanning strings
;;

(defmethod scan-contents ((marker (eql +simple-string+)) stream gc)
  (scan-string t #'scan stream gc))

(defmethod scan-contents ((marker (eql +simple-string-8+)) stream gc)
  (scan-string t #'scan-byte stream gc))

(defmethod scan-contents ((marker (eql +simple-string-16+)) stream gc)
  (scan-string t #'scan-byte-16 stream gc))

(defmethod scan-contents ((marker (eql +simple-string-24+)) stream gc)
  (scan-string t #'scan-byte-24 stream gc))

(defmethod scan-contents ((marker (eql +simple-string-32+)) stream gc)
  (scan-string t #'scan-byte-32 stream gc))

(defmethod scan-contents ((marker (eql +string+)) stream gc)
  (scan-string nil #'scan stream gc))

(defmethod scan-contents ((marker (eql +string-8+)) stream gc)
  (scan-string nil #'scan-byte stream gc))

(defmethod scan-contents ((marker (eql +string-16+)) stream gc)
  (scan-string nil #'scan-byte-16 stream gc))

(defmethod scan-contents ((marker (eql +string-24+)) stream gc)
  (scan-string nil #'scan-byte-24 stream gc))

(defmethod scan-contents ((marker (eql +string-32+)) stream gc)
  (scan-string nil #'scan-byte-32 stream gc))

(defun scan-string (simple-p character-code-scanner stream gc)
  (scan-byte stream) ; skip type marker
  (unless simple-p
    ;; fill pointer and adjustable-p
    (scan stream gc)
    (scan stream gc))
  (loop repeat (deserialize stream) ; length
        do (funcall character-code-scanner stream gc)))


;;
;; Deserializing strings
;;

(defmethod deserialize-contents ((marker (eql +simple-string+)) stream)
  (deserialize-string t #'deserialize stream))

(defmethod deserialize-contents ((marker (eql +simple-string-8+)) stream)
  (deserialize-string t #'deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +simple-string-16+)) stream)
  (deserialize-string t #'deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +simple-string-24+)) stream)
  (deserialize-string t #'deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +simple-string-32+)) stream)
  (deserialize-string t #'deserialize-byte-32 stream))

(defmethod deserialize-contents ((marker (eql +string+)) stream)
  (deserialize-string nil #'deserialize stream))

(defmethod deserialize-contents ((marker (eql +string-8+)) stream)
  (deserialize-string nil #'deserialize-byte stream))

(defmethod deserialize-contents ((marker (eql +string-16+)) stream)
  (deserialize-string nil #'deserialize-byte-16 stream))

(defmethod deserialize-contents ((marker (eql +string-24+)) stream)
  (deserialize-string nil #'deserialize-byte-24 stream))

(defmethod deserialize-contents ((marker (eql +string-32+)) stream)
  (deserialize-string nil #'deserialize-byte-32 stream))


(defun deserialize-string (simple-p character-code-reader stream)
  (let* ((type-marker (deserialize-byte stream))
         (type 'character
           ;; I used to distinguish between base-char, extended-char
           ;; and character.  But that broke when I serialized
           ;; Japanese strings with Allegro and tried to deserialize
           ;; them with Lispworks.  Or was it the other way round?
           ;; Anyway: it wasn't portable between implementations, so
           ;; we always use CHARACTER now.
          #+nil(cond ((= type-marker +base-char+) 'base-char)
                     ((= type-marker +extended-char+) 'extended-char)
                     ((= type-marker +character+) 'character)
                     (t (error "Unrecognized type marker ~D for serialized string."
                               type-marker))))
         (fill-pointer (unless simple-p (deserialize stream)))
         (adjustable-p (unless simple-p (deserialize stream)))
         (length (deserialize stream)))
    (declare (ignore type-marker))
    (let ((string (if simple-p
                      (make-string length :element-type type)
                    (make-array length 
                                :element-type type
                                :fill-pointer fill-pointer
                                :adjustable adjustable-p))))
      (loop for i below length
            for code = (funcall character-code-reader stream)
            do (setf (char string i) (code-char code)))
      string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symbols and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EFFICIENCY: We should probably keep some kind of package registry.
;; (Just as we do for symbols, etc.)

(defmethod serialize ((symbol symbol) stream)
  (cond ((keywordp symbol)
         (serialize-marker +keyword+ stream)
         (serialize (symbol-name symbol) stream))
        ((null (symbol-package symbol))
         (serialize-marker +uninterned-symbol+ stream)
         (serialize (symbol-name symbol) stream))
        (t (serialize-marker +symbol+ stream)
           (serialize (package-name (symbol-package symbol)) stream)
           (serialize (symbol-name symbol) stream))))

(defmethod deserialize-contents ((marker (eql +keyword+)) stream)
  (intern (deserialize stream) (find-package :keyword)))

(defmethod scan-contents ((marker (eql +keyword+)) stream gc)
  ;; just the symbol name
  (scan stream gc))


(defmethod deserialize-contents ((marker (eql +uninterned-symbol+)) stream)
  (make-symbol (deserialize stream)))

(defmethod scan-contents ((marker (eql +uninterned-symbol+)) stream gc)
  ;; just the symbol name
  (scan stream gc))


(defmethod deserialize-contents ((marker (eql +symbol+)) stream)
  ;; Q: Maybe we should always create the package if it doesn't exist
  ;; (without even asking?)
  (let ((package-name (deserialize stream))
        (symbol-name (deserialize stream)))
    (let ((package (or (find-package package-name)
                       (cerror "Create the package and continue."
                               "Can't find a package called ~S to intern a symbol with name ~S."
                               package-name
                               symbol-name)
                       (make-package package-name))))
      (intern symbol-name package))))

(defmethod scan-contents ((marker (eql +symbol+)) stream gc)
  ;; package name, then symbol name
  (scan stream gc)
  (scan stream gc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((object standard-object) serializer)
  (serialize-standard-object object serializer))

(defun serialize-standard-object (object serializer)
  ;; A standard object is serialized as:
  ;; - class name
  ;; - number of slots
  ;; - slot values
  (serialize-marker +object+ serializer)
  (serialize (class-name (class-of object)) serializer)
  (save-slots object serializer))

(defmethod save-slots ((object standard-object) serializer)
  (let ((slots (saved-slots object)))
    (serialize (length slots) serializer)
    (loop for slot-name in (saved-slots object)
          do (if (slot-boundp object slot-name)
                 (serialize (slot-value object slot-name) serializer)
               (serialize-marker +unbound-slot+ serializer)))))

(defmethod deserialize-contents ((marker (eql +object+)) serializer)
  (let* ((class-name (deserialize serializer))
         (object (allocate-instance (find-class class-name))))
    (load-slots object serializer)))

(defmethod load-slots ((object standard-object) stream)
  (let ((nr-slots (deserialize stream))
        (slots (saved-slots object)))
    (unless (= nr-slots (length slots))
      (error "Slot mismatch while deserializing a standard object of class ~S."
             (class-of object)))
    (loop for slot-name in (saved-slots object)
          do (let ((marker (read-next-marker stream)))
               (if (eql marker +unbound-slot+)
                   (slot-makunbound object slot-name)
                 (setf (slot-value object slot-name)
                       (deserialize-contents marker stream)))))
    object))


(defmethod scan-contents ((marker (eql +object+)) serializer gc)
  ;; Skip class name
  (scan serializer gc)
  ;; Scan all slots
  (let ((nr-slots (deserialize serializer)))
    (loop repeat nr-slots
          do (scan serializer gc))))


(defmethod scan-contents ((marker (eql +unbound-slot+)) serializer gc)
  ;; Just skip the marker and continue.
  :do-nothing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures
;;;
;;; Can't be serialized portably.  The version below works for SBCL at the
;;; moment, but using structures in Rucksack is risky: if a structure 
;;; definition changes, Rucksack won't know about it and you'll probably
;;; run into big problems.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(defmethod serialize ((object structure-object) serializer)
  (serialize-structure-object object serializer))

(defun serialize-structure-object (object serializer)
  ;; A structure object is serialized as:
  ;; - structure name
  ;; - number of slots
  ;; - slot values
  (serialize-marker +structure-object+ serializer)
  (serialize (class-name (class-of object)) serializer)
  (save-slots object serializer))

(defmethod save-slots ((object structure-object) serializer)
  (let ((slots (saved-slots object)))
    (serialize (length slots) serializer)
    (loop for slot-name in (saved-slots object)
          do (serialize (slot-value object slot-name) serializer))))

#+sbcl
(defmethod deserialize-contents ((marker (eql +structure-object+)) serializer)
  (let* ((class-name (deserialize serializer))
         (object (allocate-instance (find-class class-name))))
    (load-slots object serializer)))

(defmethod load-slots ((object structure-object) stream)
  (let ((nr-slots (deserialize stream))
        (slots (saved-slots object)))
    (unless (= nr-slots (length slots))
      (error "Slot mismatch while deserializing a structure object of class ~S."
             (class-of object)))
    (loop for slot-name in (saved-slots object)
          do (let ((marker (read-next-marker stream)))
               (setf (slot-value object slot-name)
                     (deserialize-contents marker stream))))
    object))

(defmethod scan-contents ((marker (eql +structure-object+)) serializer gc)
  ;; Skip class name
  (scan serializer gc)
  ;; Scan all slots
  (let ((nr-slots (deserialize serializer)))
    (loop repeat nr-slots
      do (scan serializer gc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EFFICIENCY: Use more space-efficient representations for specialized arrays
;; like bit-vectors, arrays of floats, arrays of fixnums, simple-vectors, ...

;; DO: Handle displaced arrays.

(defmethod serialize ((array array) stream)
  (serialize-marker +array+ stream)
  (serialize (array-element-type array) stream)
  (serialize-list (array-dimensions array) stream)  
  (when (= 1 (array-rank array)) 
    ;; Arrays with rank > 1 can't have fill-pointers.
    (serialize (array-has-fill-pointer-p array) stream)
    (when (array-has-fill-pointer-p array)
      (serialize (fill-pointer array) stream)))
  (serialize (adjustable-array-p array) stream)
  (if (array-displacement array)
      (error "Saving displaced arrays isn't implemented yet.")
    ;; Serialize placeholder for displacement.
    (serialize nil stream))
  ;; Save contents
  (loop for i below (array-total-size array)
        do (serialize (row-major-aref array i) stream)))

(defmethod deserialize-contents ((marker (eql +array+)) stream)
  (let* ((type (deserialize stream))
         (dimensions (deserialize-list stream))
         (has-fill-pointer-p nil)
         (fill-pointer nil)
         adjustable-p)
    (when (= 1 (length dimensions))
      (setq has-fill-pointer-p (deserialize stream))
      (when has-fill-pointer-p
        (setq fill-pointer (deserialize stream))))
    (setq adjustable-p (deserialize stream))
    (deserialize stream) ; skip placeholder for displacement
    ;; Create array and load contents
    (let ((array (make-array dimensions
                             :element-type type
                             :adjustable adjustable-p
                             :fill-pointer fill-pointer)))
      (loop for i below (array-total-size array)
            do (setf (row-major-aref array i) (deserialize stream)))
      array)))

(defmethod scan-contents ((marker (eql +array+)) stream gc)
  (scan stream gc) ; scan type
  (let ((dimensions (deserialize-list stream)))
    (when (= 1 (length dimensions))
      (let ((has-fill-pointer-p (deserialize stream)))
        (when has-fill-pointer-p
          ;; skip fill-pointer
          (deserialize stream))))
    (deserialize stream) ; skip adjustable-p
    (deserialize stream) ; skip placeholder for displacement
    ;; Scan contents
    (let ((total-size (reduce #'* dimensions)))
      (loop repeat total-size
            do (scan stream gc)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pathnames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((pathname pathname) stream)
  ;; PORTABILITY NOTE: This is not necessarily portable. If an implementation
  ;; uses non-serializable objects to represent host or device or directory
  ;; or name or type or version, this will break.
  (serialize-marker +pathname+ stream)
  #-sbcl(serialize (pathname-host pathname) stream)
  #+sbcl(serialize (host-namestring pathname) stream)
  (serialize (pathname-device pathname) stream)
  (serialize (pathname-directory pathname) stream)
  (serialize (pathname-name pathname) stream)
  (serialize (pathname-type pathname) stream)
  (serialize (pathname-version pathname) stream))

(defmethod deserialize-contents ((marker (eql +pathname+)) stream)
  (let* ((host (deserialize stream))
         (device (deserialize stream))
         (directory (deserialize stream))
         (name (deserialize stream))
         (type (deserialize stream))
         (version (deserialize stream)))
    (make-pathname :host host
                   :device device
                   :directory directory
                   :name name
                   :type type
                   :version version)))

(defmethod scan-contents ((marker (eql +pathname+)) stream gc)
  ;; skip host, device, directory, name, type, version
  (dotimes (i 6)
    (scan stream gc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod serialize ((hash-table hash-table) stream)
  (serialize-marker +hash-table+ stream)
  ;; Hash-table-test is guaranteed to return a symbol (for the standardized
  ;; hash-table test functions), so that's nicely portable.
  (serialize (hash-table-test hash-table) stream)
  (serialize (hash-table-size hash-table) stream)
  (serialize (hash-table-rehash-size hash-table) stream)
  (serialize (hash-table-rehash-threshold hash-table) stream)
  (serialize (hash-table-count hash-table) stream)
  (maphash (lambda (key value)
             (serialize key stream)
             (serialize value stream))
           hash-table))

(defmethod deserialize-contents ((marker (eql +hash-table+)) stream)
  (let* ((test (deserialize stream))
         (size (deserialize stream))
         (rehash-size (deserialize stream))
         (rehash-threshold (deserialize stream))
         (count (deserialize stream)))
    (let ((table (make-hash-table :test test
                                  :size size
                                  :rehash-size rehash-size
                                  :rehash-threshold rehash-threshold)))
      (loop repeat count
            do (let* ((key (deserialize stream))
                      (value (deserialize stream)))
                 (setf (gethash key table) value)))
      table)))

(defmethod scan-contents ((marker (eql +hash-table+)) stream gc)
  ;; Scan test, size, rehash-size and rehash-threshold.
  (loop repeat 4
        do (scan stream gc))
  (let ((count (deserialize stream)))
    (loop repeat count do
          ;; Scan key and value
          (scan stream gc)
          (scan stream gc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Free list integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod deserialize-contents ((marker (eql +free-block+)) stream)
  (declare (ignore stream))
  :free-block)

(defmethod deserialize-contents ((marker (eql +live-object+)) stream)
  (declare (ignore stream))
  :live-object)

(defmethod deserialize-contents ((marker (eql +dead-object+)) stream)
  (declare (ignore stream))
  :dead-object)

(defmethod deserialize-contents ((marker (eql +reserved-object+)) stream)
  (declare (ignore stream))
  :reserved)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :serialize)

;; $Id: mop.lisp,v 1.13 2007/01/20 18:17:55 alemmens Exp $

(in-package :rucksack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOP Magic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  
;;; Metaclass PERSISTENT-CLASS
;;; 

(defclass persistent-class (standard-class)
  ((persistent-slots :initform '()
                     :accessor class-persistent-slots)
   (index :initarg :index :initform nil
          :documentation "Can be either NIL (for no class index) or T
(for the standard class index).  Default value is NIL.")
   (changed-p :initform nil :accessor class-changed-p
              :documentation "True iff the class definition was changed
but the schemas haven't been updated yet.  This flag is necessary because
some MOP implementations don't call FINALIZE-INHERITANCE when a class
was redefined and a new instance of the redefined class is created.")))


(defmethod class-index ((class persistent-class))
  ;; According to the MOP, the INDEX slot is initialized with the
  ;; list of items following the :INDEX option, but we're only
  ;; interested in the first item of that list.
  (first (slot-value class 'index)))

;;
;; Persistent slot definitions
;;

(defclass persistent-slot-mixin ()
  ((persistence :initarg :persistence
                :initform t
                :reader slot-persistence
                :documentation "T for persistent slots, NIL for
transient slots.  Default value is T.")
   (index :initarg :index
          :initform nil
          :reader slot-index
          :documentation "An index spec designator for indexed slots,
NIL for non-indexed slots.  Default value is NIL.")
   (unique :initarg :unique
           :initform nil
           :reader slot-unique
           :documentation "Only relevant for indexed slots.  Can be
either NIL (slot values are not unique), T (slot values are unique,
and an error will be signaled for attempts to add a duplicate slot
value) or :NO-ERROR (slot values are unique, but no error will be
signaled for attempts to add a duplicate slot value).  :NO-ERROR
should only be used when speed is critical.
  The default value is NIL.")))

(defclass persistent-direct-slot-definition
    (persistent-slot-mixin standard-direct-slot-definition)
  ())

(defclass persistent-effective-slot-definition
    (persistent-slot-mixin standard-effective-slot-definition)
  ())


;;
;; Copying and comparing slot definitions
;;

(defun copy-slot-definition (slot-def)
  (make-instance (class-of slot-def)
                 :name (slot-definition-name slot-def)
                 :initargs (slot-definition-initargs slot-def)
                 :readers (slot-definition-readers slot-def)
                 :writers (slot-definition-writers slot-def)
                 :allocation (slot-definition-allocation slot-def)
                 :type (slot-definition-type slot-def)
                 ;; Our own options.
                 :persistence (slot-persistence slot-def)
                 :index (slot-index slot-def)
                 :unique (slot-unique slot-def)))


(defun slot-definition-equal (slot-1 slot-2)
  (and (equal (slot-persistence slot-1) (slot-persistence slot-2))
       (index-spec-equal (slot-index slot-1) (slot-index slot-2))
       (equal (slot-unique slot-1) (slot-unique slot-2))))


(defun compare-slots (old-slots slots)
  "Returns three values: a list of added slots, a list of discarded slots
and a list of changed (according to SLOT-DEFINITION-EQUAL) slots."
  (let ((added-slots (set-difference slots old-slots
                                     :key #'slot-definition-name))
        (discarded-slots (set-difference old-slots slots
                                         :key #'slot-definition-name))
        (changed-slots
         (loop for slot in slots
               for old-slot = (find (slot-definition-name slot) old-slots
                                    :key #'slot-definition-name)
               if (and old-slot
                       (not (slot-definition-equal slot old-slot)))
               collect slot)))
    (values added-slots discarded-slots changed-slots)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod validate-superclass ((class standard-class)
                                (superclass persistent-class))
  t)


(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initializing the persistent-class metaobjects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The (RE)INITIALIZE-INSTANCE methods below get called whenever a class with
;; metaclass PERSISTENT-CLASS is (re-)defined. When that happens, we: 
;;  - make sure that the class inherits from persistent-object
;;  - create or update schemas.

(defmethod initialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key direct-superclasses
                                        &allow-other-keys)
  ;; Make sure the class inherits from persistent-object.
  (let ((result (apply #'call-next-method
                       class
                       :direct-superclasses (maybe-add-persistent-object-class
                                             class
                                             direct-superclasses)
                       ;; Tell Lispworks that it shouldn't bypass
                       ;; slot-value-using-class.
                       #+lispworks :optimize-slot-access #+lispworks nil 
                       args)))
    (update-indexes class)
    result))


(defmethod reinitialize-instance :around ((class persistent-class)
                                          &rest args
                                          &key direct-superclasses
                                          &allow-other-keys)
  (let ((result (apply #'call-next-method
                       class
                       :direct-superclasses (maybe-add-persistent-object-class
                                             class
                                             direct-superclasses)
                       ;; Tell Lispworks that it shouldn't bypass
                       ;; SLOT-VALUE-USING-CLASS.
                       #+lispworks :optimize-slot-access #+lispworks nil
                       args)))
    (setf (class-changed-p class) t)
    (update-indexes class)
    result))



(defun maybe-add-persistent-object-class (class direct-superclasses)
  ;; Add PERSISTENT-OBJECT to the superclass list if necessary.
  (let ((root-class (find-class 'persistent-object nil))
        (persistent-class (find-class 'persistent-class)))
    (if (or (null root-class)
            (eql class root-class)
            (find-if (lambda (direct-superclass)
                       (member persistent-class
                               (compute-class-precedence-list
                                (class-of direct-superclass))))
                     direct-superclasses))
        direct-superclasses
      (cons root-class direct-superclasses))))

(defun update-indexes (class)
  ;; Update class and slot indexes.
  (when (fboundp 'current-rucksack)
    ;; This function is also called during compilation of Rucksack
    ;; (when the class definition of PERSISTENT-OBJECT is compiled).
    ;; At that stage the CURRENT-RUCKSACK function isn't even defined
    ;; yet, so we shouldn't call it.
    (let ((rucksack (current-rucksack)))
      (when rucksack
        (rucksack-update-class-index rucksack class)
        (rucksack-update-slot-indexes rucksack class)))))


(defmethod finalize-inheritance :after ((class persistent-class))
  (update-slot-info class))

(defun update-slot-info (class)
  ;; Register all (effective) persistent slots.
  (setf (class-persistent-slots class)
        (remove-if-not #'slot-persistence (class-slots class)))
  ;; Update schemas if necessary.
  (when (fboundp 'current-rucksack) ; see comment for UPDATE-INDEXES
    (let ((rucksack (current-rucksack)))
      (when rucksack
        (maybe-update-schemas (schema-table (rucksack-cache rucksack))
                              class))))
  ;;
  (setf (class-changed-p class) nil))

(defun maybe-update-slot-info (class)
  (when (class-changed-p class)
    (update-slot-info class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Computing slot definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-direct-slot-definition))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'persistent-effective-slot-definition))



(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  (let ((effective-slotdef (call-next-method))
        (persistent-slotdefs
         (remove-if-not (lambda (slotdef)
                          (typep slotdef 'persistent-direct-slot-definition))
                        direct-slot-definitions)))

    ;; If any direct slot is persistent, then the effective one is too.
    (setf (slot-value effective-slotdef 'persistence)
          (some #'slot-persistence persistent-slotdefs))

    ;; If exactly one direct slot is indexed, then the effective one is
    ;; too. If more then one is indexed, signal an error.
    (let ((index-slotdefs (remove-if-not #'slot-index persistent-slotdefs)))
      (cond ((cdr index-slotdefs)
             (error "Multiple indexes for slot ~S in ~S:~% ~{~S~^, ~}."
                    slot-name class
                    (mapcar #'slot-index index-slotdefs)))
            (index-slotdefs
             (setf (slot-value effective-slotdef 'index)
                   (slot-index (car index-slotdefs))))))
     
    ;; If exactly one direct slot is unique, then the effective one is
    ;; too. If more then one is unique, signal an error.
    (let ((unique-slotdefs (remove-if-not #'slot-unique persistent-slotdefs)))
      (cond ((cdr unique-slotdefs)
             (error "Multiple uniques for slot ~S in ~S:~% ~{~S~^, ~}."
                    slot-name class
                    (mapcar #'slot-unique unique-slotdefs)))
            (unique-slotdefs
             (setf (slot-value effective-slotdef 'unique)
                   (slot-unique (car unique-slotdefs))))))
     
    ;; Return the effective slot definition.
    effective-slotdef))
 

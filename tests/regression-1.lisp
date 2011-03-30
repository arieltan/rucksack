
(in-package :rucksack-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-rucksack (rs *test-suite* :if-exists :supersede)
    (with-transaction ()
      (defclass broken ()
        ((string-key :initarg :data :accessor string-key
                     :index :string-index))
        (:index t)
        (:metaclass persistent-class)))))

(progn
  ;; This used to break because LEAF-DELETE-KEY would cause a
  ;; comparison between an object id (i.e. an integer) and the
  ;; symbol KEY-IRRELEVANT.  Fixed in version 0.1.11.
  (with-rucksack (rs *test-suite*)
    (with-transaction ()
      (make-instance 'broken :data "foo1")
      (make-instance 'broken :data "foo2")
      (rucksack-map-class rs 'broken
                          (lambda (obj)
                            (format t "Found ~A~%" (string-key obj))))))
  (with-rucksack (rs *test-suite*)
    (with-transaction ()
      (let (foo1)
        (rucksack-map-slot rs 'broken 'string-key
                           (lambda (object) (setq foo1 object))
                           :equal "foo1")
        (assert foo1)
        (rucksack::rucksack-delete-object rs foo1)
        (format t "Deleted foo1~%")
        (rucksack-map-class rs 'broken
                            (lambda (obj)
                              (format t "Found ~A~%" (string-key obj))))
        (let ((index (rucksack-class-index rs 'broken :errorp t)))
          (check-order (rs::index-data index)))))))


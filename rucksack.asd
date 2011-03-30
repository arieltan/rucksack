;;; $Id: rucksack.asd,v 1.21 2009/05/27 14:26:25 alemmens Exp $

(in-package :cl-user)
(defpackage rucksack-asd
  (:use :cl :asdf))
(in-package :rucksack-asd)

(defsystem rucksack
  :version "0.1.20"
  :serial t
  :components ((:file "queue")
               (:file "package")
               (:file "errors")
               (:file "mop")
               (:file "serialize" )
               (:file "heap")
               (:file "object-table")
               (:file "schema-table")
               (:file "garbage-collector")
               (:file "cache")
               (:file "objects")
               (:file "p-btrees")
               (:file "index")
               (:file "rucksack")
               (:file "transactions")
               (:file "import-export"))
  :description "a flexible, light weight, open source persistence library"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :direction :input)
      (let ((seq (make-array (file-length stream)
                             :element-type 'character
                             :fill-pointer t)))
        (setf (fill-pointer seq) (read-sequence seq stream))
        seq)))

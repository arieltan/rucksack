
(cl:defpackage :rucksack-test
  (:nicknames :rs-test)
  (:use :common-lisp :rucksack :lisp-unit)
  (:export #:run-tests))

(cl:defpackage :rucksack-test-schema-update
  (:nicknames :rs-tsu)
  (:use :common-lisp :rucksack))


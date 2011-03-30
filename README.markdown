
* Getting started

To compile and load Rucksack and make sure that the basics are working:

1. Make sure you have ASDF (Another System Definition Facility) loaded.

2. Load rucksack.asd.

3. Load tests/rucksack-test.asd.

4. (asdf:oos 'asdf:load-op :rucksack-test)

5. (in-package :rucksack-test)

6. (run-tests)


* The tutorial

The tutorial by Brad Beveridge (in doc/rucksack-tutorial.lisp) is a
good next step.

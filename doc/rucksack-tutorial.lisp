#|
Rucksack Tutorial by Brad Beveridge (brad.beveridge@gmail.com)

What is Rucksack?  Hopefully you already know this, but the quick
description is that Rucksack is a persistence library for Common Lisp.
Its project page is at http://common-lisp.net/project/rucksack/, and
the author of the library is Arthur Lemmens.

Rucksack provides a fairly transparent persistence mechanism for
conses, vectors and CLOS objects.  RS also provides a btree based
indexing mechanism that lets you effciently look up CLOS objects and
other persistent data.

This tutorial will walk you through creating a simple address book
database that uses Rucksack as its backing store.  The tutorial is
entirely in a single file to make it easier to just load and run, I
assume that you have managed to install Rucksack and load it into your
Lisp image (probably using ASDF)

The tutorial is designed to be read from start to end, with the reader
evaluating the live code as we go (C-cC-e in Slime).  Alternately you
may compile and load this file (C-cC-k) , and only evaluate the forms
that you find interesting.  If you do go with compiling and loading
the whole file, you may get some warnings, none are serious, so just
push on through :)

Let's get started by defining a package that uses Rucksack, and
declaring that we are defining our code within it.
|#

(defpackage :rucksack-tutorial
 (:nicknames :rs-tute)
 (:use :cl :rucksack))
(in-package :rucksack-tutorial)

#| 
RS will need to be given a path where it can create its files.  The
path below should work for most unix like systems.  If you are running
other operating systems, you should create a temporary directory and
set the value below.  
|#

(defvar *rs-tute-directory* #p"/tmp/rs-tute/")

#|
Now we define our class.  It should look familiar to you because it's just CLOS
with some extra keywords.
The information that each contact in our address book should have is:
 - a unique number for internal use
 - the name of the contact
 - a phone number
 - email address
 - street address
 - a notes area for the contact

We will want to search and display the data in the address book in
many different ways, so for each slot that we may want to sort by or
look up we will use an appropriate index.  Rucksack pre-defines some
index specifications which you can find in index.lisp.

By specifying that we want to index a particular slot in a class, RS
will construct a persistent BTree that maps the value of that slot to
the object.  In our example we specify five slots to be indexed, which
means RS will manage five indexes just for this class.  Having a slot
indexed means that we can very quickly search for a specific slot
value, or return a range of slot values.

Since we also want to index all instances of the class as a whole, we
specify the (:index t) class property and RS creates a sixth index
that tracks every instance of CONTACT-DETAILS.  Since we are indexing
by other slots we don't really need this index, but it is fine for
this example.

We're going to open the rucksack storage in :SUPERSEDE mode for this
first evaluation so that we always start with a fresh database.

* NOTE *
As of 30/1/08 Rucksack requires that your DEFCLASS form is evaluated
inside an open RS and transaction.  It is during class definition that
the initial indexes are created.
|#

(with-rucksack (rs *rs-tute-directory* :if-exists :supersede)
  (with-transaction ()
    (defclass contact-details ()
      ((unique-id    :initarg :unique-id :accessor unique-id-of 
		     :index :number-index
                     :unique t
		     :documentation "A unique number for each contact in our DB")
       
       (name         :initarg :name :accessor name-of 
		     :index :case-insensitive-string-index
	             :documentation "The full name of the contact")
       
       (phone-number :initarg :phone-number :accessor phone-number-of
		     :index :number-index
		     :documentation "The phone number of the contact")
       
       (email        :initarg :email :accessor email-of
		     :index :case-insensitive-string-index
		     :documentation "Email address")
       
       (address      :initarg :address :accessor address-of
		     :index :case-insensitive-string-index
		     :documentation "Postal address")
       
       (notes        :initarg :notes :accessor notes-of
		     :documentation "Free form notes about this contact"))
      (:documentation
       "The CONTACT-DETAILS class is the backbone of our address book.
It provides details about each contact in our address book.")
      (:index t)
      (:metaclass persistent-class)) 
    ))

#|
And let's specialize INITIALIZE-INSTANCE to automatically give a unique ID.
|#

(defvar *unique-id* 0)
(defmethod initialize-instance :after ((obj contact-details) &key)
  (setf (unique-id-of obj) (incf *unique-id*)))

#| Lets make it so we can print instances of CONTACT-DETAILS |#

(defmethod print-object ((obj contact-details) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (unique-id name phone-number email address notes) obj
	(format stream "~A: '~A' ~A '~A' '~A' '~A'"
                unique-id name phone-number email address notes)))) 

#|
Now our initial database is setup, lets write a simple function that
creates new instances of CONTACT-DETAILS.

I've chosen a very simple approach here, every time you call
MAKE-CONTACT the function will open the store and create a new
transaction.  A longer running application would probably have a
WITH-RUCKSACK form near its main function, or perhaps manually open
and close the store.
|#

(defun make-contact (name &optional phone-number email address notes)
  (with-rucksack (rs *rs-tute-directory*)
    (with-transaction ()
	(make-instance 'contact-details 
		       :name (or name "")
		       :phone-number (or phone-number 0)
		       :email (or email "")
		       :address (or address "")
		       :notes notes))))

#|
Now is probably a good time to talk about Rucksack's transactions.
Basically any time you perform a Rucksack operation it must be inside
a transaction.  Rucksack transactions are just what you would expect.
Within a transaction you are guaranteed one of two outcomes, either:

1. The transaction will complete and the modified state will be
   written to the RS store.
- OR
2. The transaction is aborted and no changed state is written to the
   RS store.

In the case of #2 it is up to the application layer to decide what to
do if a transaction fails.  Inside a WITH-TRANSACTION form, a
transaction can be aborted by aborting the form's body (e.g. by
calling ABORT or by signaling an error).

----------------
Now, lets create some contacts.  We don't need to wrap these in a transaction because
MAKE-CONTACT does it for us already.
|#

(make-contact "Brad Beveridge" 0 "brad.beveridge@gmail.com" "" "Guy who wrote this.") 
(make-contact "Arthur Lemmens" 555 "alemmens@xs4all.nl" "" "The author of Rucksack.")
(make-contact "Noddy Noname" 1234 "noddy@nowhere.com")
(make-contact "Jane" 2345 "jane@hotmail.com" "Jaynes Town" "Standard female name")
(make-contact "Zane" 9345 "zane@hotmail.com" "Zaynes Town" "I needed a Z name")

#| 
Feel free to be a bit more creative than me & put some more contacts in... 

We are now at the stage where we have some CLOS objects that happen to
be persistent.  Since our transactions completed (hopefully!), those
objects are also safely stored on our hard drive.

Our first query function simply prints out all of the CONTACT-DETAILS
objects that Rucksack is aware of.
|#

(defun print-all-contacts ()
  (with-rucksack (rs *rs-tute-directory*)
    (with-transaction ()
      (rucksack-map-class rs 'contact-details 
			  (lambda (object)
			    (format t "~A~%" object))))))

#|
(print-all-contacts)

The function RUCKSACK-MAP-CLASS is fairly straight forward, it takes a
Rucksack, a class name and a function that must accept one argument.
The function will be called for each object in the store that is of
the given class type.  The class must have the
(:index t) property.

Let's write a function that finds a contact by matching their name.
|#

(defun find-contact-by-name (name)
  (with-rucksack (rs *rs-tute-directory*)
    (with-transaction ()
      (rucksack-map-slot rs 'contact-details 'name 
			 (lambda (contact)
			   (return-from find-contact-by-name contact))
			 :equal name)))
  nil)

#| 
(format t "~A~%" (find-contact-by-name "jane"))

Notice how similar this is to how we printed all of the object
instances?  Rucksack has lots of RUCKSACK-MAP-* functions that all
follow the same basic form, you pass in the data you want to search
for and Rucksack will call the function that you supply for each
object that matches.

Let's try returning a range of name matches.  If we don't supply an
end match, then it means we want all matches after start.  If we don't
supply start or end strings then the function will return all
instances in their sorted order.

|#

(defun find-contacts-by-name-range (&optional start end)
  (let (ret)
    (with-rucksack (rs *rs-tute-directory*)
      (with-transaction ()
	(rucksack-map-slot rs 'contact-details 'name 
			   (lambda (contact)
			     (push contact ret))
			   :min start :max end :include-min t :include-max t)))
    ; reverse the list so it's in the expected order
    (nreverse ret)))

#|
(dolist (contact (find-contacts-by-name-range "a" "c"))
	 (format t "~A~%" contact)) 
(dolist (contact (find-contacts-by-name-range "c"))
	 (format t "~A~%" contact)) 

Let's write a little function to delete an object if we have its name.
|#

(defun delete-object-by-name (name)
  (with-rucksack (rs *rs-tute-directory*)
    (with-transaction ()
      (let ((contact (find-contact-by-name name)))
	(when contact
	  (rucksack::rucksack-delete-object rs contact))))))

#| 
(delete-object-by-name "Zane") 
(print-all-contacts)

We've now covered enough of Rucksack to actually do some useful
things.  We can create persistent objects, search through them and
also delete them.

Rucksack has plenty of other goodies, such as persistent storage of
non CLOS data - vectors and conses.  Rucksack also supports changing
the definition of objects in a similar manner to the way you would
redefine CLOS classes.

For a more indepth explanation of how Rucksack works, please read the
talk-eclm2006.txt file that comes with the source code.

Happy Rucksacking!
Brad
|#

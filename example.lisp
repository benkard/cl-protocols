(defpackage #:mulk.protocols-examples
  (:nicknames #:protocols-examples)
  (:use #:cl #:protocols))


(in-package #:mulk.protocols-examples)


(define-protocol printable ()
  ((print-object * stream)))

(define-protocol serialisable ()
  ((serialise * stream)))

(define-protocol additive ()
  ((add * *)
   (negate *)))

(define-protocol multiplicative ()
  ((multiply * *)
   (invert *)))

(define-protocol field (additive multiplicative) ())

(define-protocol serialisable-field (serialisable field) ())


(defgeneric serialise (x stream))
(defgeneric add (x y))
(defgeneric negate (x))
(defgeneric multiply (x stream))
(defgeneric invert (x))

(defclass a () ())

;; Note the style warnings signalled by the following.
(implement-protocols a (additive multiplicative serialisable)
  (defmethod add ((x a) (y a)))
  (defmethod negate ((x a)))
  (defmethod multiply ((x a) y)))

(print (conforms-to-p 'a 'additive))
(print (really-conforms-to-p 'a 'additive))
(print (conforms-to-p 'a 'multiplicative))
(print (really-conforms-to-p 'a 'multiplicative))
(print (conforms-to-p 'a 'printable))
(print (really-conforms-to-p 'a 'printable))

(implement-protocols a (printable))

(print (conforms-to-p 'a 'printable))
(print (really-conforms-to-p 'a 'printable))

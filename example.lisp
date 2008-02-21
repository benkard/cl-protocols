(defpackage #:mulk.protocols-examples
  (:nicknames #:protocols-examples)
  (:use #:cl #:protocols))


(in-package #:mulk.protocols-examples)


(define-protocol printable ()
  ((print-object * stream))
  (:strictness t))

(define-protocol serialisable ()
  ((serialise * stream))
  (:strictness t))

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

;; The following should signal five style warnings about missing methods.
(implement-protocols a (serialisable-field))


(defclass b () ())

;; Note the two style warnings signalled by the following.
(implement-protocols b (additive multiplicative serialisable)
  (defmethod add ((x b) (y b)))
  (defmethod negate ((x b)))
  (defmethod multiply ((x b) y)))

(print (conforms-to-p 'b 'additive))              ;=> T
(print (really-conforms-to-p 'b 'additive))       ;=> T
(print (conforms-to-p 'b 'multiplicative))        ;=> T
(print (really-conforms-to-p 'b 'multiplicative)) ;=> NIL
(print (conforms-to-p 'b 'printable))             ;=> NIL
(print (really-conforms-to-p 'b 'printable))      ;=> NIL

;; The following works because PRINT-OBJECT is specialised over T.
(implement-protocols b (printable))

(print (conforms-to-p 'b 'printable))             ;=> T
(print (really-conforms-to-p 'b 'printable))      ;=> T

(print (subtypep 'b 'printable))                  ;=> T
(print (subtypep 'b 'additive))                   ;=> T
(print (subtypep 'b 'serialisable))               ;=> NIL

(in-package #:mulk.protocols)


(defvar *protocols* (make-hash-table))


(defun find-protocol (name &optional (errorp t))
  (find-class name errorp))


(defun (setf find-protocol) (protocol name &optional errorp)
  (declare (ignore errorp))
  (setf (find-class name) protocol))


(defun protocol-name (protocol)
  (class-name protocol))


(defvar *alleged-conformance* (make-hash-table :test 'equal))
(defvar *actual-conformance* (make-hash-table :test 'equal))


(defun conforms-to-p (class protocol)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class))))
        (protocol (etypecase protocol
                    (protocol protocol)
                    (symbol (find-protocol protocol)))))
    (or (gethash (cons class protocol) *alleged-conformance* nil)
        (some #'(lambda (x) (conforms-to-p x protocol))
              (class-direct-superclasses class)))))


(defun really-conforms-to-p (class protocol)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class))))
        (protocol (etypecase protocol
                    (protocol protocol)
                    (symbol (find-protocol protocol)))))
    (or (gethash (cons class protocol) *actual-conformance* nil)
        (some #'(lambda (x) (really-conforms-to-p x protocol))
              (class-direct-superclasses class)))))


(defgeneric %conforms-to-p (class protocol))

(defmethod %conforms-to-p ((class t) (protocol t))
  nil)


(defgeneric %really-conforms-to-p (class protocol))

(defmethod %really-conforms-to-p ((class t) (protocol t))
  nil)


(defclass protocol (standard-class)
  ((methods :initarg :methods
            :reader protocol-methods)))


(defmethod validate-superclass (class (superclass protocol))
  t)


(defmethod validate-superclass ((class standard-class) (superclass protocol))
  t)


(defmethod validate-superclass ((class protocol) (superclass standard-class))
  t)


(defmacro define-protocol (name superprotocols methods &rest options)
  "Define a new protocol.

superprotocols ::= (*name*\\*)

methods ::= ((method-name [\\* | class-name]\\*)\\*)"
  `(defclass ,name ,superprotocols
        ()
     (:metaclass protocol)
     (:methods ,@methods)
     ,@options))


(defun ensure-conformance (class-name protocol-designator)
  (let* ((protocol (typecase protocol-designator
                     (symbol (find-protocol protocol-designator))
                     (t protocol-designator)))
         (conforming-p nil))
    (with-accessors ((name protocol-name)
                     (superprotocols class-direct-superclasses)
                     (methods protocol-methods))
        protocol
      (loop for method in methods
            for (name . raw-argument-class-list) = method
            for argument-class-list = (substitute class-name
                                                  '*
                                                  raw-argument-class-list)
            for real-argument-class-list = (mapcar #'find-class
                                                   argument-class-list)
            for (applicable-methods methods-determinable-p) =
                (multiple-value-list
                 (funcall #'compute-applicable-methods-using-classes
                          (fdefinition name)
                          real-argument-class-list))
            when (and methods-determinable-p (null applicable-methods))
              collect method into missing-methods
              and do (warn (make-condition 'simple-style-warning
                                :format-control "Class ~A does not ~
                                                 implement method ~A ~
                                                 with argument types ~
                                                 ~A as required by ~
                                                 protocol ~A."
                                :format-arguments (list class-name
                                                        (first method)
                                                        real-argument-class-list
                                                        (protocol-name protocol))))
            unless methods-determinable-p
              do (warn (make-condition 'simple-style-warning
                            :format-control "Could not check whether ~
                                             class ~A implements ~
                                             method ~A with argument ~
                                             types ~A as required by ~
                                             protocol ~A.  Assuming it ~
                                             does."
                            :format-arguments (list class-name
                                                    (first method)
                                                    real-argument-class-list
                                                    (protocol-name protocol))))
            finally (setq conforming-p (null missing-methods)))
      (let ((really-conforming-p
             (and conforming-p
                  (every #'identity
                         ;; We have to use MAPCAR because we
                         ;; don't want short-circuiting.
                         (mapcar #'(lambda (superprotocol)
                                     (or (not (typep superprotocol 'protocol))
                                         (ensure-conformance class-name
                                                             superprotocol)))
                                 superprotocols)))))
        (setf (gethash (cons (find-class class-name) protocol)
                       *alleged-conformance*)
              t)
        (setf (gethash (cons (find-class class-name) protocol)
                       *actual-conformance*)
              really-conforming-p)
        (let ((original-class (find-class class-name)))
          (unless (subtypep original-class protocol)
            (handler-case
                (let* ((new-class-name (gensym (symbol-name class-name)))
                       (temporary-class-name (gensym (symbol-name class-name)))
                       (new-class
                        (ensure-class
                             temporary-class-name
                             :direct-superclasses (list original-class protocol)
                             :metaclass (class-of original-class))))
                  (setf (class-name original-class) new-class-name)
                  (setf (class-name new-class) class-name)
                  (setf (find-class new-class-name) original-class)
                  (setf (find-class class-name) new-class))
              (serious-condition ()
                  (warn (make-condition
                             'simple-style-warning
                             :format-control "Could not add protocol ~A ~
                                              as a superclass of ~A. ~
                                              Most probably, the metaclasses ~
                                              are incompatible.  (See the MOP ~
                                              specification, specifically the ~
                                              part about VALIDATE-SUPERCLASS.)"
                             :format-arguments (list protocol
                                                     original-class)))))))))))


(defmacro implement-protocols (class protocols &body definitions)
  `(progn
     ,@definitions
     ,@(mapcar #'(lambda (protocol)
                   `(ensure-conformance ',class ',protocol))
               protocols)))

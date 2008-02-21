(in-package #:mulk.protocols)


(defvar *protocols* (make-hash-table))


(defun find-protocol (name &optional (errorp t))
  (find-class name errorp))


(defun (setf find-protocol) (protocol name &optional errorp)
  (declare (ignore errorp))
  (setf (find-class name) protocol))


(defun protocol-name (protocol)
  (class-name protocol))


(defun conforms-to-p (class protocol)
  (let ((real-class (typecase class
                      (symbol (find-class class))
                      (t class)))
        (real-protocol (typecase protocol
                         (symbol (find-protocol protocol))
                         (t protocol))))
    (or (%conforms-to-p real-class real-protocol)
        (some #'(lambda (superclass)
                  (conforms-to-p superclass real-protocol))
              (class-direct-superclasses real-class)))))


(defun really-conforms-to-p (class protocol)
  (let ((real-class (typecase class
                      (symbol (find-class class))
                      (t class)))
        (real-protocol (typecase protocol
                         (symbol (find-protocol protocol))
                         (t protocol))))
    (or (%really-conforms-to-p real-class real-protocol)
        (some #'(lambda (superclass)
                  (really-conforms-to-p superclass real-protocol))
              (class-direct-superclasses real-class)))))


(defgeneric %conforms-to-p (class protocol))

(defmethod %conforms-to-p ((class t) (protocol t))
  (declare (ignore class protocol))
  nil)


(defgeneric %really-conforms-to-p (class protocol))

(defmethod %really-conforms-to-p ((class t) (protocol t))
  (declare (ignore class protocol))
  nil)


(defclass protocol (standard-class)
  ((methods :initarg :methods
            :reader protocol-methods)))


(defmethod validate-superclass (class (superclass protocol))
  t)


(defmethod validate-superclass ((class standard-class) (superclass protocol))
  t)


(defmacro define-protocol (name superprotocols methods &rest options)
  "Define a new protocol.

superprotocols ::= (*name*\\*)

methods ::= ((method-name [\\* | class-name]\\*)\\*)"
  `(defclass ,name ,(or superprotocols '(t))
        ()
     (:metaclass protocol)
     (:methods ,@methods)
     ,@options))


(defun ensure-conformance (class-name protocol-designator)
  (let* ((protocol (typecase protocol-designator
                     (symbol (find-protocol protocol-designator))
                     (t protocol-designator)))
         (protocol-name (protocol-name protocol))
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
        (ensure-method #'%conforms-to-p
                       '(lambda (x y) t)
                       :specializers (list (intern-eql-specializer (find-class class-name))
                                           (intern-eql-specializer (find-protocol protocol-name))))
        (ensure-method #'%really-conforms-to-p
                       `(lambda (x y) ,really-conforming-p)
                       :specializers (list (intern-eql-specializer (find-class class-name))
                                           (intern-eql-specializer (find-protocol protocol-name))))
        (let ((original-class (find-class class-name)))
          (unless (subtypep original-class protocol)
            (let ((new-class-name (gensym (symbol-name class-name))))
              (setf (class-name original-class) new-class-name)
              (setf (find-class new-class-name) original-class)
              (setf (find-class class-name)
                    (ensure-class
                         class-name
                         :direct-superclasses (list original-class protocol)
                         :metaclass (class-of original-class))))))))))


(defmacro implement-protocols (class protocols &body definitions)
  `(progn
     ,@definitions
     ,@(mapcar #'(lambda (protocol)
                   `(ensure-conformance ',class ',protocol))
               protocols)))

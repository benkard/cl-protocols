(in-package #:mulk.protocols)


(defvar *protocols* (make-hash-table))


(defun find-protocol (name &optional (errorp t))
  (let ((protocol (gethash name *protocols* nil)))
    (when (and errorp (null protocol))
      (error "There is no protocol named ~A." name))
    protocol))


(defun (setf find-protocol) (protocol name &optional errorp)
  (declare (ignore errorp))
  (setf (gethash name *protocols*) protocol))


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


(defclass protocol ()
  ((name :initarg :name
         :reader protocol-name)
   (superprotocols :initarg :superprotocols
                   :reader protocol-superprotocols)
   (methods :initarg :methods
            :reader protocol-methods)
   (options :initarg :options
            :reader protocol-options)))


(defmacro define-protocol (name superprotocols methods &rest options)
  "Define a new protocol.

superprotocols ::= (*name*\\*)

methods ::= ((method-name [\\* | class-name]\\*)\\*)"
  `(progn
     (when (find-protocol ',name nil)
       (warn (make-condition 'simple-style-warning
                             :format-control "Redefining protocol ~A."
                             :format-arguments (list ',name))))
     (setf (find-protocol ',name)
           (make-instance 'protocol
              :name ',name
              :superprotocols (mapcar #'find-protocol ',superprotocols)
              :methods ',methods
              :options ',options))))


(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream)
    (format stream "PROTOCOL ~A" (protocol-name protocol))))


(defun ensure-conformance (class-name protocol-designator)
  (let* ((protocol (typecase protocol-designator
                     (symbol (find-protocol protocol-designator))
                     (t protocol-designator)))
         (protocol-name (protocol-name protocol))
         (conforming-p nil))
    (with-slots (name superprotocols methods options)
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
                                     (ensure-conformance class-name
                                                         superprotocol))
                                 superprotocols)))))
        (ensure-method #'%conforms-to-p
                       '(lambda (x y) t)
                       :specializers (list (intern-eql-specializer (find-class class-name))
                                           (intern-eql-specializer (find-protocol protocol-name))))
        (ensure-method #'%really-conforms-to-p
                       `(lambda (x y) ,really-conforming-p)
                       :specializers (list (intern-eql-specializer (find-class class-name))
                                           (intern-eql-specializer (find-protocol protocol-name))))))))


(defmacro implement-protocols (class protocols &body definitions)
  `(progn
     ,@definitions
     ,@(mapcar #'(lambda (protocol)
                   `(ensure-conformance ',class ',protocol))
               protocols)))

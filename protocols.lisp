(in-package #:mulk.protocols)


(declaim (optimize debug safety (speed 0) (space 0)))


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
    (%conforms-to-p real-class real-protocol)))


(defun really-conforms-to-p (class protocol)
  (let ((real-class (typecase class
                      (symbol (find-class class))
                      (t class)))
        (real-protocol (typecase protocol
                         (symbol (find-protocol protocol))
                         (t protocol))))
    (%really-conforms-to-p real-class real-protocol)))


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


(defun ensure-conformance (class-name protocol-name)
  (let ((protocol (find-protocol protocol-name)))
    (with-slots (name superprotocols methods options)
        protocol
      (loop for method in methods
            for (name . raw-argument-class-list) = method
            for argument-class-list = (substitute class-name
                                                  '*
                                                  raw-argument-class-list)
            for real-argument-class-list = (mapcar #'find-class
                                                   argument-class-list)
            when (null (funcall #'compute-applicable-methods-using-classes
                                (fdefinition name)
                                real-argument-class-list))
              collect method into missing-methods
              and do (warn (make-condition 'simple-style-warning
                                :format-control "Class ~A does not implement ~
                                                 method ~A with argument types ~
                                                 ~A as required by ~
                                                 protocol ~A."
                                :format-arguments (list class-name
                                                        (first method)
                                                        real-argument-class-list
                                                        (protocol-name protocol))))
            finally (return (null missing-methods))))))


(defmacro implement-protocols (class protocols &body definitions)
  `(progn
     ,@definitions
     ,@(mapcan #'(lambda (protocol)
                   `((let ((conformance (ensure-conformance ',class ',protocol)))
                       (defmethod %really-conforms-to-p
                           ((class (eql (find-class ',class)))
                            (protocol (eql (find-protocol ',protocol))))
                         conformance))
                     (defmethod %conforms-to-p
                         ((class (eql (find-class ',class)))
                          (protocol (eql (find-protocol ',protocol))))
                       t)))
               protocols)))

(in-package #:mulk.protocols)


(define-condition simple-style-warning (style-warning)
  ((format-control :initarg :format-control
                   :reader format-control)
   (format-arguments :initarg :format-arguments
                     :reader format-arguments))
  (:report (lambda (condition stream)
             (apply #'format
                    stream
                    (format-control condition)
                    (format-arguments condition)))))

(defpackage #:mulk.protocols
  (:nicknames #:protocols)
  (:use #:closer-common-lisp)
  (:export #:define-protocol
           #:implement-protocols
           #:conforms-to-p
           #:really-conforms-to-p
           #:protocol-name
           #:find-protocol))

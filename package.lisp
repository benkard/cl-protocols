(defpackage #:mulk.protocols
  (:nicknames #:protocols)
  (:use #:cl #:c2mop)
  (:export #:define-protocol
           #:implement-protocols
           #:conforms-to-p
           #:really-conforms-to-p
           #:protocol-name
           #:find-protocol))

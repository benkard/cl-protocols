(defsystem "protocols"
  :description "Protocol enforcement for CLOS classes."
  :version "0.0.2"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU Lesser General Public License, version 3 or higher"
  :depends-on (#:closer-mop)
  :components
  ((:file "package")
   (:file "conditions" :depends-on ("package"))
   (:file "protocols" :depends-on ("package"))))

(asdf:defsystem cl-r-generator
    :version "0"
    :description "Emit R language code"
    :maintainer " <kielhorn.martin@gmail.com>"
    :author " <kielhorn.martin@gmail.com>"
    :licence "GPL"
    :depends-on ("alexandria" "jonathan")
    :serial t
    :components ((:file "package")
		 (:file "rlang")) )

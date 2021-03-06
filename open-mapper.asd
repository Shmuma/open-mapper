(defpackage :shmuma.open-mapper-system
  (:use :common-lisp :asdf))

(in-package :shmuma.open-mapper-system)

(defsystem open-mapper
    :name "open-mapper"
    :description "Open Mapper"
    :serial t
    :components ((:file "tiles")
                 (:file "pixmap")
                 (:file "storage")
                 (:file "tests")
                 (:file "main"))
    :depends-on (:drakma)
)

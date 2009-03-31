(defpackage :shmuma.open-mapper-system
  (:use :common-lisp :asdf))

(in-package :shmuma.open-mapper-system)

(defsystem open-mapper
    :name "open-mapper"
    :descrioption "Open Mapper"
    :depends-on
    (:tiles
     :storage
     :tests))
    
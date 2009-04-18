(defpackage :shmuma.mapper.storage
  (:use :common-lisp 
        :shmuma.mapper.tiles))

(in-package :shmuma.mapper.storage)


(defclass sqlite-storage ()
  ((name :initarg :name
         :initform (error "You must provide file name")
         :reader name)))


(defmethod initialize-instance :after ((db sqlite-storage) &key)
  :documentation "sqlite-storage constructor. Creates DB object"
  (format t "Init SQLite DB ~a~%" (name db)))

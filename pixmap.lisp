(defpackage :shmuma.mapper.pixmap
  (:use :common-lisp)
  (:export :pixmap
           :make-pixmap))


(in-package :shmuma.mapper.pixmap)


(defclass pixmap ()
  ((data
   :initarg :data
   :initform nil
   :accessor data)))


(defun make-pixmap (data)
  (make-instance 'pixmap :data data))
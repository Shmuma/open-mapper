(defpackage :shmuma.mapper.storage
  (:use :common-lisp)
  )

(in-package :shmuma.mapper.storage)


(defclass storage ()
  nil)


(defgeneric store-tile (storage tile)
  (:documentation "Stores given tile object into storage"))

;; (defmethod store-tile ((storage sqlite-storage) (tile (tile)))
;;   nil)
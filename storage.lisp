(defpackage :shmuma.mapper.storage
  (:use :common-lisp 
        :shmuma.mapper.tiles
        #+ffi :sqlite
        #-ffi :sqlite-fake))

(in-package :shmuma.mapper.storage)


(defclass storage ()
  nil)


(defgeneric store-tile (storage tile)
  (:documentation "Stores given tile object into storage"))

(defmethod store-tile ((storage storage) (tile tile))
  (sqlite-open "tiles.db"))


(defun test ()
  (sqlite-open "test.db"))
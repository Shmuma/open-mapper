(defpackage :shmuma.mapper.storage
  (:use :common-lisp 
        :shmuma.mapper.tiles
        :shmuma.mapper.pixmap))

(in-package :shmuma.mapper.storage)


(defclass storage ()
  nil)


(defclass file-storage (storage)
  ((top-dir
   :initarg :top-dir
   :initform (error "You must provide :top-dir argument")
   :reader top-dir)))


(defgeneric tile-storage-address (storage tile)
  (:documentation "Obtain tile's address understandable by storage engine."))

(defgeneric put-pixmap (storage tile pixmap)
  (:documentation "Put pixmap of given tile in storage."))

(defgeneric get-pixmap (storage tile)
  (:documentation "Get pixmap of tile from storage or nil if not exists."))



;; File system storage engine
(defun read-whole-file (f)
  (let ((data (make-array '(0) :element-type 'unsigned-byte :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte f nil nil)
       while b
       do (vector-push-extend b data))
    data))


(defmethod get-pixmap ((stg file-storage) (tile tile))
  (with-open-file (f (tile-storage-address stg tile) :direction :input :element-type 'unsigned-byte)
    (make-pixmap (read-whole-file f))))


(defmethod tile-storage-address ((stg file-storage) (tile tile))
  (format nil "~s/~6,'0d-~6,'0d-~6,'0d" (top-dir stg) (tx tile) (ty tile) (zoom tile)))


;; HTTP storage engine
;; SQLite storage engine

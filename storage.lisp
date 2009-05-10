(defpackage :shmuma.mapper.storage
  (:use :common-lisp 
        :trivial-http
        :shmuma.mapper.tiles
        :shmuma.mapper.pixmap)
  (:export :storage
           :file-storage
           :http-storage
           :put-pixmap
           :get-pixmap
           :tile-to-storage-ptr
           :make-file-storage
           :make-http-storage
           :print-object))

(in-package :shmuma.mapper.storage)


(defclass storage-ptr ()
  ((tile
    :initarg :tile
    :reader tile)))

(defclass storage-ptr-fname (storage-ptr)
  ((fname
    :initarg :fname
    :reader fname)))

(defclass storage-ptr-http (storage-ptr)
  ((url
    :initarg :url
    :reader url)))


(defclass storage ()
  nil)


(defclass file-storage (storage)
  ((top-dir
   :initarg :top-dir
   :initform (error "You must provide :top-dir argument")
   :reader top-dir)))


(defclass http-storage (storage)
  nil)


(defgeneric put-pixmap (storage sorage-ptr pixmap)
  (:documentation "Put pixmap of given tile in storage."))

(defgeneric get-pixmap (storage storage-ptr)
  (:documentation "Get pixmap of tile from storage or nil if not exists."))

(defgeneric tile-to-storage-ptr (storage tile)
  (:documentation 
   "Constructs storage pointer of apropriate type to
be used in given storage engine"))


;; File system storage engine
(defun make-file-storage (topdir)
  (make-instance 'file-storage :top-dir topdir))


(defun read-whole-file (f)
  (let ((data (make-array '(0) :element-type 'unsigned-byte :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte f nil nil)
       until (null b)
       do (vector-push-extend b data))
    data))


(defmethod get-pixmap ((stg file-storage) (ptr storage-ptr-fname))
  (with-open-file (f (fname ptr) :direction :input :element-type 'unsigned-byte)
    (make-pixmap (read-whole-file f))))


(defmethod put-pixmap ((stg file-storage) (ptr storage-ptr-fname) (pixmap pixmap))
  (with-open-file (f (fname ptr) :direction :output :element-type 'unsigned-byte)
    (loop for b across (data pixmap)
       do (write-byte b f))))


(defmethod tile-to-storage-ptr ((stg file-storage) (tile tile))
  (let ((fname (format nil "~s/~6,'0d-~6,'0d-~6,'0d"
                       (top-dir stg) (tx tile) (ty tile) (zoom tile))))
    (make-instance 'storage-ptr-fname
                   :tile tile :fname fname)))

;; HTTP storage engine
(defun make-http-storage ()
  (make-instance 'http-storage))


(defmethod get-pixmap ((stg http-storage) (ptr storage-ptr-http))
  (destructuring-bind (code header stream) (http-get (url ptr))
    (declare (ignore header))
    (let ((res 
           (if (= 200 code)
               (make-pixmap (read-whole-file stream))
               nil)))
      (close stream)
      res)))


(defmethod tile-to-storage-ptr ((stg http-storage) (tile tile))
  (make-instance 'storage-ptr-http
                 :tile tile :url (get-tile-url tile)))


;; SQLite storage engine


;; utility routines
(defmethod print-object (stm (ptr storage-ptr-http))
  (format stm "#<storage-ptr-http url: ~s>~%" (url ptr)))
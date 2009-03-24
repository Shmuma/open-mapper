;; (defpackage :openmapper.fetcher
;;   (:use :common-lisp))


(defclass base-coord-system ()
  ((name :initarg :name
         :initform (error "Must supply a coord system's name")
         :reader name
         :documentation "Name of tile coordinate system")))


(defclass yandex-coord-system (base-coord-system) ())


(defun google-latlon2unit (lat lon)
  :documentation "Routine converts latitude and longitude to google's internal map units"
  (format t "Hello~%")
)
(defpackage :shmuma.mapper.tiles
  (:use :common-lisp :shmuma.mapper.coords)
  (:export :tiles
           :tiles-for-region))

(in-package :shmuma.mapper.tiles)


(defclass tiles ()
  ((coords
    :initarg :coords
    :initform (error "You must provide coordinate system")
    :reader coords)))


(defgeneric tiles-for-region (tiles up-latlon bot-latlon zoom)
  (:documentation "Obtain list of tile urls by rectangle and zoom level"))


(defmethod tiles-for-region ((tiles tiles) up-latlon dn-latlon zoom)
  (let* ((up (latlon2tiles (coords tiles) up-latlon zoom))
         (dn (latlon2tiles (coords tiles) dn-latlon zoom))
         (ll (list (min (car up) (car dn))
                   (min (cadr up) (cadr dn))))
         (rr (list (max (car up) (car dn))
                   (max (cadr up) (cadr dn)))))
    (loop for ty from (cadr ll) to (cadr rr)
         append (loop for tx from (car ll) to (car rr)
                   collect (format-url (coords tiles) tx ty zoom)))))

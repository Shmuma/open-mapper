(defpackage :shmuma.mapper.tiles
  (:use :common-lisp)
  (:export :tile :valid-tilep :tx :ty :zoom :coords
           :tiles-coords
           :yandex-coords
           :latlon2units
           :units2tile
           :latlon2tile
           :format-url
           :tiles-for-region))

(in-package :shmuma.mapper.tiles)


(defclass tile ()
  ((coords
    :initarg :coords
    :initform (error "You must provide coordinate system")
    :reader coords)
   (tx
    :initarg :tx
    :initform nil
    :accessor tx)
   (ty
    :initarg :ty
    :initform nil
    :accessor ty)
   (zoom
    :initarg :zoom
    :initform nil
    :accessor zoom)))


(defgeneric valid-tilep (tile)
  (:documentation "Checks tile for validity"))

(defgeneric tile-url (tile)
  (:documentation "Returns url of tile"))

(defmethod valid-tilep ((tile tile))
  (and (tx tile) (ty tile) (zoom tile)))

(defmethod tile-url ((tile tile))
  (if (valid-tilep tile)
      (format-url (coords tile) tile)
      (error 'tile-invalid-error)))

(define-condition tile-invalid-error (error)
  ())


(defclass tiles-coords ()
  ((world-size
    :initarg :world-size
    :reader world-size)
   (tile-size
    :initform 8
    :reader tile-size)
   (max-zoom
    :initarg :max-zoom
    :initform (error "You must provide max-zoom")
    :reader max-zoom)
   (version
    :initarg :version
    :initform nil
    :reader version)))


(defclass yandex-coords (tiles-coords)
  ((yandex-a
    :initform 20037508.342789d0
    :reader yandex-a)
   (yandex-e
    :initform 0.0818191908426d0
    :reader yandex-e)
   (yandex-f
    :initform 53.5865938d0
    :reader yandex-f)
   (yandex-rn
    :initform 6378137.0d0
    :reader yandex-rn)
   (max-zoom
    :initform 23)
   (version
    :initform "2.2.3")))
    

(defgeneric latlon2units (coord latlon)
  (:documentation "Convert lat-lon pair to coordinate system's coords pair"))

(defgeneric units2tile (coord units zoom)
  (:documentation "Convert units pair to tile object"))

(defgeneric latlon2tile (coord units zoom)
  (:documentation "Convert lat-lon pair to tile object"))

(defgeneric format-url (coord tile)
  (:documentation "Return URL of tile for given coordinate system"))

(defgeneric tiles-for-region (coord up-latlon dn-latlon zoom)
  (:documentation "Return array of tiles objects for region"))


(define-condition invalid-latlon-error (error)
  ((latlon :initarg :latlon :reader latlon)))

(define-condition invalid-units-error (error)
  ((units :initarg :units :reader units)))


(defun deg2rad (val)
  :documentation "Convert degree to radians"
  (* val (/ pi 180)))


(defun rad2deg (val)
  :documentation "Convert radians to degree"
  (* val (/ 180 pi)))


(defmethod units2tile ((coord tiles-coords) units zoom)
  (restart-case
      (progn
        (unless (and (listp units)
                     (= 2 (length units)))
          (error 'invalid-units-error :units units))
        (let* ((pow (expt 2 (+ zoom (tile-size coord))))
               (tile (map 'list #'(lambda (n)
                                    (truncate (/ n pow))) units)))
          (make-instance 'tile :coords coord :tx (car tile) :ty (cadr tile) :zoom zoom)))
    (show-error-message (err)
      (format t "Invalid units passed: ~a~%" (units err)))))


(defmethod latlon2tile ((coord tiles-coords) latlon zoom)
  (units2tile coord (latlon2units coord latlon) zoom))


(defun latlon2units-yandex (coord latlon)
  (let* ((lat (car latlon))
         (lon (cadr latlon))
         (pi4 (/ pi 4))
         (rlat (deg2rad lat))
         (rlon (deg2rad lon))
         (tmp (tan (+ pi4 (/ rlat 2))))
         (pow_tmp (expt (tan (+ pi4 (/ (asin (* (yandex-e coord) 
                                                (sin rlat))) 
                                       2))) 
                        (yandex-e coord))))
    (map 'list #'(lambda (v)
                   (truncate (* (yandex-f coord)
                                (+ v (yandex-a coord)))))
         (list (* (yandex-rn coord) rlon) 
               (* -1 
                  (yandex-rn coord)
                  (log (/ tmp pow_tmp)))))))


(defmethod latlon2units ((coord yandex-coords) latlon)
  (restart-case 
      (progn
        (unless (and (listp latlon)
                     (= 2 (length latlon)))
          (error 'invalid-latlon-error :latlon latlon))
        (latlon2units-yandex coord latlon))
      (show-error-message (err) 
        (format t "Invalid latlon passed: ~a~%" (latlon err)))))


(defmethod format-url ((coord yandex-coords) (tile tile))
  (format nil "http://vec.maps.yandex.net/tiles?l=map&v=~a&x=~d&y=~d&z=~d" 
          (version coord) (tx tile) (ty tile) (- (max-zoom coord) (zoom tile))))


(defmethod tiles-for-region ((coord tiles-coords) up-latlon dn-latlon zoom)
  (let* ((up (latlon2tile coord up-latlon zoom))
         (dn (latlon2tile coord dn-latlon zoom))
         (ux (min (tx up) (tx dn)))
         (uy (min (ty up) (ty dn)))
         (dx (max (tx up) (tx dn)))
         (dy (max (ty up) (ty dn))))
    (loop for ty from uy to dy
         append (loop for tx from ux to dx
                   collect (make-instance 'tile :coords coord :tx tx :ty ty :zoom zoom)))))

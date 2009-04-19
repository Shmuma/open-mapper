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
    :accessor zoom)
   (data
    :initarg :data
    :initform nil
    :reader data)))


(defgeneric tile-url (tile layer)
  (:documentation "Returns url of tile with specified layer"))

(defun valid-tilep (tile)
  :documentation "Checks tile for validity"
  (and (tx tile) (ty tile) (zoom tile)))

(defun have-datap (tile)
  :documentation "Checks that tile has pixmap data"
  (if (data tile) t nil))

(defmethod tile-url ((tile tile) layer)
  (if (valid-tilep tile)
      (format-url (coords tile) tile layer)
      (error 'tile-invalid-error)))

(define-condition tile-invalid-error (error)
  ())


(defclass coord-system ()
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


(defclass coord-system-yandex (coord-system)
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
    

(defgeneric latlon->units (coord latlon)
  (:documentation "Convert lat-lon pair to coordinate system's coords pair"))

(defgeneric units->tile (coord units zoom)
  (:documentation "Convert units pair to tile object"))

(defgeneric format-url (coord tile layer)
  (:documentation "Return URL of tile for given coordinate system and layer kind"))


(define-condition invalid-latlon-error (error)
  ((latlon :initarg :latlon :reader latlon)))

(define-condition invalid-units-error (error)
  ((units :initarg :units :reader units)))

(define-condition bad-layer-error (error)
  ((units :initarg :layer :reader layer)))


(defun deg2rad (val)
  :documentation "Convert degree to radians"
  (* val (/ pi 180)))


(defun rad2deg (val)
  :documentation "Convert radians to degree"
  (* val (/ 180 pi)))


(defmethod units->tile ((coord coord-system-yandex) units zoom)
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


(defun latlon->units-yandex (coord latlon)
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


(defmethod latlon->units ((coord coord-system-yandex) latlon)
  (restart-case 
      (progn
        (unless (and (listp latlon)
                     (= 2 (length latlon)))
          (error 'invalid-latlon-error :latlon latlon))
        (latlon->units-yandex coord latlon))
      (show-error-message (err) 
        (format t "Invalid latlon passed: ~a~%" (latlon err)))))


(defun latlon->tile (coord latlon zoom)
  (units->tile coord (latlon->units coord latlon) zoom))




(defmethod format-url ((coord yandex-coords) (tile tile) layer)
     (format nil 
             (ecase (layer) 
               (vector "http://vec.maps.yandex.net/tiles?l=map&v=~a&x=~d&y=~d&z=~d")
               (nil (error bad-layer-error :layer layer)))
             (version coord) (tx tile) (ty tile) (- (max-zoom coord) (zoom tile))))


;; (defmethod tiles-for-region ((coord tiles-coords) up-latlon dn-latlon zoom)
;;   (let* ((up (latlon2tile coord up-latlon zoom))
;;          (dn (latlon2tile coord dn-latlon zoom)))
;;     (loop for ty from (min (ty up) (ty dn)) to (max (ty up) (ty dn))
;;          append (loop for tx from (min (tx up) (tx dn)) to (max (tx up) (tx dn))
;;                    collect (make-instance 'tile :coords coord :tx tx :ty ty :zoom zoom)))))

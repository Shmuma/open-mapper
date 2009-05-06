(defpackage :shmuma.mapper.tiles
  (:use :common-lisp)
  (:export :tile :tx :ty :zoom
           :coord-system :min-zoom :max-zoom :layer
           :coord-system-yandex
           :make-coord-yandex
           :cs-layers
           :latlon->units
           :units->tile
           :latlon->tile
           :get-tile-url
           :loop-tiles)
)

(in-package :shmuma.mapper.tiles)


(defclass tile ()
  ((coord-system
    :initarg :coord-system
    :initform (error "You must provide coordinate system")
    :reader coord-system
    :documentation "Coordinate system this tile belongs")
   (tx
    :initarg :tx
    :initform nil
    :accessor tx
    :documentation "Tile X coordinate")
   (ty
    :initarg :ty
    :initform nil
    :accessor ty
    :documentation "Tile Y coordinate")
   (zoom
    :initarg :zoom
    :initform nil
    :accessor zoom
    :documentation "Tile zoom level"))
  (:documentation "Single tile with X, Y and Z in given coordinate system."))


(defclass coord-system ()
  ((world-size
    :initarg :world-size
    :reader world-size
    :allocation :class)
   (tile-size
    :initform 8
    :reader tile-size
    :allocation :class)
   (min-zoom
    :initarg :min-zoom
    :reader min-zoom
    :allocation :class)
   (max-zoom
    :initarg :max-zoom
    :reader max-zoom
    :allocation :class)
   (layer
    :initarg :layer
    :initform nil
    :accessor layer
    :documentation "Layer this coord system represents")))



(defgeneric cs-layers (coord)
  (:documentation "Get list of known layers of given coordinate system"))


(defclass coord-system-yandex (coord-system)
  ((yandex-a
    :initform 20037508.342789d0
    :reader yandex-a
    :allocation :class)
   (yandex-e
    :initform 0.0818191908426d0
    :reader yandex-e
    :allocation :class)
   (yandex-f
    :initform 53.5865938d0
    :reader yandex-f
    :allocation :class)
   (yandex-rn
    :initform 6378137.0d0
    :reader yandex-rn
    :allocation :class)
   (world-size
    :initform #x7FFFFFFF)
   (min-zoom
    :initform 4)
   (max-zoom
    :initform 23)))


;; constructors
(defun make-tile (cs x y z)
  (make-instance 'tile :coord-system cs :tx x :ty y :zoom z))

(defun make-coord-yandex (layer)
  (make-instance 'coord-system-yandex :layer layer))
    

;; generics
(defgeneric latlon->units (coord latlon)
  (:documentation "Convert lat-lon pair to coordinate system's coords pair"))

(defgeneric units->tile (coord units zoom)
  (:documentation "Convert units pair to tile object"))

(defgeneric tile->url (coord layer tile)
  (:documentation "Return URL of tile for given coordinate system and layer kind"))

(defgeneric layer-version (coord layer)
  (:documentation "Return string with latest known version of this layer"))


(define-condition invalid-latlon-error (error)
  ((latlon :initarg :latlon :reader latlon)))

(define-condition invalid-units-error (error)
  ((units :initarg :units :reader units)))

(define-condition bad-layer-error (error)
  ((layer :initarg :layer :reader layer)))


(defun deg2rad (val)
  :documentation "Convert degree to radians"
  (* val (/ pi 180)))


(defun rad2deg (val)
  :documentation "Convert radians to degree"
  (* val (/ 180 pi)))


(defmethod cs-layers ((coord coord-system-yandex))
  '(vector satellite traffic hybrid))


(defmethod units->tile ((coord coord-system-yandex) units zoom)
  (restart-case
      (progn
        (unless (and (listp units)
                     (= 2 (length units)))
          (error 'invalid-units-error :units units))
        (let* ((pow (expt 2 (+ zoom (tile-size coord))))
               (tile (map 'list #'(lambda (n)
                                    (truncate (/ n pow))) units)))
          (make-tile coord (car tile) (cadr tile) zoom)))
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


;; layer versions
(defmethod layer-version ((coord coord-system-yandex) (layer (eql 'vector)))
  "2.4.2")

(defmethod layer-version ((coord coord-system-yandex) (layer (eql 'satellite)))
  "1.8.0")

(defmethod layer-version ((coord coord-system-yandex) (layer (eql 'hybrid)))
  "2.4.2")


;; url formatting methods for yandex
(defmethod tile->url ((coord coord-system-yandex) (layer (eql 'vector)) (tile tile))
  (format nil "http://vec.maps.yandex.net/tiles?l=map&v=~a&x=~d&y=~d&z=~d"
          (layer-version coord layer)
          (tx tile)
          (ty tile)
          (- (max-zoom coord) (zoom tile))))


(defmethod tile->url ((coord coord-system-yandex) (layer (eql 'satellite)) (tile tile))
  (format nil "http://sat.maps.yandex.net/tiles?l=sat&v=~a&x=~d&y=~d&z=~d"
          (layer-version coord layer)
          (tx tile)
          (ty tile)
          (- (max-zoom coord) (zoom tile))))


(defmethod tile->url ((coord coord-system-yandex) (layer (eql 'hybrid)) (tile tile))
  (format nil "http://vec.maps.yandex.net/tiles?l=skl&v=~a&x=~d&y=~d&z=~d"
          (layer-version coord layer)
          (tx tile)
          (ty tile)
          (- (max-zoom coord) (zoom tile))))


;; nice utility methods
(defmethod print-object ((tile tile) s)
  (format s "#<~a X=~d Y=~d Z=~d>" (class-name (class-of tile))
          (tx tile) (ty tile) (zoom tile)))


(defmethod print-object ((cs coord-system-yandex) s)
  (format s "#<~a layers: (~{~a~^, ~})>" (class-name (class-of cs))
          (cs-layers cs)))


(defmacro loop-tiles ((coord from-latlon to-latlon zoom tile-var) body)
  :documentation "Iterates over all tiles of specified region, zoom and coordinate system"
  (let ((from (gensym))
        (to (gensym))
        (cs (gensym))
        (min-tx (gensym))
        (min-ty (gensym))
        (max-tx (gensym))
        (max-ty (gensym))
        (x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,cs ,coord)
            (,z ,zoom)
            (,from (latlon->tile ,cs ,from-latlon ,z))
            (,to (latlon->tile ,cs ,to-latlon ,z))
            (,min-tx (min (tx ,from) (tx ,to)))
            (,min-ty (min (ty ,from) (ty ,to)))
            (,max-tx (max (tx ,from) (tx ,to)))
            (,max-ty (max (ty ,from) (ty ,to))))
       (loop for ,y from ,min-ty to ,max-ty
            do (loop for ,x from ,min-tx to ,max-tx
                    do (let ((,tile-var (make-tile ,cs ,x ,y ,z)))
                         ,body))))))


(defun get-tile-url (tile)
  (let* ((coord (coord-system tile))
         (layer (layer coord)))
    (tile->url coord layer tile)))

(defpackage :shmuma.mapper.coords
  (:use :common-lisp)
  (:export :tiles-coords
           :yandex-coords
           :latlon2units
           :units2tiles
           :latlon2tiles
           :format-url))

(in-package :shmuma.mapper.coords)


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

(defgeneric units2tiles (coord units zoom)
  (:documentation "Convert units pair to tile coordinates"))

(defgeneric latlon2tiles (coord units zoom)
  (:documentation "Convert lat-lon pair to tiles coordinates"))

(defgeneric format-url (coord tx ty zoom)
  (:documentation "Return URL for given coordinate system"))



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


(defmethod units2tiles ((coord tiles-coords) units zoom)
  (restart-case
      (progn
        (unless (and (listp units)
                     (= 2 (length units)))
          (error 'invalid-units-error :units units))
        (let ((pow (expt 2 (+ zoom (tile-size coord)))))
          (map 'list #'(lambda (n)
                         (truncate (/ n pow)))
               units)))
    (show-error-message (err)
      (format t "Invalid units passed: ~a~%" (units err)))))


(defmethod latlon2tiles ((coord tiles-coords) latlon zoom)
  (units2tiles coord (latlon2units coord latlon) zoom))


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


(defmethod format-url ((coord yandex-coords) tx ty zoom)
  (format nil "http://vec.maps.yandex.net/tiles?l=map&v=~s&x=~d&y=~d&z=~d" 
          (version coords) tx ty (- (max-zoom coord) zoom)))


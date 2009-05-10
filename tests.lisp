(defpackage :shmuma.mapper.tests
  (:use :common-lisp :shmuma.mapper.tiles :shmuma.mapper.storage)
  (:export :test-coords
           :test-tiles))

(in-package :shmuma.mapper.tests)


(defun show-error-message (err)
  (invoke-restart 'show-error-message err))


(defun convert (latlon &optional (coord (make-coord-yandex 'vector)))
    (handler-bind ((invalid-latlon-error 
                    #'show-error-message)
                   (invalid-units-error
                    #'show-error-message))
      (let* ((units (latlon->units coord latlon)))
        (format t "Lat: ~5$, Lon: ~5$~%" (car latlon) (cadr latlon))
        (format t "UX: ~d, UY: ~d~%~%" (car units) (cadr units))
        (loop for z from (min-zoom coord) to (max-zoom coord)
             do (let ((tile (units->tile coord units z)))
                  (format t "T[~2d]: (~7d,~7d)~%" z (tx tile) (ty tile)))))))


(defun test-coords ()
  (let ((latlon '(55.80744 37.56762))
        (coord (make-instance 'coord-system-yandex)))
    (convert latlon coord)))


(defun test-tiles ()
  (let ((coord (make-coord-yandex 'vector))
        (urls nil))
    (loop-tiles (coord '(55.80744d0 37.56762d0) '(55.90744d0 37.66762d0) 8 tile)
       (push (get-tile-url tile) urls))
    urls))


;; download kremlin tile in all available zooms
(defun test-http-storage (&optional (dest-dir "/tmp"))
  (let* ((cs (make-coord-yandex 'vector))
         (kremlin-ll '(55.752425 37.618731))
         (units (latlon->units cs kremlin-ll))
         (src-stg (make-http-storage))
         (dst-stg (make-file-storage dest-dir)))
    (loop for z from (min-zoom cs) to (max-zoom cs)
         do (let* ((tile (units->tile cs units z))
                   (src-ptr (tile-to-storage-ptr src-stg tile))
                   (dst-ptr (tile-to-storage-ptr dst-stg tile)))
;              (print-object t src-ptr)
;              (format t "~a -> ~a~%" src-ptr dst-ptr)))))
              (let ((pixmap (get-pixmap src-stg src-ptr)))
                (unless (null pixmap)
                  (put-pixmap dst-stg dst-ptr pixmap)))))))

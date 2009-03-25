(defpackage :shmuma.mapper.tests
  (:use :common-lisp :shmuma.mapper.coords :shmuma.mapper.tiles)
  (:export :test-coords
           :test-tiles))



(defun show-error-message (err)
  (invoke-restart 'show-error-message err))


(defun convert (latlon &optional (coord (make-instance 'yandex-coords)))
    (handler-bind ((invalid-latlon-error 
                    #'show-error-message)
                   (invalid-units-error
                    #'show-error-message))
      (let* ((units (latlon2units coord latlon)))
        (format t "Lat: ~5$, Lon: ~5$~%" (car latlon) (cadr latlon))
        (format t "UX: ~d, UY: ~d~%~%" (car units) (cadr units))
        (loop for z from 4 to 20
             do (let ((tiles (units2tiles coord units z)))
                  (format t "TX[~2d]: ~7d, TY[~2d]: ~7d~%" z (car tiles) z (cadr tiles)))))))


(defun test-coords ()
  (let ((latlon '(55.80744 37.56762))
        (coord (make-instance 'yandex-coords)))
    (convert latlon coord)
    (latlon2tiles coord latlon 8)))


(defun test-tiles ()
  (tiles-for-region (make-instance 'tiles :coords (make-instance 'yandex-coords))
                    '(55.80744d0 37.56762d0)
                    '(55.90744d0 37.66762d0)
                    8))



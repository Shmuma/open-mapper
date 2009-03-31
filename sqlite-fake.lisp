(defpackage :sqlite-fake
  (:use :cl)
  (:export :sqlite-open))

(in-package :sqlite-fake)


(defun sqlite-open (name &optional (mode 0))
  (format t "sqlite-fake: open ~a (~d)~%" name mode))
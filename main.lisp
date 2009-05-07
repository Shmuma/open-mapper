(defpackage :shmuma.mapper.main
  (:use :common-lisp
        :shmuma.mapper.tiles
        :shmuma.mapper.storage
        :shmuma.mapper.tests))

(in-package :shmuma.mapper.main)


(defun main ()
  (show-usage)
)


(defmacro strcat (&rest args)
  `(concatenate 'string ,@args))


(defun show-usage ()
  (format t (strcat
      "Usage: open-mapper -r region-spec -z zoom-spec -sd src-drv -s src -dd dst-drv -d dst~%"
      "~%"
      "Options:~%"
      "  -r	- region spec as a list: (ul-lat ul-lon dr-lat dr-lon)~%"
      "  -z	- zoom spec as from-to or a comma-separated list~%"
      "  -sd	- source driver or help for a list~%"
      "  -s	- source options (see -sd help for a list)~%"
      "  -dd	- destination driver or help for a list~%"
      "  -d	- destination options (see -dd help for a list)~%"
      "~%")))
;;; -*- Mode: Lisp -*-

(defsystem fit
  :serial t
  :depends-on (com.gigamonkeys.binary-data
               alexandria cxml)
  :components ((:file "packages")
               (:file "data")
               (:file "fit")))

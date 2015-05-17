(eval-when (:compile-toplevel :load-toplevel :execute)
  (:asd :alexandria))

(defpackage garmin
  (:use :cl)
  (:export
   #:process-fr
   #:process-edge))

(in-package :garmin)

(defvar *config-file* (merge-pathnames ".config/garmin-processor"
                                       (user-homedir-pathname)))

(defvar *activities-directories* nil)

(defvar *device-type* :edge
  ":edge or :fr")
(defvar *processed* nil)

(defvar *save-to* #p"~/doc/garmin-fit/")

(defun process-file (file)
  (let* ((name (format nil "~(~a~)-~a" *device-type*
                       (pathname-name file)))
         (tmp-path (make-pathname :name name
                                  :type "tfit"
                                  :defaults *save-to*)))
    (alexandria:copy-file file tmp-path)
    (rename-file tmp-path (make-pathname :name name
                                         :type "fit"
                                         :defaults *save-to*)))
  (pushnew (file-namestring file) (getf *processed* *device-type*)
           :test #'equal))

(defun read-config ()
  (with-standard-io-syntax
    (with-open-file (stream *config-file*)
      (destructuring-bind (&key processed
                                path)
          (read stream)
        (setf *processed* processed
              *activities-directories* path)
        (values)))))

(defun write-config ()
  (with-standard-io-syntax
    (let (*print-pretty*)
     (with-open-file (stream *config-file* :direction :output
                                           :if-exists :supersede)
       (prin1 (list :path *activities-directories*
                    :processed *processed*)
              stream)
       (values)))))

(defun files-to-process ()
  (set-difference (directory (merge-pathnames "*.fit"
                                              (getf *activities-directories*
                                                    *device-type*)))
                  (getf *processed* *device-type*)
                  :test #'equal :key #'file-namestring))

(defun process ()
  (read-config)
  (let ((files-to-process (files-to-process)))
    (cond (files-to-process
           (mapc #'process-file files-to-process)
           (write-config)
           (throw 'exit 0))
          (t
           (write-line "Nothing to process")
           (throw 'exit 1)))))

(defun process-fr ()
  (let ((*device-type* :fr))
    (process)))

(defun process-edge ()
  (let ((*device-type* :edge))
    (process)))

#+()
(sb-ext:save-lisp-and-die "do-garmin-process"
                          :toplevel
                          (lambda ()
                            (let ((code 1))
                              (unwind-protect
                                   (setf code
                                         (catch 'exit
                                           (if (equal (file-namestring
                                                       (car sb-ext:*posix-argv*))
                                                      "pgfr")
                                               (garmin:process-fr)
                                               (garmin:process-edge))
                                           0))
                                (sb-ext:exit :code code))))
                          :executable t)

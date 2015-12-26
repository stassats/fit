(in-package :fit)

(defun semicircle-to-degrees (x)
  (float (/ x (/ (expt 2 31) 180)) 1d0))

(defun convert-to-gpx (input output)
  (let* ((parsed (parse-fit input))
         (output (if (pathname-name output)
                     output
                     (make-pathname :name (format nil "activity_~a"
                                                  (- (cdr (assoc :start-time (cdr (or (assoc :session parsed)
                                                                                      (assoc :lap parsed)))))
                                                     +date-time-offset+))
                                    :type "gpx"
                                    :defaults output))))
    (with-open-file (stream output :direction :output :if-exists :supersede)
      (cxml:with-xml-output (cxml:make-character-stream-sink stream)
        (cxml:with-element "gpx"
          (cxml:attribute "xmlns" "http://www.topografix.com/GPX/1/1")
          (cxml:with-element "trk"
            (cxml:with-element "trkseg"
              (loop for (type . fields) in parsed
                    when (eq type :record)
                    do
                    (let ((lat (cdr (assoc :position-lat fields)))
                          (lon (cdr (assoc :position-long fields))))
                      (when (and lat lon)
                        (cxml:with-element "trkpt"
                      
                          (cxml:attribute "lat"
                                          (format nil "~f" (semicircle-to-degrees lat)))
                          (cxml:attribute "lon"
                                          (format nil "~f"(semicircle-to-degrees lon))))))))))))
    output))

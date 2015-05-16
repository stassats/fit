(in-package :fit)

(define-binary-type u1 () (integer :bytes 1))
(define-binary-type u2 () (integer :bytes 2))
(define-binary-type u4 () (integer :bytes 4))

(define-binary-type integer (bytes)
  (:reader (in)
           (loop with value = 0
                 for lsb to (* 8 (1- bytes)) by 8 do
                 (setf (ldb (byte 8 lsb) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for lsb to (* 8 (1- bytes)) by 8
                 do (write-byte (ldb (byte 8 lsb) value) out))))

(define-binary-type be-integer (bytes)
  (:reader (in)
           (loop with value = 0
                 for msb from (* 8 (1- bytes)) downto 8 by 8
                 do (setf (ldb (byte 8 msb) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)))

(define-binary-type ascii-string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (loop for i below length
                   do (setf (char string i)
                            (code-char (read-byte in))))
             string))
  (:writer (out value)))

(define-binary-type crc (length)
  (:reader (in)
           (loop repeat length
                 sum (read-byte in)))
  (:writer (out value)))

(define-binary-class fit-header ()
                     ((header-length u1)
                      (version u1)
                      (profile u2)
                      (data-length u4)
                      (magic (ascii-string :length 4))
                      (crc (crc :length (- header-length 12)))))

(defvar *definitions*)
(defvar *data*)

(defun parse-fit (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((header (make-instance 'fit-header))
          (*definitions* (make-hash-table))
          *data*)
      (read-object header stream)
      (assert (equal (magic header) ".FIT"))
      (loop while (< (file-position stream)
                     (- (data-length header) 2))
            do
            (read-data stream))
      *data*)))

(defun read-header (stream)
  (let* ((byte (read-byte stream))
         (time (ldb-test (byte 1 7) byte)))
    (declare (type (unsigned-byte 8) byte))
    (if time
        (values :time
                (ldb (byte 5 0) byte)
                (ldb (byte 2 5) byte))
        (values :normal
                (if (ldb-test (byte 1 6)  byte)
                    :definition
                    :data)
                (ldb (byte 4 0) byte)))))

(define-binary-class field-definition ()
                     ((field-number u1)
                      (size u1)
                      (base-type u1)
                      (name)
                      (parser)
                      (scale)))

(define-binary-type field (length)
  (:reader (in)
           (let ((vector (make-array length)))
             (loop for i below length
                   for def = (make-instance 'field-definition)
                   do
                   (setf (svref vector i) def)
                   (read-object def in))
             vector))
  (:writer (out value)
           (write-sequence value out)))

(define-binary-class definition-message ()
                     ((reserved u1)
                      (architecture u1)
                      (message-number u2)
                      (fields-length u1)
                      (fields (field :length fields-length))
                      (message-type)
                      (field-types)))

(defconstant +date-time-offset+ (encode-universal-time 0 0 0 31 12 1989 0))

(defvar *parsers* nil)

(defmacro define-parser (type args &body body)
  (let ((name (alexandria:symbolicate 'parse- type)))
    `(progn
       (defun ,name ,args
         ,@body)
       (setf (getf *parsers* ,type) #',name)
       ',name)))

(define-parser :date-time (value)
  (declare (type (unsigned-byte 32) value))
  (+ +date-time-offset+ value))

(define-parser :lap-trigger (value)
  (cdr (assoc value
              '((0 . :manual) (1 . :time) (2 . :distance) (3 . :position-start)
                (4 . :position-lap) (5 . :position-waypoint) (6 . :position-marked)
                (7 . :session-end) (8 . :fitness-equipment)))))

(define-parser :event-type (value)
  (cdr (assoc value
              '((0 . :start) (1 . :stop) (2 . :consecutive-depreciated) (3 . :marker)
                (4 . :stop-all) (5 . :begin-depreciated) (6 . :end-depreciated)
                (7 . :end-all-depreciated) (8 . :stop-disable) (9 . :stop-disable-all)))))

(define-parser :event (value)
  (cdr (assoc value
              '((0 . :timer) (3 . :workout) (4 . :workout-step) (5 . :power-down)
                (6 . :power-up) (7 . :off-course) (8 . :session) (9 . :lap)
                (10 . :course-point) (11 . :battery) (12 . :virtual-partner-pace)
                (13 . :hr-high-alert) (14 . :hr-low-alert) (15 . :speed-high-alert)
                (16 . :speed-low-alert) (17 . :cad-high-alert) (18 . :cad-low-alert)
                (19 . :power-high-alert) (20 . :power-low-alert) (21 . :recovery-hr)
                (22 . :battery-low) (23 . :time-duration-alert)
                (24 . :distance-duration-alert) (25 . :calorie-duration-alert)
                (26 . :activity) (27 . :fitness-equipment) (28 . :length) (32 . :user-marker)
                (33 . :sport-point) (36 . :calibration) (42 . :front-gear-change)
                (43 . :rear-gear-change) (44 . :rider-position-change) (45 . :elev-high-alert)
                (46 . :elev-low-alert)))))

(define-parser :sport (value)
  (cdr (assoc value
              '((0 . :generic) (1 . :running) (2 . :cycling) (3 . :transition)
                (4 . :fitness-equipment) (5 . :swimming) (6 . :basketball) (7 . :soccer)
                (8 . :tennis) (9 . :american-football) (10 . :training) (11 . :walking)
                (12 . :cross-country-skiing) (13 . :alpine-skiing) (14 . :snowboarding)
                (15 . :rowing) (16 . :mountaineering) (17 . :hiking) (18 . :multisport)
                (19 . :paddling) (20 . :flying) (21 . :e-biking) (254 . :all)))))

(define-parser :sub-sport (value)
  (cdr (assoc value
              '((0 . :generic) (1 . :treadmill) (2 . :street) (3 . :trail) (4 . :track)
                (5 . :spin) (6 . :indoor-cycling) (7 . :road) (8 . :mountain) (9 . :downhill)
                (10 . :recumbent) (11 . :cyclocross) (12 . :hand-cycling)
                (13 . :track-cycling) (14 . :indoor-rowing) (15 . :elliptical)
                (16 . :stair-climbing) (17 . :lap-swimming) (18 . :open-water)
                (19 . :flexibility-training) (20 . :strength-training) (21 . :warm-up)
                (22 . :match) (23 . :exercise) (24 . :challenge) (25 . :indoor-skiing)
                (26 . :cardio-training) (27 . :indoor-walking) (28 . :e-bike-fitness)
                (254 . :all)))))

(define-parser :sint32 (value)
  (logior value (- (mask-field (byte 1 31) value))))



(defun read-data-message (local stream &optional offset)
  (let* ((definition (gethash local *definitions*)))
    (push (cons (message-type definition)
                (loop for field across (fields definition)
                      for value = (funcall (parser field)
                                           (read-value 'integer stream
                                                       :bytes (size field)))
                      for scale = (scale field)
                      collect (cons (name field)
                                    (if scale
                                        (/ value scale)
                                        value))))
          *data*)))

(defun read-definition-message (local stream)
  (let ((definition (make-instance 'definition-message)))
    (read-object definition stream)
    (let* ((type (cdr (assoc (message-number definition) *message-types*)))
           (field-types (cdr (assoc type *message-subtypes*))))
      (setf (message-type definition) type
            (field-types definition) field-types)
      ;; no support for big-endidan
      (assert (zerop (architecture definition)))
      (loop for field across (fields definition)
            for (name type scale) = (cdr (assoc (field-number field) field-types))
            do (setf (parser field) (getf *parsers* type #'identity)
                     (scale field) (and (numberp scale)
                                        (/= scale 1)
                                        scale)
                     (name field) (or name
                                      (field-number field)))))
    (setf (gethash local *definitions*) definition)))

(defun read-data (stream)
  (multiple-value-bind (normal message-type local-message-type) (read-header stream)
    (cond ((eq normal :time)
           (read-data-message local-message-type stream message-type))
          ((eq message-type :definition)
           (read-definition-message local-message-type stream))
          ((eq message-type :data)
           (read-data-message local-message-type stream))
          (t
           (error "lose")))))

(defun smooth-max-cadence (data)
  (loop with previous = 0
        for (type . fields) in data
        for cadence = (cdr (assoc :cadence fields))
        when (and (eq type :record)
                  cadence
                  (< cadence 160)
                  (or (zerop previous)
                      (< (- cadence previous) 15)))
        maximize cadence
        and do (setf previous cadence)))

(defun summarize (file)
  (let* ((parsed (parse-fit file))
         (session (cdr (assoc :session parsed)))
         (type (cdr (assoc :sport session))))
    (list :type (ecase type
                  (:cycling :bike-ride)
                  (:running :run))
          :start-time (cdr (assoc :start-time session))
          :time (cdr (assoc :total-timer-time session))
          :elapsed-time (cdr (assoc :total-elapsed-time session))
          :distance (cdr (assoc :total-distance session))
          :avg-speed (cdr (assoc :avg-speed session))
          :max-speed (cdr (assoc :max-speed session))
          :avg-cadence (cdr (assoc :avg-cadence session))
          :max-cadence (smooth-max-cadence parsed)
          :avg-hr (cdr (assoc :avg-heart-rate session))
          :max-hr (cdr (assoc :max-heart-rate session)))))

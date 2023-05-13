#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.wav
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:wave-format-error
   #:file
   #:source
   #:in-memory-source
   #:file-drain))
(in-package #:org.shirakumo.fraf.mixed.wav)

(define-condition wave-format-error (error)
  ((file :initarg :file :initform NIL :accessor file)))

(define-condition bad-header (wave-format-error)
  ()
  (:report (lambda (c s) (format s "The file~%  ~a~%is not a valid RIFF file: bad header."
                                 (file c)))))

(define-condition unsupported-audio-format (wave-format-error)
  ((audio-format :initarg :audio-format :accessor audio-format))
  (:report (lambda (c s) (format s "The file~%  ~a~%contains the audio format ~d which is unsupported."
                                 (file c) (audio-format c)))))

(define-condition missing-block (wave-format-error)
  ((block-type :initarg :block-type :accessor block-type))
  (:report (lambda (c s) (format s "The file~%  ~a~%is missing the required block type ~a"
                                 (file c) (block-type c)))))

(defun evenify (int)
  (if (evenp int)
      int
      (1+ int)))

(defun decode-int (stream size)
  (let ((int 0))
    (dotimes (i size int)
      (setf (ldb (byte 8 (* 8 i)) int) (read-byte stream)))))

(defun decode-label (stream)
  (map-into (make-string 4) (lambda () (code-char (read-byte stream)))))

(defun check-label (stream label file)
  (let ((found (decode-label stream)))
    (unless (string= found label)
      (error 'bad-header :file file))))

(defun decode-block-type (type stream)
  (when (string= type "fmt ")
    (list :audio-format (decode-int stream 2)
          :channels (decode-int stream 2)
          :samplerate (decode-int stream 4)
          :byterate (decode-int stream 4)
          :block-align (decode-int stream 2)
          :bits-per-sample (decode-int stream 2))))

(defun decode-block (stream)
  (let ((label (decode-label stream))
        (start (file-position stream))
        (size (decode-int stream 4)))
    (prog1 (list* :label label
                  :start (+ start 4)
                  :end (+ start size)
                  (decode-block-type label stream))
      (file-position stream (evenify (+ start size 4))))))

(defun determine-sample-format (format file)
  (case (getf format :audio-format)
    (1 (ecase (/ (getf format :bits-per-sample) 8)
         (1 :uint8)
         (2 :int16)
         (3 :int24)
         (4 :int32)))
    (3 :float)
    (T (error 'unsupported-audio-format :file file :audio-format (getf format :audio-format)))))

(defun decode-wav-header (stream)
  (check-label stream "RIFF" (pathname stream))
  (dotimes (i 4) (read-byte stream))
  (check-label stream "WAVE" (pathname stream))
  (let* ((blocks (loop for block = (ignore-errors (decode-block stream))
                       while block collect block))
         (format (find "fmt " blocks :key #'second :test #'string=))
         (data (find "data" blocks :key #'second :test #'string=)))
    (unless format
      (error 'missing-block :file (pathname stream) :block-type "FMT"))
    (unless data
      (error 'missing-block :file (pathname stream) :block-type "DATA"))
    (file-position stream (getf data :start))
    (values (getf format :channels)
            (getf format :samplerate)
            (determine-sample-format format (pathname stream))
            (getf data :start)
            (getf data :end))))

(defclass source (mixed:source)
  ((file :initarg :file :accessor file)
   (wav-stream :initform NIL :accessor wav-stream)
   (data-start :accessor data-start)
   (data-end :accessor data-end)))

(defmethod initialize-instance :after ((source source) &key file)
  (mixed:start source))

(defmethod mixed:free ((source source))
  (when (wav-stream source)
    (close (wav-stream source))
    (setf (wav-stream source) NIL)))

(defmethod mixed:start ((source source))
  (unless (wav-stream source)
    (let ((stream (open (file source) :direction :input
                                      :element-type '(unsigned-byte 8))))
      (setf (wav-stream source) stream)
      (multiple-value-bind (channels samplerate encoding start end) (decode-wav-header stream)
        (setf (mixed:samplerate (mixed:pack source)) samplerate)
        (setf (mixed:channels (mixed:pack source)) channels)
        (setf (mixed:encoding (mixed:pack source)) encoding)
        (setf (data-start source) start)
        (setf (data-end source) end)))))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (let* ((stream (wav-stream source))
             (avail (max 0 (min size (- (data-end source) (max (data-start source) (file-position stream))))))
             (read (- (read-sequence data stream :start start :end (+ start avail)) start)))
        (cond ((< 0 read)
               (incf (mixed:byte-position source) read)
               (mixed:finish read))
              ((<= (mixed:available-read (mixed:pack source)) 2)
               (setf (mixed:done-p source) T)))))))

(defmethod mixed:end ((source source))
  (close (wav-stream source))
  (setf (wav-stream source) NIL))

(defmethod mixed:seek-to-frame ((source source) position)
  (file-position (wav-stream source)
                 (min (data-end source)
                      (+ (data-start source) (max 0 (* position (mixed:framesize (mixed:pack source))))))))

(defmethod mixed:frame-count ((source source))
  (/ (- (data-end source) (data-start source)) (mixed:framesize (mixed:pack source))))

(defclass in-memory-source (mixed:source)
  ((buffer :accessor buffer)
   (index :initform 0 :accessor mixed:byte-position)
   (framesize :accessor mixed:framesize)))

(defmethod initialize-instance :after ((source in-memory-source) &key file)
  (with-open-file (stream file :direction :input
                               :element-type '(unsigned-byte 8))
    (multiple-value-bind (channels samplerate encoding start end) (decode-wav-header stream)
      (setf (mixed:samplerate (mixed:pack source)) samplerate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding)
      (setf (mixed:framesize source) (mixed:framesize (mixed:pack source)))
      (file-position stream start)
      (let ((buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        (setf (buffer source) buffer)))))

(defmethod mixed:start ((source in-memory-source)))

(defmethod mixed:mix ((source in-memory-source))
  (declare (optimize speed))
  (let ((pack (mixed:pack source)))
    (mixed:with-buffer-tx (data start size pack :direction :output)
      (when (< 0 size)
        (let* ((buffer (buffer source))
               (index (mixed:byte-position source))
               (avail (max 0 (min size (- (length buffer) index)))))
          (declare (type (simple-array (unsigned-byte 8) (*)) data buffer))
          (declare (type (unsigned-byte 32) index))
          (cond ((< 0 avail)
                 (replace data buffer :start1 start :start2 index :end1 (+ start avail))
                 (setf (mixed:byte-position source) (+ index avail))
                 (mixed:finish avail))
                ((<= (mixed:available-read pack) 2)
                 (setf (mixed:done-p source) T))))))))

(defmethod mixed:end ((source in-memory-source)))

(defmethod mixed:seek-to-frame ((source in-memory-source) position)
  (setf (mixed:byte-position source) (max 0 (min (length (buffer source)) (* position (mixed:framesize source))))))

(defmethod mixed:frame-position ((source in-memory-source))
  (/ (mixed:byte-position source)
     (mixed:framesize source)))

(defmethod mixed:frame-count ((source in-memory-source))
  (/ (length (buffer source))
     (mixed:framesize source)))

(define-condition unsupported-sample-format (wave-format-error)
  ((sample-format :initarg :sample-format :accessor sample-format))
  (:report (lambda (c s) (format s "Sample format~%  ~a~%is not supported."
                                 (sample-format c)))))

(defun write-label (stream label)
  (loop for char across label
        do (write-byte (char-code char) stream)))

(defun write-int (stream size int)
  ;; little endian
  (dotimes (i size int)
    (write-byte (ldb (byte 8 (* 8 i)) int) stream)))

(defun audio-format (sample-format)
  (case sample-format
    ((:uint8 :int16 :int24 :int32) 1)  ; PCM
    ((:float) 3)  ; IEEE FLOAT
    (T (error 'unsupported-sample-format :sample-format sample-format))))

(defun bytes-per-sample (sample-format)
  (ecase sample-format
    (:uint8 1)
    (:int16 2)
    (:int24 3)
    (:int32 4)
    (:float 4)))

(defun fmt-size (audio-format)
  (ecase audio-format
    (1 16)
    (3 18)))

(defclass file-drain (mixed:file-drain)
  ((riff-chunk-offset :initform NIL :accessor riff-chunk-offset)
   (fact-chunk-offset :initform NIL :accessor fact-chunk-offset)
   (data-chunk-offset :initform NIL :accessor data-chunk-offset)))

(defmethod mixed:start ((drain file-drain))
  (unless (riff-chunk-offset drain)  ; Check if we already started
    (with-slots ((stream mixed:stream) mixed:pack) drain
      (let* ((bytes-per-sample (bytes-per-sample (mixed:encoding mixed:pack)))
             (block-align (* (mixed:channels mixed:pack) bytes-per-sample))
             (byterate (* (mixed:samplerate mixed:pack) block-align))
             (audio-format (audio-format (mixed:encoding mixed:pack))))
        (write-label stream "RIFF")     ; RIFF chunk
        (setf (riff-chunk-offset drain) (file-position stream)) ; skip size field, save offset
        (write-int stream 4 0)
        (write-label stream "WAVE")
        (write-label stream "fmt ")     ; fmt chunk
        (write-int stream 4 (fmt-size audio-format))
        (write-int stream 2 audio-format)
        (write-int stream 2 (mixed:channels mixed:pack))
        (write-int stream 4 (mixed:samplerate mixed:pack))
        (write-int stream 4 byterate)
        (write-int stream 2 block-align)
        (write-int stream 2 (* 8 bytes-per-sample))
        (when (= audio-format 3)
          (write-int stream 2 2)
          (write-label stream "fact")   ; fact chunk
          (write-int stream 4 4)
          (setf (fact-chunk-offset drain) (file-position stream)) ; skip size field, save offset
          (write-int stream 4 0))
        (write-label stream "data")     ; data chunk
        (setf (data-chunk-offset drain) (file-position stream)) ; skip size field, save offset
        (write-int stream 4 0)))))

(defmethod mixed:end ((drain file-drain))
  (with-slots ((stream mixed:stream) mixed:pack) drain
    (let ((data-size (- (file-position stream)
                        (+ 4 (data-chunk-offset drain)))))
      ;; pad byte for data chunk
      (when (oddp data-size)
        (write-byte 0 stream))
      ;; Set RIFF chunk size (including pad byte)
      (let* ((position (file-position stream))
             (riff-size (- position
                           (+ 4 (riff-chunk-offset drain)))))
        (file-position stream (riff-chunk-offset drain))
        (write-int stream 4 riff-size)
        ;; Set number of samples in fact chunk when needed
        (when (fact-chunk-offset drain)
          (let ((samples (/ data-size (bytes-per-sample (mixed:encoding mixed:pack)))))
            (file-position stream (fact-chunk-offset drain))
            (write-int stream 4 samples)))
        ;; Set data chunk size
        (file-position stream (data-chunk-offset drain))
        (write-int stream 4 data-size)
        (when (mixed:dont-close-p drain)
          (file-position stream position))))))

;; Reset offsets
(defmethod mixed:end :after ((drain file-drain))
  (setf (data-chunk-offset drain) NIL
        (fact-chunk-offset drain) NIL
        (riff-chunk-offset drain) NIL))

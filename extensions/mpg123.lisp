#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.mpg123
  (:use #:cl)
  #+package-local-nicknames
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.mpg123)

(defclass source (mixed:source)
  ((file :initform NIL :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (file source) (cl-mpg123:make-file file :buffer-size NIL))
  ;; Early start to set pack properties
  (mixed:start source))

(defmethod mixed:free ((source source))
  (when (file source)
    (when (cl-mpg123:connected (file source))
      (cl-mpg123:disconnect (file source)))
    (setf (file source) NIL)))

(defmethod mixed:start ((source source))
  (unless (cl-mpg123:connected (file source))
    (cl-mpg123:connect (file source))
    (cl-mpg123:scan (file source))
    (multiple-value-bind (rate channels encoding) (cl-mpg123:file-format (file source))
      (setf (mixed:samplerate (mixed:pack source)) rate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding))))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (handler-bind ((cl-mpg123:read-failed
                       (lambda (e)
                         (case (cl-mpg123:error-code e)
                           (:new-format))
                         (continue e))))
        (let ((read (cl-mpg123:read-directly (file source) (mixed:data-ptr) size)))
          (cond ((< 0 read)
                 (incf (mixed:byte-position source) read)
                 (mixed:finish read))
                ((= 0 (mixed:available-read (mixed:pack source)))
                 (setf (mixed:done-p source) T))))))))

(defmethod mixed:end ((source source))
  (cl-mpg123:disconnect (file source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (cl-mpg123:seek (file source) position :mode :absolute :by :sample))

(defmethod mixed:frame-count ((source source))
  (cl-mpg123:sample-count (file source)))

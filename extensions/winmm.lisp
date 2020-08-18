#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.winmm
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:winmm #:org.shirakumo.fraf.mixed.winmm.cffi))
  (:export
   #:winmm-error
   #:code
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.winmm)

(define-condition winmm-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "WinMM error ~a" (code c)))))

(defun check-result (error)
  (unless (eql :ok error)
    (error 'winmm-error :code error)))

(defclass drain (mixed:drain)
  ((device :initform NIL :accessor device)
   (event :initform NIL :accessor event)
   (header :initform NIL :accessor header)))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library winmm:winmm))

(defmethod mixed:start ((drain drain))
  (unless (event drain)
    (let ((event (winmm:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer))))
      (if (cffi:null-pointer-p event)
          (error 'winmm-error :cod :event-creation-failed)
          (setf (event drain) event))))
  (unless (header drain)
    (setf (header drain) (cffi:foreign-alloc '(:struct winmm:wave-header)))
    (setf (winmm:wave-header-data (header drain)) (cffi:foreign-alloc :uint32 :count 512))
    (setf (winmm:wave-header-loops (header drain)) 0))
  (unless (device drain)
    (let ((pack (mixed:pack drain)))
      (cffi:with-foreign-objects ((device :pointer)
                                  (format '(:struct winmm:waveformat-ex)))
        (winmm:encode-wave-format format (mixed:samplerate pack) (mixed:channels pack) :int32)
        (check-result
         (winmm:wave-out-open device winmm:WAVE-MAPPER format (event drain) (cffi:null-pointer) '(:default-device :callback-event :allow-sync)))
        (multiple-value-bind (samplerate channels encoding) (winmm:decode-wave-format format)
          (setf (mixed:samplerate pack) samplerate)
          (setf (mixed:channels pack) channels)
          (setf (mixed:encoding pack) encoding))
        (setf (device drain) (cffi:mem-ref device :pointer))
        (setf (winmm:wave-header-buffer-length (header drain)) (* 512 (cffi:foreign-type-size :uint32)))
        (setf (winmm:wave-header-flags (header drain)) ())
        (check-result
         (winmm:wave-out-prepare (device drain) (header drain)
                                 (cffi:foreign-type-size '(:struct winmm:wave-header))))))))

(defmethod mixed:mix ((drain drain))
  (let ((device (device drain))
        (header (header drain)))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (let ((size (min 512 (- end start))))
        (when (winmm:wave-header-flags header)
          (loop until (find :done (winmm:wave-header-flags header))
                do (winmm:wait-for-single-object (event drain) 100)))
        (static-vectors:replace-foreign-memory (winmm:wave-header-data header) (mixed:data-ptr) size)
        (setf (winmm:wave-header-buffer-length header) size)
        (check-result
         (winmm:wave-out-write device header size))
        (mixed:finish size)))))

(defmethod mixed:end ((drain drain))
  (when (device drain)
    (winmm:wave-out-reset (device drain))
    (winmm:wave-out-close (device drain))
    (setf (device drain) NIL)))
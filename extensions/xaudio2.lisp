#|
This file is a part of cl-mixed
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.xaudio2
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:com #:org.shirakumo.com-on)
   (#:xaudio2 #:org.shirakumo.fraf.mixed.xaudio2.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.xaudio2)

(cffi:defcstruct (callback :conc-name callback-)
  (vtbl :pointer)
  (pack :pointer)
  (size :uint32)
  (callbacks (:struct xaudio2:voice-callback)))

(defclass drain (mixed:drain)
  ((interface :initform NIL :accessor interface)
   (master :initform NIL :accessor master)
   (source :initform NIL :accessor source)
   (callback :initform NIL :accessor callback)
   (version :initform NIL :accessor version)))

(defmethod mixed:free :after ((drain drain))
  (when (source drain)
    (xaudio2:voice-destroy (source drain))
    (setf (source drain) NIL))
  (when (master drain)
    (xaudio2:voice-destroy (master drain))
    (setf (master drain) NIL))
  (when (interface drain)
    (com:release (interface drain))
    (setf (interface drain) NIL))
  (when (callback drain)
    (cffi:foreign-free (callback drain))
    (setf (callback drain) NIL)))

(defmacro try-library (library &body body)
  `(block try-library
     (handler-case (cffi:use-foreign-library ,library)
       (error () (return-from try-library NIL)))
     ,@body))

(defmethod initialize-instance :after ((drain drain) &key)
  (com:init)
  (let* ((pack (mixed:pack drain))
         (callback (cffi:foreign-alloc '(:struct callback)))
         (vtbl (cffi:foreign-slot-pointer callback '(:struct callback) 'callbacks)))
    (setf (callback-vtbl callback) vtbl)
    (setf (callback-size callback) 0)
    (setf (callback-pack callback) (mixed:handle pack))
    (setf (xaudio2::%voice-callback-processing-pass-start vtbl) (cffi:callback processing-pass-start))
    (setf (xaudio2::%voice-callback-processing-pass-end vtbl) (cffi:callback processing-pass-end))
    (setf (xaudio2::%voice-callback-stream-end vtbl) (cffi:callback stream-end))
    (setf (xaudio2::%voice-callback-buffer-start vtbl) (cffi:callback buffer-start))
    (setf (xaudio2::%voice-callback-buffer-end vtbl) (cffi:callback buffer-end))
    (setf (xaudio2::%voice-callback-loop-end vtbl) (cffi:callback loop-end))
    (setf (xaudio2::%voice-callback-error vtbl) (cffi:callback error))
    (setf (callback drain) callback)
    (cffi:with-foreign-object (format '(:struct xaudio2:waveformat-extensible))
      (xaudio2:encode-wave-format format (mixed:samplerate pack) (mixed:channels pack) (mixed:encoding pack))
      (or (try-library xaudio2:xaudio2.9
            (setf (version drain) '2.9)
            (setf (interface drain) (com:with-deref (interface :pointer)
                                      (xaudio2:create interface 0 :default)))
            (setf (master drain) (com:with-deref (master :pointer)
                                   (xaudio2:2.9-interface-create-mastering-voice (interface drain) master (mixed:channels pack) (mixed:samplerate pack) 0 (cffi:null-pointer) (cffi:null-pointer) :other)))
            (setf (source drain) (com:with-deref (source :pointer)
                                   (xaudio2:2.9-interface-create-source-voice (interface drain) source format 0 1.0 callback (cffi:null-pointer) (cffi:null-pointer)))))
          (try-library xaudio2:xaudio2.7
            (setf (version drain) '2.7)
            (setf (interface drain) (com:create xaudio2:CLSID-XAUDIO2 xaudio2:IID-IXAUDIO2))
            (com:check-hresult (xaudio2:2.7-interface-initialize (interface drain) 0 :default))
            (setf (master drain) (com:with-deref (master :pointer)
                                   (xaudio2:2.7-interface-create-mastering-voice (interface drain) master (mixed:channels pack) (mixed:samplerate pack) 0 0 (cffi:null-pointer) :other)))
            (setf (source drain) (com:with-deref (source :pointer)
                                   (xaudio2:2.7-interface-create-source-voice (interface drain) source format 0 1.0 callback (cffi:null-pointer) (cffi:null-pointer)))))
          (error "Failed to find a suitable XAudio2 dll.")))))

(defmethod mixed:start ((drain drain))
  (ecase (version drain)
    (2.9 (xaudio2:2.9-interface-start-engine (interface drain)))
    (2.7 (xaudio2:2.7-interface-start-engine (interface drain))))
  (xaudio2:voice-start (source drain) 0 0))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    ;; FIXME: This amounts to a spin lock and is horribly inefficient.
    (when (and (< 0 size) (= 0 (callback-size (callback drain))))
      (cffi:with-foreign-object (buffer '(:struct xaudio2:buffer))
        (static-vectors:fill-foreign-memory buffer (load-time-value (cffi:foreign-type-size '(:struct xaudio2:buffer))) 0)
        (setf (xaudio2:buffer-size buffer) size)
        (setf (xaudio2:buffer-audio-data buffer) (mixed:data-ptr))
        (setf (callback-size (callback drain)) size)
        (com:check-hresult (xaudio2:voice-submit-source-buffer (source drain) buffer (cffi:null-pointer)))))))

(defmethod mixed:end ((drain drain))
  (xaudio2:voice-flush-source-buffers (source drain))
  (xaudio2:voice-stop (source drain) 0 0)
  (ecase (version drain)
    (2.9 (xaudio2:2.9-interface-stop-engine (interface drain)))
    (2.7 (xaudio2:2.7-interface-stop-engine (interface drain)))))

(cffi:defcallback processing-pass-start :void ((callback :pointer) (bytes :uint32))
  (declare (ignore callback bytes)))

(cffi:defcallback processing-pass-end :void ((callback :pointer))
  (declare (ignore callback)))

(cffi:defcallback stream-end :void ((callback :pointer))
  (declare (ignore callback)))

(cffi:defcallback buffer-start :void ((callback :pointer) (context :pointer))
  (declare (ignore callback context)))

(cffi:defcallback buffer-end :void ((callback :pointer) (context :pointer))
  (declare (ignore context))
  ;; Commit our read data here.
  (mixed-cffi:pack-finish-read (callback-size callback) (callback-pack callback))
  (setf (callback-size callback) 0))

(cffi:defcallback loop-end :void ((callback :pointer) (context :pointer))
  (declare (ignore callback context)))

(cffi:defcallback error :void ((callback :pointer) (error com:hresult))
  (declare (ignore callback))
  (format T "~&XAudio2 Error: ~a~%" (com:error-message error)))

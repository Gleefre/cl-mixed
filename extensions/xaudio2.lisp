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
   (#:xaudio2 #:org.shirakumo.fraf.mixed.xaudio2.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.xaudio2)

(cffi:defcstruct (callback :conc-name callback-)
  (vtbl :pointer)
  (pack :pointer)
  (callbacks (:struct xaudio2:voice-callback)))

(defclass drain (mixed:drain)
  ((interface :initform NIL :accessor interface)
   (master :initform NIL :accessor master)
   (source :initform NIL :accessor source)
   (callback :initform NIL :accessor callback)))

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

(defmethod initialize-instance :after ((drain drain) &key)
  (cffi:use-foreign-library xaudio2:xaudio2)
  (let* ((pack (pack (mixed:pack)))
         (callback (cffi:foreign-alloc 'callback))
         (vtbl (cffi:foreign-slot-pointer callback '(:struct callback) 'callbacks)))
    (setf (callback-vtbl callback) vtbl)
    (setf (xaudio2:voice-callback-processing-pass-start vtbl) (cffi:null-pointer))
    (setf (xaudio2:voice-callback-processing-pass-end vtbl) (cffi:null-pointer))
    (setf (xaudio2:voice-callback-stream-end vtbl) (cffi:null-pointer))
    (setf (xaudio2:voice-callback-buffer-start vtbl) (cffi:null-pointer))
    (setf (xaudio2:voice-callback-buffer-end vtbl) (cffi:callback buffer-end))
    (setf (xaudio2:voice-callback-loop-end vtbl) (cffi:null-pointer))
    (setf (xaudio2:voice-callback-error vtbl) (cffi:null-pointer))
    (setf (callback drain) callback)
    (setf (interface drain) (com:with-deref (interface :pointer)
                              (xaudio2:create interface 0 :default)))
    (setf (master drain) (com:with-deref (master :pointer)
                           (xaudio2:interface-create-mastering-voice (interface drain) master (mixed:channels pack) (mixed:samplerate pack) 0 (cffi:null-pointer) (cffi:null-pointer) :other)))
    (cffi:with-foreign-objects ((format '(:struct xaudio2:waveformat-extensible))
                                (chain '(:struct xaudio2:effect-chain)))
      (xaudio2:encode-wave-format format (mixed:samplerate pack) (mixed:channels pack) (mixed:encoding pack))
      (setf (xaudio2:effect-chain-effect-count chain) 0)
      (setf (source drain) (com:with-deref (source :pointer)
                             (xaudio2:interface-create-source-voice (interface drain) source format 0 1.0 callback (cffi:null-pointer) chain))))))

(defmethod mixed:start ((drain drain))
  (xaudio2:interface-start-engine (interface drain))
  (xaudio2:voice-start (source drain) 0 0)
  (setf (callback-pack (callback drain)) (mixed:handle (mixed:pack drain))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      (cffi:with-foreign-object ((buffer '(:struct xaudio2:buffer)))
        (static-vectors:fill-foreign-memory buffer (cffi:foreign-type-size '(:struct xaudio2:buffer)) 0)
        (setf (xaudio2:buffer-size buffer) size)
        (setf (xaudio2:buffer-audio-data buffer) (mixed:data-ptr))
        (setf (xaudio2:buffer-context buffer) size)
        (com:check-hresult (xaudio2:voice-submit-source-buffer buffer (cffi:null-pointer)))))))

(defmethod mixed:end ((drain drain))
  (xaudio2:voice-flush-source-buffers (source drain))
  (xaudio2:voice-stop (source drain) 0 0)
  (xaudio2:interface-stop-engine (interface drain)))

(cffi:defcallback buffer-end :void ((callback :pointer) (read :uint64))
  ;; Commit our read data here.
  (mixed-cffi:pack-finish-read (ldb (byte 32 0) read) (callback-pack callback)))

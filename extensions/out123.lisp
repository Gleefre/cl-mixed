#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.out123
  (:use #:cl)
  #+package-local-nicknames
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.out123)

(defclass drain (mixed:drain)
  ((out :initform NIL :accessor out)))

(defmethod initialize-instance :after ((drain drain) &key)
  (let* ((pack (mixed:pack drain))
         (out (cl-out123:make-output NIL :rate (mixed:samplerate pack) :channels (mixed:channels pack) :encoding :float)))
    (cl-out123:connect out)
    (setf (out drain) out)))

(defmethod mixed:free ((drain drain))
  (when (out drain)
    (cl-out123:disconnect (out drain))
    (cl-out123-cffi:del (cl-out123:handle (out drain)))
    (setf (out drain) NIL)))

(defmethod mixed:start ((drain drain))
  (cl-out123:start (out drain))
  (setf (mixed:samplerate (mixed:pack drain)) (cl-out123:rate (out drain)))
  (setf (mixed:encoding (mixed:pack drain)) (cl-out123:encoding (out drain)))
  (setf (mixed:channels (mixed:pack drain)) (cl-out123:channels (out drain))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (mixed:finish (cl-out123:play-directly (out drain) (mixed:data-ptr) size))))

(defmethod mixed:end ((drain drain))
  (cl-out123:stop (out drain)))

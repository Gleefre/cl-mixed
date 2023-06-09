#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass repeat (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment repeat) &key time mode samplerate bypass)
  (with-error-on-failure ()
    (mixed-cffi:make-segment-repeat (float time 0f0) samplerate (handle segment)))
  (when mode (setf (repeat-mode segment) mode))
  (setf (bypass segment) bypass))

(defun make-repeat (&rest args &key time mode samplerate)
  (declare (ignore time samplerate))
  (apply #'make-instance 'repeat args))

(define-field-accessor repeat-position repeat :float :repeat-position)
(define-field-accessor duration repeat :float :repeat-time)
(define-field-accessor repeat-mode repeat mixed-cffi:repeat-mode :repeat-mode)
(define-field-accessor samplerate repeat :float :samplerate)
(define-field-accessor bypass repeat :bool :bypass)

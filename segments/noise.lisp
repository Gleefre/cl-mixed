#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass noise (segment)
  ()
  (:default-initargs
   :type :pink))

(defmethod initialize-instance :after ((segment noise) &key type)
  (with-error-on-failure ()
    (mixed-cffi:make-segment-noise type (handle segment))))

(defun make-noise (&rest args &key type)
  (declare (ignore type))
  (apply #'make-instance 'noise args))

(define-field-accessor volume noise :float :volume)
(define-field-accessor noise-type noise mixed-cffi:noise-type :noise-type)

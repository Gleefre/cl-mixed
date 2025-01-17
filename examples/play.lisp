#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun play (file &key (samplerate 44100) (output 'org.shirakumo.fraf.mixed.out123:drain))
  (mixed:with-objects ((source (mixed:make-unpacker :samplerate samplerate))
                       (drain (mixed:make-packer :samplerate samplerate))
                       (mp3 (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file file :pack source))
                       (out (make-instance output :pack drain)))
    (mixed:with-buffers 500 (l r)
      (mixed:connect source :left drain :left l)
      (mixed:connect source :right drain :right r)
      (mixed:start out)
      (mixed:with-chain chain (mp3 source drain out)
        (format T "~&Playing back on ~d channels @ ~dHz, ~a~%"
                (mixed:channels drain) (mixed:samplerate drain) (mixed:encoding drain))
        (loop until (mixed:done-p mp3)
              do (mixed:mix chain))))))

;;;; binary-io.lisp - Readers and writers for primitive binary types

(in-package #:cl-nbt)

(defun read-unsigned-int (s size &key (endian :big))
  "Read an unsigned integer of SIZE bytes from stream S.
   Adapted from binary-types library."
  (let ((unsigned-value 0))
    (if (= 1 size)
        (setf unsigned-value (read-byte s nil nil))
        (ecase endian
          (:big
	   (dotimes (i size)
	     (setf unsigned-value (+ (* unsigned-value 256)
				     (read-byte s nil nil)))))
          (:little
	   (dotimes (i size)
	     (setf unsigned-value (+ unsigned-value
				     (ash (read-byte s nil nil)
					  (* 8 i))))))))
    unsigned-value))

(defun read-signed-int (s size &key (endian :big))
  "Read a signed integer of SIZE bytes from stream S.
   Adapted from binary-types library."
  (let ((unsigned-value (read-unsigned-int s size :endian endian)))
    (if (>= unsigned-value (ash 1 (1- (* 8 size))))
        (- unsigned-value (ash 1 (* 8 size)))
        unsigned-value)))

(defun write-int (s n size &key (endian :big))
  "Write a (signed or unsigned) integer N of SIZE bytes to stream S.
   Adapted from binary-types library."
  (if (= 1 size)
      (write-byte n s)
      (ecase endian
        (:big
	 (do ((i (* 8 (1- size)) (- i 8)))
	     ((minusp i) size)
	   (write-byte (ldb (byte 8 i) n) s)))
        (:little
	 (dotimes (i size)
	   (write-byte (ldb (byte 8 (* 8 i)) n) s)))))
  n)

(defun read-si1 (s) (read-signed-int s 1))
(defun read-si2 (s) (read-signed-int s 2))
(defun read-si3 (s) (read-signed-int s 3))
(defun read-si4 (s) (read-signed-int s 4))
(defun read-si8 (s) (read-signed-int s 8))
(defun write-si1 (s n) (write-int s n 1))
(defun write-si2 (s n) (write-int s n 2))
(defun write-si3 (s n) (write-int s n 3))
(defun write-si4 (s n) (write-int s n 4))
(defun write-si8 (s n) (write-int s n 8))

;; If an invalid entry is read, it is stored and written out exactly as
;; it was. Right now, ALL types of condition are caught (which is not
;; very smart).

(defun read-f4 (s)
  (let ((n (read-si4 s)))
    (handler-case (ieee-floats:decode-float32 n)
      (condition () n))))

(defun write-f4 (s n)
  (handler-case (write-si4 s (ieee-floats:encode-float32 n))
    (condition ()
      (write-si4 s n))))

(defun read-f8 (s)
  (let ((n (read-si8 s)))
    (handler-case (ieee-floats:decode-float64 n)
      (condition () n))))

(defun write-f8 (s n)
  (handler-case (write-si8 s (ieee-floats:encode-float64 n))
    (condition ()
      (write-si8 s n))))

(defun read-utf-8-string (s length)
  (let ((seq (make-array length
			 :element-type '(unsigned-byte 8)
			 :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq s))
    (handler-case
	(babel:octets-to-string seq :encoding :utf-8)
      (condition () seq))))

(defun write-utf-8-string (s str)
  (handler-case
      (write-sequence (babel:string-to-octets str :encoding :utf-8) s)
    (condition () (write-sequence str s))))

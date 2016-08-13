;;;; dot-mca.lisp - Code for parsing region (.mca) files.

(in-package #:cl-nbt)

(defclass region ()
  ((offsets :initarg :offsets :accessor offsets)
   (sector-counts :initarg :sector-counts :accessor sector-counts)
   (timestamps :initarg :timestamps :accessor timestamps)
   (len-chunk-data :initarg :len-chunk-data :accessor len-chunk-data)
   (compression-type :initarg :compression-type :accessor compression-type)
   (chunks :initarg :chunks :accessor chunks))
  (:default-initargs
    :offsets (make-array 1024 :initial-element 0)
    :sector-counts (make-array 1024 :initial-element 0)
    :timestamps (make-array 1024 :initial-element 0)
    :chunks (make-array 1024))
  (:documentation "Contents of a region file."))

;; Verify whether read-mca-file and write-mca-file preserve chunk entry order.

(defun read-mca-file (mca-file)
  (with-open-file (in mca-file :direction :input
		      :element-type '(unsigned-byte 8))
    (read-mca-payload in (read-mca-header in))))
    ;(read-mca-header in)))

(defun read-mca-header (stream)
  (let ((rgn (make-instance 'region)))
    (with-slots (offsets sector-counts timestamps
			 len-chunk-data compression-type) rgn
      (loop
	 for i below 1024
	 do (setf (aref offsets i) (read-si3 stream)
		  (aref sector-counts i) (read-si1 stream)))
      (loop
	 for i below 1024
	 do (setf (aref timestamps i) (read-si4 stream)))
      (setf len-chunk-data (read-si4 stream)
	    compression-type (read-si1 stream)))
    rgn))

(defun read-mca-payload (stream rgn)
  (with-slots (offsets sector-counts chunks) rgn
    (loop
       for i below 1024
       and offset across offsets
       and sector-count across sector-counts
       do (when (not (and (zerop offset) (zerop sector-count)))
	    (file-position stream (+ 5 (* 4096 offset)))
	    (setf (aref chunks i)
		  (read-tag (chipz:make-decompressing-stream
			     'chipz:zlib stream))))))
  rgn)

(defun write-mca-file (mca-file rgn)
  (with-open-file (out mca-file
		       :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-does-not-exist :create
                       :if-exists :supersede)
    (write-mca-header out rgn)
    (write-mca-payload out rgn)))

(defun write-mca-header (stream rgn)
  (with-slots (offsets sector-counts timestamps
		       len-chunk-data compression-type) rgn
    (loop
       for offset across offsets
       and sector-count across sector-counts
       do (write-si3 stream offset)
	 (write-si1 stream sector-count))
    (loop
       for timestamp across timestamps
       do (write-si4 stream timestamp))
    (write-si4 stream len-chunk-data)
    (write-si1 stream compression-type)))

(defun write-mca-payload (stream rgn)
  (with-slots (offsets sector-counts chunks) rgn
    (loop
       for i below 1024
       and chunk across chunks
       and offset across offsets
       and sector-count across sector-counts
       do (when (typep chunk 'nbt-tag)
	    (let* ((data (salza2:compress-data (tag-to-octets chunk)
					       'salza2:zlib-compressor))
		   (ceil (ceiling (length data) 4096)))

	      ;; Go back and edit sector-count value in header (if necessary)
	      (when (not (= sector-count ceil))
		(setf sector-count ceil)
		(let ((prev-pos (file-position stream)))
		  (file-position stream (+ 3 (* i 4)))
		  (write-si1 stream sector-count)
		  (file-position stream prev-pos)))

	      ;; Write compressed chunk data
	      (write-sequence data stream)

	      ;; Pad up to end of entry
	      (unless (= (length data) (* sector-count 4096))
		(write-sequence (make-array (- (* sector-count 4096)
					       (length data))
					    :element-type '(unsigned-byte 8)
					    :initial-element 0)
				stream)))))

    ;; Add padding so file length is a multiple of 4096
    (unless (zerop (mod (file-length stream) 4096))
      (write-sequence (make-array (mod (file-length stream) 4096)
				  :element-type '(unsigned-byte 8)
				  :initial-element 0)
		      stream))))

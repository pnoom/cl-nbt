
;;;; dot-dat.lisp - Code for parsing .dat files, eg. level.dat

(in-package #:cl-nbt)

(defun gzip-file (file)
  "Compress FILE in gzip format in-place."
  (cl-fad:with-open-temporary-file (temp :direction :io
					 :element-type '(unsigned-byte 8))
    (cl-fad:copy-file file temp :overwrite t)
    (salza2:gzip-file temp file))
  t)

(defun write-dat-file (dat-file root-tag &key (compress t))
  "Writes ROOT-TAG to DAT-FILE, compressing if necessary. Returns t if
  compressed, nil if not."
  (with-open-file (out dat-file :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (write-tag out root-tag))
  (if compress
      (gzip-file dat-file)))

(defun read-dat-file (dat-file)
  "Reads DAT-FILE file into a tag, gunzipping first if necessary. Returns
  tag, and a generalized boolean (t if DAT-FILE was originally compressed)."
  (with-open-file (in dat-file :direction :input
		      :element-type '(unsigned-byte 8))
    (cond ((and (= (read-byte in) #x1f)	;gzip magic numbers
		(= (read-byte in) #x8b))
	   (file-position in 0)
	   (values
	    (read-tag (chipz:make-decompressing-stream
		       'chipz:gzip in))
	    t))
	  (t (file-position in 0)
	     (values
	      (read-tag in)
	      nil)))))

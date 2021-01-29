(in-package :dimgp)

(defun read-ascii-file (filename)
  (words (regex-replace-all "#.*\\n"
			    (with-open-file (in filename)
			      (uiop:read-file-string in))
			    " ")))

(defun read-pgm-ascii (filename)
  "Read a pgm image file in string format and return an image object."
  (let ((pgm (read-ascii-file filename)))
    (match (pmap 'list #'parse-integer (cdr pgm))
      ((list* width height max-level pixels-values)
       (make-instance 'image :max-level max-level
			     :width width
			     :height height
			     :color-model 'grayscale
			     :channels
			     (list (make-array (* width height)
					       :element-type 'number
					       :initial-contents pixels-values)))))))

(defun read-ppm-ascii (filename)
  "Read a ppm image file in string format and return an image object."
  (let ((ppm (read-ascii-file filename)))
    (match (pmap 'list #'parse-integer (cdr ppm))
      ((list* width height max-level pixels-values)

       (flet ((splice (seq)
		(let ((res (make-array (* width height) :element-type 'number)))
		  (loop for el in seq by #'cdddr
			for i = 0 then (1+ i)
			do (setf (aref res i) el))
		  res)))

	 (make-instance 'image :max-level max-level
			       :width width
			       :height height
			       :color-model 'rgb
			       :channels
			       (pmap 'list #'splice (list pixels-values
							  (cdr pixels-values)
							  (cddr pixels-values)))))))))

(defun image-read (image-path)
  "Read an image file from disk at image-path and return an image object."
  (let* ((extension (first (last (split-sequence #\. image-path))))
	 (img-res (match extension
		    ((or "ppm" "pgm")
		     (match (read-netpbm-type image-path)
		       ("P2" (read-pgm-ascii image-path))
		       ("P3" (read-ppm-ascii image-path))
		       ("P5" (read-pgm-raw image-path))
		       (x (error (format nil "Can't open type ~a~%" x)))))
		    (_ (error (format nil "Can't open the file ~a~%" image-path))))))
    (setf (channels img-res)
	  (channels (map-each-channel (lambda (x)
					(/ x (max-level img-res)))
				      img-res)))
    img-res))

(defun read-netpbm-type (filename)
  (with-open-file (stream-char filename
			       :direction :input
			       :element-type 'character)
    (flet ((get-next-char ()
	     (string (read-char stream-char))))
      (concatenate 'string (get-next-char) (get-next-char)))))

(defun read-pgm-raw (image-path)
  (with-open-file (stream-char image-path
			       :direction :input
			       :element-type '(unsigned-byte 8))

    (labels ((get-next-char ()
	       (string (code-char (read-byte stream-char))))

	     (ignore-comment ()
	       (loop for char = (get-next-char)
		     while (not (string= char (string #\Newline)))))

	     (parse-int ()
	       (parse-integer (apply #'concatenate 'string
				     (loop for char = (get-next-char)
					   while (and (string>= char "0") (string<= char "9"))
					   collect char))))

	     (parse-header ()
	       (let ((width-height-levels nil))
		 (loop for char = (code-char (read-byte stream-char))
		       while (or (< (length width-height-levels) 3)
				 (match char
				   (#\# t)
				   (#\Newline t)
				   (#\Space t)
				   (_ nil)))
		       when (equal #\# char)
			 do (ignore-comment)
		       when (and (string> char "0") (string< char "9"))
			 do (progn (file-position stream-char (1- (file-position stream-char)))
				   (push (parse-int) width-height-levels))
		       finally (file-position stream-char (1- (file-position stream-char))))
		 width-height-levels)))

      (match (concatenate 'string (get-next-char) (get-next-char))
	("P5" t)
	(_ (error "Expected P5 image")))
      (destructuring-bind (max-level height width) (parse-header)
	(let ((pixels (make-array (* height width) :element-type 'number)))
	  (read-sequence pixels stream-char)
	  (make-instance 'image :max-level max-level
			     :width width
			     :height height
			     :color-model 'grayscale
			     :channels
			     (list pixels)))))))

;;; Write image
(defun write-netpbm (image filename img-type)
  (with-slots (channels width height max-level color-model) image
    (with-open-file (out filename
			 :direction :output
			 :if-exists :supersede)
      (let ((channels (flatten (apply #'map 'list 'list channels))))
	(format out "~a~%~a ~a~%~a~%"
		img-type width height max-level)
	(loop for i = 0 then (1+ i)
	      for pixel in channels
	      do (if (= 0 (mod i 10))
		     (format out "~a~%" pixel)
		     (format out "~a " pixel)))))))

(defun image-write (image image-path)
  (let ((extension (first (last (split-sequence #\. image-path))))
	(image (map-each-channel (lambda (x)
				   (round (* x (max-level image))))
				 image)))
    (match extension
      ("pgm" (write-netpbm image image-path "P2"))
      ("ppm" (write-netpbm image image-path "P3"))
      (_ (error (format nil "Can't open the file ~a~%" image-path))))))

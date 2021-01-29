;;; Single channel transforms

(in-package :dimgp)

(defun inverse ()
  "Returns 1 - x."
  (lambda (x)
    (- 1 x)))

(defun logarithm (c)
  "Returns the min of 1 and (c/log(2)) * log(1 + x)."
  (lambda (x)
    (min 1 (/ (* c (log (1+ x)))
	      (log 2)))))

(defun gamma (gamma offset)
  "Return a transformation T(X) = c*(offset + X)^gamma."
  (lambda (x)
    (/ (expt (+ offset x) gamma)
       (expt (1+ offset) gamma))))

(defun contrast-stretching (a b c d)
  "Return a transformation T(X) using (a b) (c d). "
  (lambda (x)
    (cond ((<= x a)
	   (* b x))
	  ((and (> x a) (< x c))
	   (let* ((delta (/ (- d b) (- c a)))
		  (k (- b (* delta a))))
	     (+ k (* delta x))))
	  (t
	   (let* ((delta (/ (- 1 d) (- 1 c)))
		  (k (- 1 delta)))
	     (+ k (* delta x)))))))

(defun histogram-equalization (image)
  (with-slots (width height max-level) image
    (let* ((img-res (image-copy-metadata image))
	   (num-pixels (* width height))
	   (color-model (color-model image))
	   (hsl-img (match color-model
		      ('rgb (rgb-to-hsl image))
		      (_ nil)))
	   (channel-to-equalize (match color-model
				  ('rgb (third (channels hsl-img)))
				  ((or 'hsl 'hsv) (third (channels image)))
				  ('grayscale (first (channels image)))))
	   (new-channels (match color-model
			   ('grayscale '())
			   ('rgb (list (first (channels hsl-img)) (second (channels hsl-img))))
			   (_ (list (copy-seq (first (channels image)))
				    (copy-seq (second (channels image)))))))
	   (unnormalized-channel (map 'vector (lambda (x) (round (* x max-level))) channel-to-equalize))
	   (histogram (map 'vector (lambda (v)
				     (/ (count v unnormalized-channel) num-pixels))
			   (range 0 (1+ (max-level img-res)))))
	   (prob-dist (map 'vector (let ((acc 0))
				     (lambda (x)
				       (setf acc (+ acc x))))
			   histogram))
	   (equalized-channel (list (map 'vector (lambda (i)
						   (aref prob-dist i))
					 unnormalized-channel))))
      (setf (channels img-res) (concatenate 'list new-channels equalized-channel))
      (match color-model
	('rgb (hsl-to-rgb img-res))
	(_ img-res)))))

(in-package :dimgp)
;; (spatial-filtering (define-filter '((0.2 0.2 0.2)
;; 				    (0.2 1 0.2)
;; 				    (0.2 0.2 0.2)))
;; 		   (3 5) image)

(defun spatial-filtering (function window-size image)
  (with-slots (width height) image
    (let* ((window-cols (first window-size))
	   (window-lines (second window-size)))

      (flet ((map-channel (channel)
	       (let ((range (matrix-range width height))
		     (get-window (window-around window-lines window-cols
						height width channel)))
		 (pmap 'vector (lambda (x y)
				 (funcall function (funcall get-window x y)))
		       (first range) (second range)))))

	(let ((img-res (copy-image-metadata image)))
	  (setf (channels img-res)
		(pmap 'list #'map-channel (channels image)))
	  img-res)))))

(defun window-around (window-lines window-cols height width matrix)
  (lambda (x y)
    (let ((start-lines (- y (floor (/ window-lines 2))))
	  (start-cols (- x (floor (/ window-cols 2)))))
      (loop for i across (range start-lines (+ start-lines window-lines))
	    nconc (loop for j across (range start-cols (+ start-cols window-cols))
			collect (aref matrix (+ (* width (clamp i 0 (1- height)))
						(clamp j 0 (1- width)))))))))

(defun matrix-range (n-cols n-lines)
  "Return a list with 2 inner lists. The first has the X coordinates,
 the second has the Y coordinates."
  (declare (type fixnum n-cols n-lines))
  (list (repeat-sequence (iota n-cols) n-lines)
	(mapcan (lambda (x)
		  (repeat-sequence (list x) n-cols))
		(iota n-lines))))

;; (image-write (spatial-filtering #'mean '(3 5) (image-read "imgs/lena.ppm")) "test.ppm")


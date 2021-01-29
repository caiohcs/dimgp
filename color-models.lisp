(in-package :dimgp)

(defun rgb-to-grayscale (image)
  (let ((res (map-channels (lambda (r g b)
			     (+ (* 0.299 r)
				(* 0.587 g)
				(* 0.114 b)))
			   image)))
    (setf (color-model res) 'grayscale)
    res))

(defun rgb-to-hsv (image)
  (flet ((pixel-rgb-to-hsv (vector-list)
	   (lambda (i r g b)
	     (let* ((v (max r g b))
		    (x-min (min r g b))
		    (c (- v x-min))
		    (s-v (match v
			   ((= 0) 0)
			   (_ (/ c v))))
		    (h (mod (match v
			      ((= x-min) 0)
			      ((and (type number) (= r)) (* 60 (/ (- g b) c)))
			      ((and (type number) (= g)) (* 60 (+ 2 (/ (- b r) c))))
			      ((and (type number) (= b)) (* 60 (+ 4 (/ (- r g) c))))
			      (_ (error (format nil "V value was: ~a~%" v))))
			    360)))
	       (setf (aref (first vector-list) i) h)
	       (setf (aref (second vector-list) i) s-v)
	       (setf (aref (third vector-list) i) v)))))

    (let ((res (transform-channels #'pixel-rgb-to-hsv image)))
      (setf (color-model res) 'hsv)
      res)))

(defun rgb-to-hsl (image)
  (flet ((pixel-rgb-to-hsl (vector-list)
	   (lambda (i r g b)
	     (let* ((v (max r g b))
		    (x-min (min r g b))
		    (c (- v x-min))
		    (l (/ (+ v x-min) 2))
		    (s-l (match l
			   ((or (= 0) (= 1) 0))
			   (_ (/ c (- 1 (abs (- (* 2 v) c 1)))))))
		    (h (mod (match v
			      ((and (type number) (= x-min)) 0)
			      ((and (type number) (= r)) (* 60 (/ (- g b) c)))
			      ((and (type number) (= g)) (* 60 (+ 2 (/ (- b r) c))))
			      ((and (type number) (= b)) (* 60 (+ 4 (/ (- r g) c))))
			      (_ (error (format nil "V value was: ~a~%" v))))
			    360)))
	       (setf (aref (first vector-list) i) h)
	       (setf (aref (second vector-list) i) s-l)
	       (setf (aref (third vector-list) i) l)))))

    (let ((res (transform-channels #'pixel-rgb-to-hsl image)))
      (setf (color-model res) 'hsl)
      res)))

(defun hsl-to-rgb (image)
  (flet ((pixel-rgb-to-hsl (vector-list)
	   (lambda (i h s l)
	     (let* ((c (* s (- 1 (abs (1- (* 2 l))))))
		    (hp (/ h 60))
		    (x (* c (- 1 (abs (1- (mod hp 2))))))
		    (m (- l (/ c 2)))
		    (rgb-1 (match hp
			     ((and (>= 0) (<= 1)) (list c x 0))
			     ((and (>= 1) (<= 2)) (list x c 0))
			     ((and (>= 2) (<= 3)) (list 0 c x))
			     ((and (>= 3) (<= 4)) (list 0 x c))
			     ((and (>= 4) (<= 5)) (list x 0 c))
			     ((and (>= 5) (<= 6)) (list c 0 x)))))
	       (mapc (lambda (fn rgb)
		       (setf (aref (funcall fn vector-list) i) (+ rgb m)))
		     (list #'first #'second #'third) rgb-1)))))

    (let ((res (transform-channels #'pixel-rgb-to-hsl image)))
      (setf (color-model res) 'rgb)
      res)))
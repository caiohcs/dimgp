(in-package :dimgp)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (count-cpus) :name "custom-kernel"))

(defclass image ()
  ((width
    :type fixnum
    :initarg :width)
   (height
    :type fixnum
    :initarg :height)
   (max-level
    :type fixnum
    :initarg :max-level
    :accessor max-level)
   (color-model
    :type symbol
    :initarg :color-model
    :accessor color-model)
   (channels
    :initarg :channels
    :accessor channels)))

(defun normalize (image)
  (let* ((max (apply #'max (reduce-each-channel #'max image)))
	 (min (apply #'min (reduce-each-channel #'min image)))
	 (res (map-each-channel (lambda (x) (/ (- x min) (- max min))) image)))
    (values res max min)))

(defun image-copy-metadata (image)
  "Return an image that has a copy of IMAGE's attributes except by the channels, which
will be unbound."
  (with-slots (channels width height max-level color-model) image
    (make-instance 'image :width width
			  :height height
			  :max-level max-level
			  :color-model color-model)))

;;; Map channels
(defun map-channels (function image)
  "Map the channels of IMAGE.
FUNCTION should be a function with an arity equal to the image's number of channels."
  (let ((img-res (image-copy-metadata image)))
    (setf (channels img-res)
	  (list (apply #'pmap 'vector function (channels image))))
    img-res))

(defun map-each-channel (functions image)
  "Map each channel of IMAGE using FUNCTIONS.
FUNCTIONS can be a function or a list of functions. If it's a function, then the
same function is mapped to all channels. Each function should take one argument.
If FUNCTIONS is a list, then it must have the length should be equal to the number
of channels."
  (unless (listp functions)
    (setf functions (repeat-sequence (list functions) (length (channels image)))))

  (flet ((map-channel (function channel)
	   (pmap 'vector function channel)))

    (let ((img-res (image-copy-metadata image)))
      (setf (channels img-res)
	    (pmap 'list #'map-channel
		  functions (channels image)))
      img-res)))

(defun transform-channels (function image)
  (with-slots (channels) image
    (let* ((img-res (image-copy-metadata image))
	   (num-channels (length channels))
	   (size-channel (array-dimension (first channels) 0))
	   (vector-list (loop for i from 0 below num-channels
			      collect (make-array size-channel)))
	   (fn (funcall function vector-list)))
      (apply #'pmap 'list fn (concatenate 'list (list (iota size-channel)) (channels image)))
      (setf (channels img-res) vector-list)
      img-res)))

(defun reduce-each-channel (functions image)
  "Reduce each channel of IMAGE using FUNCTIONS.
FUNCTIONS can be a function or a list of functions."
  (unless (listp functions)
    (setf functions (repeat-sequence (list functions) (length (channels image)))))

  (flet ((reduce-channel (channel function)
	   (preduce function channel)))

    (pmap 'list #'reduce-channel
	  (channels image) functions)))


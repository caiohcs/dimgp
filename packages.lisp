(defpackage #:dimgp
  (:use #:common-lisp)
  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:alexandria
		#:flatten
		#:iota
		#:mean
		#:median
		#:clamp)
  (:import-from #:serapeum
		#:count-cpus
		#:repeat-sequence
		#:range
		#:frequencies
		#:words)
  (:import-from #:cl-ppcre
		#:regex-replace-all)
  (:import-from #:lparallel
		#:pmap
		#:preduce)
  (:import-from #:trivia
		#:match)
  (:export #:image
	   #:image-read
	   #:image-write
	   #:image-copy-metadata
	   #:map-channels
	   #:map-each-channel
	   #:transform-channels
	   #:reduce-each-channel
	   #:spatial-filltering
	   #:rgb-to-hsv
	   #:rgb-to-hsl
	   #:rgb-to-grayscale
	   #:hsl-to-rgb))

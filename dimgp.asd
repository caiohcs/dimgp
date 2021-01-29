(asdf:defsystem #:dimgp
  :name "dimgp"
  :version "0.1"
  :license "GPLv3"
  :author "Caio Henrique"
  :description "Digital image processing algorithms."
  :serial t
  :depends-on (#:alexandria
	       #:lparallel
	       #:serapeum
	       #:trivia
	       #:split-sequence
	       #:cl-ppcre)
  :components ((:file "packages")
	       (:file "dimgp")
	       (:file "color-models")
	       (:file "single-channel-transformations")
	       (:file "spatial-filters")
	       (:file "img-read-write")))

(asdf:defsystem #:dimgp/test
  :name "dimgp/test"
  :version "0.1"
  :license "GPLv3"
  :author "Caio Henrique"
  :serial t
  :depends-on (#:dimgp #:fiveam)
  :components ((:file "test")))

(asdf:defsystem realm-of-dark-forces
	:name "In the Realm of Dark Forces"
	:depends-on (trivial-gamekit cl-csv)
	:serial t
	:components ((:file "packages")
		     (:file "character")
		     (:file "tiles")
		     (:file "tilemap-files")
		     (:file "main")))

(asdf:defsystem realm-of-dark-forces
	:name "In the Realm of Dark Forces"
	:depends-on (trivial-gamekit cl-csv cxml)
	:serial t
	:components ((:file "packages")
		     (:file "character")
		     (:file "tiles")
		     (:file "tileset-files")
		     (:file "tilemap-files")
		     (:file "main")))

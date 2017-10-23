(asdf:defsystem realm-of-dark-forces
	:name "In the Realm of Dark Forces"
	:depends-on (:trivial-gamekit :cl-csv :cxml :cl-ppcre)
	:serial t
	:components ((:file "packages")
		     (:file "util")
		     (:file "character")
		     (:file "tiles")
		     (:file "tileset-files")
		     (:file "tilemap-files")
		     (:file "game")
		     (:file "animations")
		     (:file "draw")
		     (:file "main")))

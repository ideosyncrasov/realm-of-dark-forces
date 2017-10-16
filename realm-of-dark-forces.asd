(asdf:defsystem realm-of-dark-forces
	:name "Realm of Dark Forces"
	:depends-on (trivial-gamekit cl-csv)
	:components ((:file "packages")
		     (:file "main")))

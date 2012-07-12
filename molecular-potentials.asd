(asdf:defsystem :molecular-potentials
  :components
  ((:module "setup"
	    :pathname #p"./"
	    :components ((:file "molecular-potentials-package-def")
			 (:file "object-setup"
				:depends-on ("molecular-potentials-package-def"))))
   (:module "body"
	    :pathname #p"./"
	    :depends-on ("setup")
	    :components ((:file "hs-molecular-potential")
			 (:file "lennard-jones-coeffs")
			 (:file "lennard-jones-molecular-potential"
				:depends-on ("lennard-jones-coeffs")))))
  :depends-on (:lisp-unit
	       :alexandria))

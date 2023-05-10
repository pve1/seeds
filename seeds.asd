(asdf:defsystem #:seeds
  :description "A utility for conveniently creating new projects."
  :author "Peter von Etter <peterve@gmail.com>"
  :license "LGPL-3.0"
  :version "0.0.1"
  :components ((:file "seeds"))
  :depends-on (#:alexandria
               #:quickproject
               #:capitalized-export))

;;;; goof.asd

(asdf:defsystem #:goof
  :serial t
  :description "A simple, static, forth compiler for the 68k."
  :author "Erik Haliewicz <ehal256@gmail.com>"
  :license "Do whatever you want."
  :depends-on (#:cl-interpol)
  :components ((:file "package")
               (:file "goof")
               (:file "compiler")
               (:file "primitives")))


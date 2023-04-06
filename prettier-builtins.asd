(defsystem "prettier-builtins"
  :depends-on ("alexandria"
               "uiop")
  :license "MIT"
  :description "A lightweight library to pretty print builtin arrays and hash-tables."
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :components ((:file "package")
               (:file "array")
               (:file "hash-table")))

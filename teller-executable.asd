(asdf:defsystem "teller-executable"
  :build-operation "program-op"
  :build-pathname "teller.derp" ;; shell name
  :entry-point "teller::tell" ;; thunk
  :depends-on ("teller")
  ;; :components ((:file "main"))
  )

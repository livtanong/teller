build:
	sbcl \
		--load teller.asd \
		--load teller-executable.asd \
		--eval '(ql:quickload :teller-executable)' \
		--eval '(asdf:make :teller-executable)' \
		--eval '(quit)'

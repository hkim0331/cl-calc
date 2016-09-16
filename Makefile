calc:
	sbcl \
	--eval "(ql:quickload :calc)" \
	--eval "(in-package :calc)" \
	--eval "(sb-ext:save-lisp-and-die \"calc\" :executable t :toplevel 'main)"

clean:
	${RM} calc

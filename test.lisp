
(in-package :common-lisp-user)

(defpackage :fd-stream.test
  (:use :common-lisp
	:cl-stream
	:fd-stream)
  #.(cl-stream:shadowing-import-from))

(in-package :fd-stream.test)

(with-stream (s (open-file "/tmp/test"))
  (dotimes (i 100)
    (write s i)))

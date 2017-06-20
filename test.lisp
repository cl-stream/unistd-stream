
(in-package :common-lisp-user)

(defpackage :fd-stream.test
  (:use :common-lisp
        :cl-stream
        :fd-stream)
  #.(cl-stream:shadowing-import-from))

(in-package :fd-stream.test)

(let ((s (fd-output-stream 1)))
  (write s 13)
  (dotimes (i 10)
    (write s (+ 48 i)))
  (write s 13)
  (flush s))

(let ((s (fd-input-stream 0)))
  (read s))

(with-stream (s (open-file "/tmp/test"))
  (dotimes (i 100)
    (write s i)))

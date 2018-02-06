
(in-package :unistd-stream)

(defclass unistd-io-stream (unistd-input-stream unistd-output-stream)
  ()
  (:documentation "A buffered input/output stream using
UNISTD:READ and UNISTD:WRITE."))

(defmethod stream-close ((stream unistd-io-stream))
  (call-next-method)
  (cffi:foreign-free (stream-input-buffer stream))
  (cffi:foreign-free (stream-output-buffer stream)))

(defun unistd-io-stream (fd)
  "Creates a buffered input/output stream for file descriptor FD."
  (make-instance 'unistd-io-stream :fd fd))

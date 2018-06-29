
(in-package :unistd-stream)

(defclass unistd-output-stream (unistd-stream buffered-output-stream)
  ()
  (:documentation "A buffered output stream using UNISTD:WRITE."))

(defmethod make-stream-output-buffer ((stream unistd-output-stream))
  (cffi:foreign-alloc :unsigned-char
                      :count (stream-output-buffer-size stream)))

(defmethod stream-discard-output-buffer ((stream unistd-output-stream))
  (cffi:foreign-free (stream-output-buffer stream))
  (setf (stream-output-buffer stream) nil))

(defmethod stream-flush-output ((stream unistd-output-stream))
  (let* ((buffer (stream-output-buffer stream))
         (index (stream-output-index stream))
         (length (stream-output-length stream))
         (buflen (- length index)))
    (declare (type fixnum index length buflen))
    (when (< 0 buflen)
      (let* ((fd (stream-fd stream))
             (buf (cffi:mem-aptr buffer :unsigned-char index))
             (r (if (stream-blocking-p stream)
                    (unistd:write fd buf buflen)
                    (unistd:write-non-blocking fd buf buflen))))
        (cond ((eq :non-blocking r) :non-blocking)
              ((= 0 r) :eof)
              ((< r 0) (error 'stream-output-error :stream stream))
              (t (incf (the fixnum (stream-output-index stream)) r)
                 (when (= (the fixnum (stream-output-index stream))
                          (the fixnum (stream-output-length stream)))
                   (setf (stream-output-index stream) 0
                         (stream-output-length stream) 0))
               nil))))))

(defmethod stream-write-element-to-buffer ((stream unistd-output-stream)
                                           element)
  (setf (cffi:mem-aref (stream-output-buffer stream) :unsigned-char
                       (stream-output-length stream))
        element)
  (incf (stream-output-length stream))
  nil)

(defun unistd-output-stream (fd)
  "Creates a buffered output stream for file descriptor FD."
  (make-instance 'unistd-output-stream :fd fd))

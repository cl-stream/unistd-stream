
(in-package :unistd-stream)

(defclass unistd-input-stream (unistd-stream buffered-input-stream)
  ()
  (:documentation "A buffered input stream using UNISTD:READ."))

(defmethod make-stream-input-buffer ((stream unistd-input-stream))
  (cffi:foreign-alloc :unsigned-char
                      :count (stream-input-buffer-size stream)))

(defmethod stream-discard-input-buffer ((stream unistd-input-stream))
  (cffi:foreign-free (stream-input-buffer stream))
  (setf (stream-input-buffer stream) nil))

(defmethod stream-fill-input-buffer ((stream unistd-input-stream))
  (let* ((buffer (stream-input-buffer stream))
         (length (stream-input-length stream))
         (fd (stream-fd stream))
         (buf (cffi:mem-aptr buffer :unsigned-char length))
         (buflen (- (stream-input-buffer-size stream) length))
         (r (if (stream-blocking-p stream)
                (unistd:read fd buf buflen)
                (unistd:read-non-blocking fd buf buflen))))
    (cond ((eq :non-blocking r)
           :non-blocking)
          ((= r 0)
           :eof)
          ((< r 0)
           (error 'stream-input-error :stream stream))
          (t
           (incf (stream-input-length stream) r)
           nil))))

(defmethod stream-read-element-from-buffer ((stream unistd-input-stream))
  (let ((element (cffi:mem-aref (stream-input-buffer stream) :unsigned-char
                                (stream-input-index stream))))
    (incf (stream-input-index stream))
    (when (= (stream-input-index stream) (stream-input-length stream))
      (setf (stream-input-index stream) 0
            (stream-input-length stream) 0))
    (values element nil)))

(defun unistd-input-stream (fd)
  "Creates a buffered input stream for file descriptor FD."
  (make-instance 'unistd-input-stream :fd fd))

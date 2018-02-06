
(in-package :unistd-stream)

(defun compute-flags (read write append non-blocking create)
  (logior (if append              fcntl:+o-append+   0)
          (if non-blocking        fcntl:+o-nonblock+ 0)
          (if create              fcntl:+o-creat+    0)
          (cond ((and read write) fcntl:+o-rdwr+)
                (read             fcntl:+o-rdonly+)
                (write            fcntl:+o-wronly+)
                (t 0))))

(defun compute-class (read write)
  (cond ((and read write) 'unistd-io-stream)
        (read             'unistd-input-stream)
        (write            'unistd-output-stream)))

(defun unistd-stream-open (pathname &key
                                      read write append
                                      non-blocking
                                      (create #o777)
                                      (input-buffer-size
                                       *stream-default-buffer-size*)
                                      (output-buffer-size
                                       *stream-default-buffer-size*))
  (assert (or read write)
          (read write)
          "Open not for reading nor writing.")
  (let* ((flags (compute-flags read write append non-blocking create))
         (fd (fcntl:open pathname flags (or create 0)))
         (class (compute-class read write)))
    (make-instance class
                   :fd fd
                   :input-buffer-size input-buffer-size
                   :output-buffer-size output-buffer-size)))

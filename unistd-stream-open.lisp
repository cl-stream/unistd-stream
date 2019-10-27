
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

(defun compute-mode (create)
  (cond ((null create) nil)
        ((eq t create) #o666)
        (t create)))

(defun unistd-stream-open (pathname &key
                                      read write append
                                      non-blocking
                                      (create #o666)
                                      (input-buffer-size
                                       *stream-default-buffer-size*)
                                      (output-buffer-size
                                       *stream-default-buffer-size*))
  (assert (or read write)
          (read write)
          "Open not for reading nor writing.")
  (when (pathnamep pathname)
    (setq pathname (namestring pathname)))
  (let* ((flags (compute-flags read write append non-blocking create))
         (mode (compute-mode create))
         (fd (fcntl:open pathname flags mode))
         (class (compute-class read write))
         (args ()))
    (when read
      (setf args (list* :input-buffer-size input-buffer-size args)))
    (when write
      (setf args (list* :output-buffer-size output-buffer-size args)))
    (apply #'make-instance class :fd fd args)))

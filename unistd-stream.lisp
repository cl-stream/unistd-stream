;;
;;  unistd-stream  -  Unix file descriptor layer for cl-stream
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :unistd-stream)

(deftype fixnum+ (&optional (start 0))
  `(integer ,start ,most-positive-fixnum))

(defclass unistd-stream (ub8-stream)
  ((fd :initarg :fd
       :reader stream-fd
       :type file-descriptor)
   (blocking-p :initarg :blocking-p
               :type boolean))
  (:documentation "Base class for file descriptor streams."))

(defmethod stream-blocking-p ((stream unistd-stream))
  (or (when (slot-boundp stream 'blocking-p)
        (slot-value stream 'blocking-p))
      (setf (slot-value stream 'blocking-p)
            (let ((flags (fcntl:getfl (stream-fd stream))))
              (zerop (logand fcntl:+o-nonblock+ flags))))))

(defmethod (setf stream-blocking-p) (value (stream unistd-stream))
  (let* ((fd (stream-fd stream))
         (flags (fcntl:getfl fd))
         (o-nonblock (not (= 0 (logand fcntl:+o-nonblock+ flags)))))
    (cond
      ((and value (not o-nonblock))
       t)
      ((and (not value) o-nonblock)
       nil)
      (t
       (fcntl:setfl fd (if value
                           (logand (lognot fcntl:+o-nonblock+) flags)
                           (logior fcntl:+o-nonblock+ flags)))
       (setf (slot-value stream 'blocking-p) value)))))

(defmethod stream-close ((stream unistd-stream))
  (when (stream-open-p stream)
    (unistd:close (stream-fd stream))))

(defmethod stream-open-p ((stream unistd-stream))
  (let ((fd (stream-fd stream)))
    (unless (= -1 (unistd:c-dup2 fd fd))
      t)))

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

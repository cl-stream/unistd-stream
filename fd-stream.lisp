;;
;;  fd-stream  -  Unix file descriptor layer for cl-stream
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
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

(in-package :fd-stream)

(deftype fixnum+ (&optional (start 0))
  `(integer ,start ,most-positive-fixnum))

(defclass fd-stream (stream)
  ((fd :initarg :fd
       :reader stream-fd
       :type file-descriptor)
   (blocking-p :initarg :blocking-p
               :type boolean))
  (:documentation "Base class for file descriptor streams."))

(defmethod stream-blocking-p ((stream fd-stream))
  (or (when (slot-boundp stream 'blocking-p)
        (slot-value stream 'blocking-p))
      (setf (slot-value stream 'blocking-p)
            (let ((flags (fcntl:getfl (stream-fd stream))))
              (not (= 0 (logand fcntl:+o-nonblock+ flags)))))))

(defmethod (setf stream-blocking-p) (value (stream fd-stream))
  (let* ((fd (stream-fd stream))
         (flags (fcntl:getfl fd))
         (o-nonblock (not (= 0 (logand fcntl:+o-nonblock+ flags)))))
    (cond
      ((and value o-nonblock)
       t)
      ((and (not value) (not o-nonblock))
       nil)
      (t
       (fcntl:setfl fd (if value
                           (logand (lognot fcntl:+o-nonblock+) flags)
                           (logior fcntl:+o-nonblock+ flags)))
       (setf (slot-value stream 'blocking-p) value)))))

(defmethod stream-element-type ((stream fd-stream))
  '(unsigned-byte 8))

(defmethod close ((stream fd-stream))
  (unistd:close (stream-fd stream))
  (call-next-method))

(defclass fd-input-stream (fd-stream buffered-input-stream)
  ()
  (:documentation "A buffered input stream using UNISTD:READ."))

(defmethod make-stream-input-buffer ((stream fd-input-stream))
  (cffi:foreign-alloc :unsigned-char
                      :count (stream-input-buffer-size stream)))

(defmethod discard-stream-input-buffer ((stream fd-input-stream))
  (cffi:foreign-free (stream-input-buffer stream))
  (setf (stream-input-buffer stream) nil))

(defmethod stream-fill-input-buffer ((stream fd-input-stream))
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

(defmethod stream-read-element-from-buffer ((stream fd-input-stream))
  (let ((element (cffi:mem-aref (stream-input-buffer stream) :unsigned-char
                                (stream-input-index stream))))
    (incf (stream-input-index stream))
    (values element nil)))

(defun fd-input-stream (fd)
  "Creates a buffered input stream for file descriptor FD."
  (make-instance 'fd-input-stream :fd fd))

(defclass fd-output-stream (fd-stream buffered-output-stream)
  ()
  (:documentation "A buffered output stream using UNISTD:WRITE."))

(defmethod make-stream-output-buffer ((stream fd-output-stream))
  (cffi:foreign-alloc :unsigned-char
                      :count (stream-output-buffer-size stream)))

(defmethod discard-stream-output-buffer ((stream fd-output-stream))
  (cffi:foreign-free (stream-output-buffer stream))
  (setf (stream-output-buffer stream) nil))

(defmethod stream-flush-output-buffer ((stream fd-output-stream))
  (let ((buffer (stream-output-buffer stream)))
    (let* ((fd (stream-fd stream))
           (index (stream-output-index stream))
           (buf (cffi:mem-aptr buffer :unsigned-char index))
           (buflen (- (stream-output-length stream) index))
           (r (if (stream-blocking-p stream)
                  (unistd:write fd buf buflen)
                  (unistd:write-non-blocking fd buf buflen))))
      (cond ((eq :non-blocking r)
             :non-blocking)
            ((= 0 r)
             :eof)
            ((< r 0)
             (error 'stream-output-error :stream stream))
            (t
             (incf (stream-output-index stream) r)
             (when (= (stream-output-index stream)
                      (stream-output-length stream))
               (setf (stream-output-index stream) 0
                     (stream-output-length stream) 0))
             nil)))))

(defmethod stream-write-element-to-buffer ((stream fd-output-stream)
                                           element)
  (setf (cffi:mem-aref (stream-output-buffer stream) :unsigned-char
                       (stream-output-length stream))
        element)
  (incf (stream-output-length stream))
  nil)

(defun fd-output-stream (fd)
  "Creates a buffered output stream for file descriptor FD."
  (make-instance 'fd-output-stream :fd fd))

(defclass fd-io-stream (fd-input-stream fd-output-stream)
  ()
  (:documentation "A buffered input/output stream using
UNISTD:READ and UNISTD:WRITE."))

(defmethod close ((stream fd-io-stream))
  (call-next-method)
  (cffi:foreign-free (stream-input-buffer stream))
  (cffi:foreign-free (stream-output-buffer stream)))

(defun fd-io-stream (fd)
  "Creates a buffered input/output stream for file descriptor FD."
  (make-instance 'fd-io-stream :fd fd))

(defun open-file (pathname &key
                             (append nil)
                             (blocking t)
                             (create t)
                             (direction :io)
                             (input-buffer-size *default-buffer-size*)
                             (mode #o777)
                             (output-buffer-size *default-buffer-size*))
  (let* ((flags (logior (if append
                            fcntl:+o-append+
                            0)
                        (if blocking
                            0
                            fcntl:+o-nonblock+)
                        (if create
                            fcntl:+o-creat+
                            0)
                        (ecase direction
                          ((:input) fcntl:+o-rdonly+)
                          ((:output) fcntl:+o-wronly+)
                          ((:io) fcntl:+o-rdwr+))))
         (fd (fcntl:open pathname flags mode)))
    (make-instance (case direction
                     ((:input) 'fd-input-stream)
                     ((:output) 'fd-output-stream)
                     ((:io) 'fd-io-stream))
                   :fd fd
                   :input-buffer-size input-buffer-size
                   :output-buffer-size output-buffer-size)))

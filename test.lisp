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

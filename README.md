# unistd-stream

unistd streams are `(unsigned-byte 8)` cl-stream streams
using `unistd:read`, `unistd:write` and `unistd:close` to
operate on Unix file descriptors.

Depends on :
- https://github.com/cffi-posix/cffi-fcntl
- https://github.com/cffi-posix/cffi-unistd
- https://github.com/cl-stream/cl-stream

## Class: unistd-stream
Base class for file descriptor streams.

### Generic: stream-fd *unistd-stream* => *fd*
Returns the file descriptor of UNISTD-STREAM.

## Class: unistd-input-stream
A buffered input stream using UNISTD:READ.

### Function: unistd-input-stream *fd* => *stream*
Creates a buffered input stream for file descriptor *fd*.

## Class: unistd-output-stream
A buffered output stream using UNISTD:WRITE.

### Function: unistd-output-stream *fd* => *stream*
Creates a buffered output stream for file descriptor *fd*.

## Class: unistd-io-stream
A buffered input/output stream using UNISTD:READ and UNISTD:WRITE.

### Function: unistd-io-stream *fd* => *stream*
Creates a buffered input/output stream for file descriptor *fd*.

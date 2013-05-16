(use test)

(include "../common.scm")

;;; Transport tests

(test-begin "Testing transport layer")

(define (write/read data-len)
  (let ((len (call-with-output-string
              (lambda (port)
                (write-content-length (make-string data-len #\0) port)))))
    (call-with-input-string
     len
     (lambda (port)
       (read-content-length port)))))

(test 42 (write/read 42))
(test 255 (write/read 255))
(test-error (write/read 256))

(test-end "Testing transport layer")
(test-exit)

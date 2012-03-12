(define json-rpc-version "2.0")

(define debug
  (let ((debug? (get-environment-variable "JSON_RPC_DEBUG")))
    (lambda (msg #!optional direction)
      (when debug?
        (case direction
          ((in) (display "<-- ") (pp msg))
          ((out) (display "--> ") (pp msg))
          (else (pp msg)))))))


(define (send-message! data-str port)
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (display data-str port)
  ;; (debug (sprintf "Sending (length ~a):  ~a"
  ;;                 (string-length data-str)
  ;;                 data-str))
  (flush-output port))


(define (send-message-dup! data-str port)
  ;; To simulate sending to responses/requests in the same message
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (display data-str port)
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte 0 port)
  (write-byte (string-length data-str) port)
  (display data-str port)
  (flush-output port))


(define (receive-message port)
  (handle-exceptions exn ;; FIXME: check for read timeout
    #f
    (let ((b1 (read-byte port)))
      (if (eof-object? b1)
          #f
          (let* ((b2 (read-byte port))
                 (data-length (bitwise-ior (arithmetic-shift b1 8)
                                           b2))
                 (data (read-string (+ 4 data-length) port))
                 (data (substring data 4)))
            ;; (debug (sprintf "Receiving (length ~a): ~a"
            ;;                data-length
            ;;                data))
            data)))))

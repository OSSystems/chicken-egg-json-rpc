(define json-rpc-version "2.0")

(define debug
  (let ((debug? (get-environment-variable "JSON_RPC_DEBUG")))
    (lambda (msg #!optional direction)
      (when debug?
        (case direction
          ((in) (display "<-- ") (pp msg))
          ((out) (display "--> ") (pp msg))
          (else (pp msg)))))))

;;;
;;; Transport stuff
;;;
;; The transport layer assumes the content length to be a serialized
;; Qt quint32 data type.

(define (read-content-length port)
  (define (content-length bytes)
    (bitwise-ior
     (arithmetic-shift (vector-ref bytes 4) 24)
     (arithmetic-shift (vector-ref bytes 5) 16)
     (arithmetic-shift (vector-ref bytes 6) 8)
     (vector-ref bytes 7)))
  (handle-exceptions exn ;; FIXME: check for read timeout
    #f
    (let ((bytes (make-vector 8))
          (first-byte (read-byte port)))
      (if (eof-object? first-byte)
          #f
          (begin
            (vector-set! bytes 0 first-byte)
            (let loop ((i 1))
              (if (fx> i 7)
                  (content-length bytes)
                  (let ((byte (read-byte port)))
                    (debug (sprintf "[~a] => ~a" i byte))
                    (vector-set! bytes i byte)
                    (loop (fx+ i 1))))))))))

(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...)
     (let loop ((i 0))
       (unless (= i n)
         body ...
         (loop (fx+ i 1)))))))

(define (write-content-length data-str port)
  ;; FIXME: this is incomplete.  It just writes content length for
  ;; messages < 256 bytes.
  (let ((data-len (string-length data-str)))
    (repeat 2
      (cond ((< data-len 256)
             (repeat 3 (write-byte 0 port))
             (write-byte data-len port))
            (else (error 'write-content-length
                         "FIXME: attempt to write message > 255 bytes"))))))

;;; end of transport stuff

(define (send-message! data-str port)
  (write-content-length data-str port)
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
  (let ((content-length (read-content-length port)))
    (debug (conc "+++++ content-length = " content-length))
    (and content-length
         (read-string content-length port))))

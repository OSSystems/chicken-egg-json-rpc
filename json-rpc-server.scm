(use tcp json srfi-18 utils)

(include "common.scm")

(tcp-buffer-size 2048)

(define *remote-procedures* '())

(define-syntax define-json
  (syntax-rules ()
    ((_ (name args ...)  body ...)
     (set! *remote-procedures*
           (cons (cons 'name
                       (lambda (args ...) body ...))
                 *remote-procedures*)))))


(define (register-procedure name proc)
  (set! *remote-procedures*
        (cons (cons name proc)
              *remote-procedures*)))


(define (call-procedure proc-name args id)
  (let ((proc (alist-ref proc-name *remote-procedures*)))
    (if proc
        (handle-exceptions exn
          (make-error-response -32603 "Internal error")
          (make-response (apply proc args) id))
        (make-error-response -32601 "Method not found."))))


(define (json->string data)
  (with-output-to-string
    (lambda ()
      (json-write data))))


(define (make-response data id)
  (json->string
   `#(("jsonrpc" . ,json-rpc-version)
      ("result"  . ,data)
      ("id"      . ,id))))


(define (make-error-response code message)
  (json->string
   `#(("jsonrpc" . ,json-rpc-version)
      ("error"   . #(("code" . ,code)
                     ("message" . ,message)))
      ("id"      . "null"))))


(define (dispatch-request req)
  (let* ((req (vector->list req))
         (method (alist-ref "method" req equal?))
         (params (alist-ref "params" req equal?))
         (id (alist-ref "id" req equal?)))
    (if method
        (call-procedure (string->symbol method) (or params '()) id)
        (make-error-response -32601 "Method not found."))))


(define (start-json-rpc-server port)
  (let ((listener (tcp-listen port)))
    (let accept-next-connection ()
      (receive (in out)
        (tcp-accept listener)
        (thread-start!
         (lambda ()
           (let loop ()
             (handle-exceptions e
               (begin
                 (print-call-chain)
                 (print-error-message e)
                 (make-error-response -32603 "Internal error."))
               (and-let* ((message (receive-message in))
                          (req (with-input-from-string message json-read))
                          (response (dispatch-request req)))
                 (send-message-dup! "{\"jsonrpc\": \"2.0\", \"signal\": \"foo\"}" out)
                 (send-message! response out)
                 (loop))))))
        (accept-next-connection)))))

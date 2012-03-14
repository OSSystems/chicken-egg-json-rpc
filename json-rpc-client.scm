(module json-rpc-client
  (connect! disconnect! json-call register-signal-handler!)

(import chicken scheme ports data-structures posix extras srfi-1)
(use json tcp srfi-18)

(include "common.scm")

(define (make-request method args id)
  (let ((data
         (append `(("jsonrpc" . ,json-rpc-version)
                   ("method" . ,(->string method)))
                 (cond ((or (not args) (null? args))
                        '())
                       ((vector? (car args))
                        `(("params" . ,(car args))))
                       (else `(("params" . ,args))))
                 `(("id" . ,id)))))
    (list->vector data)))


(define-record signal proc args)
(define-record method id result)

(define (parse-response response)
  (let* ((data (vector->list response))
         (id (alist-ref "id" data equal?)))
    (or (and-let* ((signal (alist-ref "signal" data equal?))
                   (args (or (alist-ref "params" data equal?) '())))
          (make-signal signal args))
        (or (and-let* ((err (alist-ref "error" data equal?))
                       (err (vector->list err))
                       (code (alist-ref "code" err equal?))
                       (message (alist-ref "message" err equal?)))
              (signal
               (make-composite-condition
                (make-property-condition
                 'exn
                 'location 'parse-response
                 'message (conc message
                                " (message id: " (alist-ref "id" data equal?) ", "
                                " error " code ")"))
                (make-property-condition 'json-error))))
            (make-method id (alist-ref "result" data equal?))))))


(define *inport* #f)
(define *outport* #f)


(define (connect! host port)
  (debug "Connecting")
  (let-values (((in out) (tcp-connect host port)))
    (set! *inport* in)
    (set! *outport* out)
    (debug "Running dispatcher")
    (json-rpc-dispatcher)))


(define (disconnect!)
  (debug "Disconnecting")
  (close-input-port *inport*)
  (close-output-port *outport*))


(define (with-connection host port thunk)
  (connect! host port)
  (let ((disconnect? #t))
    (handle-exceptions exn
      (begin
        (set! disconnect? #f)
        (print-call-chain)
        (print-error-message exn)
        (disconnect!))
      (thunk))
    (when disconnect?
      (disconnect!))))


(define *signal-handlers* '())

(define (register-signal-handler! name proc)
  (set! *signal-handlers*
        (cons (cons name proc)
              *signal-handlers*)))


(define (signal-call signal-name args)
  (debug (conc "Handling signal " signal-name))
  (let ((proc (alist-ref (string->symbol signal-name) *signal-handlers* equal?)))
    (if proc
        (apply proc args)
        (signal
         (make-composite-condition
          (make-property-condition
           'exn
           'location signal-name
           'message (conc "No signal handler for " signal-name)
           (make-property-condition 'signal-handler-error)))))))


(define json-call
  ;; Sends a request to the server and returns the response.
  (let ((id 0))
    (lambda (method #!optional args)
      (set! id (+ id 1))
      (let* ((req-data (make-request method args id))
             (req-data-str
              (with-output-to-string
                (lambda ()
                  (json-write req-data)))))
        (debug req-data-str 'out)
        (send-message! req-data-str *outport*)
        (let loop ()
          ; (debug *available-results*)
          (or (and-let* ((res (get-result id)))
                res)
              (begin
                (thread-sleep! 0.1)
                (loop))))))))


(define *available-results* '())

(define (add-available-result! method)
  (set! *available-results*
        (cons (cons (method-id method) (method-result method))
              *available-results*)))


(define (remove-available-result! method-id)
  (set! *available-results*
        (alist-delete method-id *available-results* equal?)))


(define (get-result id)
  (alist-ref id *available-results* equal?))


(define (json-rpc-dispatcher)
  (thread-start!
   (lambda ()
     (let loop ()
       (let* ((raw-response (receive-message *inport*))
              (_ (debug raw-response 'in))
              (response
               (and raw-response
                    (parse-response (with-input-from-string raw-response json-read)))))
         (cond ((not response)
                (void))
               ((signal? response)
                (signal-call (signal-proc response) (signal-args response)))
               ((method? response)
                (add-available-result! response))
               (else (error 'dispatcher "Unknonw object" response))))
       (loop)))))

) ;; end module
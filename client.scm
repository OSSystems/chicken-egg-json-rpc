(load "json-rpc-client.scm")

(register-signal-handler!
 'foo
 (lambda ()
   (print "----------> foo")))

(register-signal-handler!
 'Peripherals.applyMouseConfiguration
 (lambda ()
   (print "====================== Peripherals.applyMouseConfiguration")))

(connect! "127.0.0.1" 7777)

(print (json-call 'Peripherals.kbdModel))

;; (with-connection "127.0.0.1" 7777
;;   (lambda ()
;;     (let ((d (dispatcher)))
;;       (print (json-call 'Peripherals.kbdModel))
;;       (thread-join! d)
;;       )))

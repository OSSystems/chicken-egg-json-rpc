(load "json-rpc-server.scm")

(define-json (echo foo)
  foo)

(define-json (add a b)
  (+ a b))

(define (fatorial n)
  (if (< n 2)
      1
      (* n (fatorial (- n 1)))))

(register-procedure 'fatorial fatorial)


;;;
;;; Ossa peripherals
;;;

(define model "br")

(define-json (Peripherals.kbdModel)
  model)

(define-json (Peripherals.kbdAvailableModels)
  '("foo" "bar" "baz"))

(define-json (Peripherals.kbdAvailableLayouts)
  '("layout1" "layout2" "layout3"))

(define-json (Peripherals.kbdAvailableVariants)
  '("variant1" "variant2" "variant3"))

(define-json (Peripherals.kbdLayout)
  "layout")

(define-json (Peripherals.kbdVariant)
  "variant")

(define-json (kbdSetModel m)
  (set! model m))


(start-json-rpc-server 7777)

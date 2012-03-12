(use awful html-tags html-utils regex)

(load "json-rpc-client.scm")

(register-signal-handler!
 'foo
 (lambda ()
   (print "----------> foo")))

(register-signal-handler!
 'Peripherals.applyMouseConfiguration
 (lambda ()
   (print "====================== Peripherals.applyMouseConfiguration")))


(register-signal-handler!
 'Peripherals.applyKeyboardConfiguration
 (lambda ()
   (print "===================== Peripherals.applyKeyboardConfiguration")))


(connect! "10.2.254.32" 7777)

(define (kbd:layouts)
  (let ((all-layouts (json-call 'Peripherals.kbdAvailableInputLayouts)))
    (pp all-layouts)
    (filter-map
     (lambda (lang/layout)
       (let ((lang (car lang/layout))
             (layout (cdr lang/layout)))
         (and
          (string-match
           "^(br(/nativo-us|/nodeadkeys|$)|^jp$|us(/alt-intl|/altgr-intl|/intl|$)|es(/nodeadkeys|/deadtilde|$)|latam)"
           lang)
          lang/layout)))
     (vector->list all-layouts))))

(define-page (main-page-path)
  (lambda ()
    (ajax "set-kbd-layout" 'kbd-layouts 'change
          (lambda ()
            (with-request-variables (layout)
              (json-call 'Peripherals.kbdSetInputLayout (list layout))
              layout))
          arguments: `((layout . "$('#kbd-layouts').val()"))
          target: "echo-area")

    (++ (combo-box "kbd-layouts" (kbd:layouts))
        (<div> id: "echo-area")))
  use-ajax: #t)

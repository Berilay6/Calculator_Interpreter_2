#lang racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (evaluate input)
  (with-handlers ([exn:fail? (lambda (e) (format "Error: ~a" (exn-message e)))])
    (format "Result: ~a" (eval (read (open-input-string input)) ns))))

(define (calculator-loop)
  (display "> ")
  (let ((input (read-line)))
    (unless (equal? input "q")
      (displayln (evaluate input))
      (calculator-loop))))

(displayln "Enter your arithmetic expression (Press 'q' to exit)")
(calculator-loop)

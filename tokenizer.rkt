#lang racket



(define (process-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line)))
        (when line
          (let* ((tokens (tokenize line))
                 (types (classify-tokens tokens)))
            (if (check-syntax types)
                (begin
                  (displayln line)
                  (displayln types))
                (displayln (format "Invalid syntax: ~a" line))))
          (loop (read-line)))))))

(define (tokenize line)
  (string-split line))

(define (classify-tokens tokens)
  (map (lambda (token)
         (cond ((string->number token) 'idx)
               ((equal? token "read") 'read)
               ((equal? token "write") 'write)
               ((equal? token "goto") 'goto)
               ((equal? token "gosub") 'gosub)
               ((equal? token "return") 'return)
               ((regexp-match #rx"[a-zA-Z]+" token) 'id)
               ((equal? token "(") 'lparen)
               ((equal? token ")") 'rparen)
               ((equal? token "+") 'plus)
               ((equal? token "-") 'minus)
               ((equal? token "=") 'equals)
               ((equal? token ":") 'colon)
               (else (error (format "Unknown token: ~a" token)))))
       tokens))

(define (check-syntax types)
  (with-handlers ([exn:fail:read? (lambda (exn) #f)])
    (read (open-input-string (string-join (map symbol->string types) " ")))
    #t))

(process-file "file06.txt")

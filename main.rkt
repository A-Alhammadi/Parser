#lang racket

(require racket/file)



(define lines (file->lines "file.txt"))
(display lines)

(define (parse filename)
  (let* ((lines (file->lines filename))
         (error-lines (get-syntax-error-lines lines)))
    (if (null? error-lines)
        "Accept"
        (format "Syntax error on line ~a" (car error-lines)))))

(define (linelist lines)
(or (null? lines)
(and (line? (car lines))
(linelist (cdr lines)))))

(define (line? line-str)
(let ((parts (string-split line-str)))
(and (idx (car parts))
(stmt (cadr parts))
(linetail (cddr parts))
(string=? (car (reverse parts)) ""))))

(define (idx str)
(and (nonzero-digit? (string-ref str 0))
(digits (substring str 1))))

(define (linetail parts)
(or (null? parts)
(and (string=? ":" (car parts))
(stmt (cadr parts)))))

(define (stmt str)
(cond ((equal? str 'if) (if-stmt (cddr str)))
((equal? str 'read) (read-stmt (cddr str)))
((equal? str 'write) (write-stmt (cddr str)))
((equal? str 'goto) (goto-stmt (cddr str)))
((equal? str 'gosub) (gosub-stmt (cddr str)))
((equal? str 'return) (return-stmt))
((id? str) (assign-stmt str))
(else #f)))

(define (if-stmt parts)
(and (expr (car parts))
(string=? "then" (cadr parts))
(stmt (caddr parts))))

(define (read-stmt parts)
(id? (car parts)))

(define (write-stmt parts)
(expr (car parts)))

(define (goto-stmt parts)
(idx (car parts)))

(define (gosub-stmt parts)
(idx (car parts)))

(define (return-stmt)
#t)

(define (assign-stmt str)
(and (id? (car (string-split str "=")))
(expr (cadr (string-split str "=")))))

(define (expr str)
(or (id? str)
(num? str)
(and (string=? "(" (string-ref str 0))
(string=? ")" (string-ref str (- (string-length str) 1)))
(expr (substring str 1 (- (string-length str) 1))))
(and (id? (car (string-split str etail)))
(etail (cdr (string-split str etail))))))

(define (etail parts)
(or (null? parts)
(and (string=? "+" (car parts))
(expr (cadr parts))
(etail (cddr parts)))
(and (string=? "-" (car parts))
(expr (cadr parts))
(etail (cddr parts)))
(and (string=? "=" (car parts))
(expr (cadr parts)))
#t))

(define (id? str)
(and (string=? (string-downcase str) str)
(not (string=? str ""))
(regexp-match #rx"^[a-z]+$" str)))

(define (num? str)
(or (and (numsign (string-ref str 0))
(digits (substring str 1)))
(digits str)))

(define (numsign ch)
  (or (char=? ch #\+)
      (char=? ch #\-)
      #t))

(define (digits str)
  (cond ((string=? str "") #f)
        ((and (digit? (string-ref str 0))
              (digits (substring str 1)))
         (string-append (string (string-ref str 0))
                        (digits (substring str 1))))
        (else #f)))

(define (digit? ch)
  (char<? #\0 ch #\9))

(define (exp exp-str)
  (cond ((id? exp-str) #t)
        ((num? exp-str) #t)
        ((equal? exp-str 'true) #t)
        ((equal? exp-str 'false) #t)
        ((and (string=? "(" (string-take exp-str 1))
              (string=? ")" (string-take-right exp-str 1)))
         (exp (string-drop (string-take-right (string-drop exp-str 1) -1) 1)))
        ((and (binary-op? (string-ref exp-str 0))
              (exp (substring exp-str 1)))
         #t)
        (else #f)))

(define (binary-op? ch)
  (or (char=? ch #\+)
      (char=? ch #\-)
      (char=? ch #\*)
      (char=? ch #\/)))

(define (get-syntax-error-lines lines)
  (let loop ((lines lines) (linenum 1) (errors '()))
    (if (null? lines)
        (reverse errors)
        (if (line? (car lines))
            (loop (cdr lines) (+ linenum 1) errors)
            (loop (cdr lines) (+ linenum 1) (cons linenum errors))))))

(define (nonzero-digit? ch)
  (and (digit? ch)
       (not (char=? ch #\0))))

(define (term-list parts)
  (if (null? parts)
      '()
      (let ((op (or (car parts) ""))
            (rest (cdr parts)))
        (if (or (string=? op "+") (string=? op "-"))
            (cons op (term-list rest))
            (cons (factor op)
                  (term-list (rest)))))))

(define (term str)
  (let ((factor (factor (car (reverse (factor-list (string-split str)))))))
    (if (null? factor)
        (error "Syntax error in term")
        (car factor))))

(define (factor-list parts)
  (if (null? parts)
      '()
      (let ((op (or (car parts) ""))
            (rest (cdr parts)))
        (if (or (string=? op "*") (string=? op "/"))
            (cons op (factor-list rest))
            (cons (term op) (factor-list rest))))))

(define (factor str)
  (cond ((num? str) str)
        ((id? str) str)
        ((equal? (car (string->list str)) #\() (expr (substring str 1 (- (string-length str) 1))))
        (else (error "Syntax error in factor"))))

(define (string-take s n)
  (cond
    [(not (string? s)) (error 'string-take "expected a string, but received: ~e" s)]
    [(not (and (integer? n) (exact? n) (<= 0 n (string-length s))))
     (error 'string-take "index out of range: ~e" n)]
    [else (substring s 0 n)]))

(define (string-take-right s n)
  (let ((len (string-length s)))
    (if (and (integer? n) (exact? n) (<= 0 n len))
        (substring s (- len n) len)
        (error 'string-take-right "invalid arguments"))))

(define (string-drop s n)
  (string? s)
  (let ((len (string-length s)))
    (integer? n 0 len)
    (substring s n)))

(parse "file.txt")

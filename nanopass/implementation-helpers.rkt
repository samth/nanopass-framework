#lang racket
(provide
 ;; formatting
 format printf pretty-print
 
 ;; listy stuff
 iota make-list list-head
 
 ;; gensym stuff (related to nongenerative languages)
 gensym regensym
 
 ;; source-information stuff
 syntax->source-information
 source-information-source-file
 source-information-byte-offset-start
 source-information-char-offset-start
 source-information-byte-offset-end
 source-information-char-offset-end
 source-information-position-line
 source-information-position-column
 source-information-type
 provide-full-source-information
 
 ;; library export stuff (needed for when used inside module to
 ;; auto-indirect export things)
 indirect-export
 
 ;; useful for warning and error items
 warningf errorf
 
 ;; used to get the best performance from hashtables
 eq-hashtable-set! eq-hashtable-ref
 
 ;; needed to know what code to generate
 optimize-level
 
 ;; the base record, so that we can use gensym syntax
 define-nanopass-record
 
 ;; failure token so that we can know when parsing fails with a gensym
 np-parse-fail-token
 
 ;; handy syntactic stuff
 (for-syntax with-implicit)
 
 ;; apparently not neeaded (or no longer needed)
 ; scheme-version= scheme-version< scheme-version> scheme-version>=
 ; scheme-version<= with-scheme-version gensym? errorf with-output-to-string
 ; with-input-from-string
 )

(provide (all-defined-out))
(define fx=? =)
(define fold-right foldr)
(define (remp f l) (filter-not f l))
(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))
(define (exists f l) (ormap f l))
(define for-all andmap)
(define (find f l)
  (cond [(memf f l) => car] [else #f]))
(define memp memf)

(define optimize-level 'finishme)
(begin-for-syntax
  (define-syntax with-implicit
    (syntax-rules ()
      [(_ (id name ...) body bodies ...)
       (with-syntax ([name (datum->syntax #'id 'name)] ...) body bodies ...)])))

; the base language
(define-syntax define-nanopass-record
  (lambda (x)
    (syntax-case x ()
      [(k) (with-implicit (k nanopass-record nanopass-record? nanopass-record-tag)
                          #'(define-record-type (nanopass-record make-nanopass-record nanopass-record?)
                              (nongenerative nanopass-record_d47f8omgluol6otrw1yvu5-0)
                              (fields (immutable tag nanopass-record-tag))))])))

;; another gensym listed into this library
(define np-parse-fail-token 'np-parse-fail-token_dlkcd4b37swscag1dvmuiz-13)

(define-syntax eq-hashtable-set! (make-rename-transformer #'hash-set!))
(define-syntax eq-hashtable-ref (make-rename-transformer #'hash-ref))

(define list-head
  (lambda (orig-ls orig-n)
    (let f ([ls orig-ls] [n orig-n])
      (cond
        [(zero? n) '()]
        [(null? ls) (error 'list-head "index out of range" orig-ls orig-n)]
        [else (cons (car ls) (f (cdr ls) (- n 1)))]))))

(define iota
  (lambda (n)
    (let loop ([n n] [ls '()])
      (if (zero? n)
          ls
          (let ([n (- n 1)])
            (loop n (cons n ls)))))))

(define (regensym . args) (gensym))

#;
(define regensym
  (case-lambda
    [(gs extra)
     (unless (gensym? gs) (errorf 'regensym "~s is not a gensym" gs))
     (unless (string? extra) (errorf 'regensym "~s is not a string" extra))
     (let ([pretty-name (parameterize ([print-gensym #f]) (format "~s" gs))]
           [unique-name (gensym->unique-string gs)])
       (with-input-from-string (format "#{~a ~a~a}" pretty-name unique-name extra) read))]
    [(gs extra0 extra1)
     (unless (gensym? gs) (errorf 'regensym "~s is not a gensym" gs))
     (unless (string? extra0) (errorf 'regensym "~s is not a string" extra0))
     (unless (string? extra1) (errorf 'regensym "~s is not a string" extra1))
     (with-output-to-string (lambda () (format "~s" gs)))
     (let ([pretty-name (parameterize ([print-gensym #f]) (format "~s" gs))]
           [unique-name (gensym->unique-string gs)])
       (with-input-from-string (format "#{~a~a ~a~a}" pretty-name extra0 unique-name extra1) read))]))

(define provide-full-source-information
  (make-parameter #f (lambda (x) (and x #t))))

(struct source-information
  (source-file byte-offset-start char-offset-start byte-offset-end
               char-offset-end position-line position-column type)
  #:prefab)
(define (make-source-information a type)
  (source-information
   (syntax-source a) (syntax-position a)
   (syntax-position a) #f #f (syntax-line a)
   (syntax-column a) type))

(define syntax->source-information
  (lambda (stx)
    (let loop ([stx stx] [type 'at])
      (cond
        [(syntax? stx) (make-source-information stx type)]
        [(pair? stx) (or (loop (car stx) 'near) (loop (cdr stx) 'near))]
        [else #f]))))

(define-syntax warningf
  (syntax-rules ()
    [(_ who fmt args ...) (warning who (format fmt args ...))]))

(define-syntax errorf
  (syntax-rules ()
    [(_ who fmt args ...) (error who (format fmt args ...))]))

(define-syntax indirect-export
  (syntax-rules ()
    [(_ id indirect-id ...) (define t (if #f #f))]))
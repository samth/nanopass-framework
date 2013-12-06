#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for detatils
(provide
 ;; auxiliary keywords for language/pass definitions
 extends definitions entry terminals nongenerative-id
 
 ;; predicates for looking for identifiers independent of context
 ellipsis? unquote? colon? arrow? plus? minus? double-arrow? 
 
 ;; things for dealing with syntax and idetnfieris
 all-unique-identifiers? construct-id gentemp bound-id-member? bound-id-union
 partition-syn (for-syntax datum) datum
 
 ;; things for dealing with language meta-variables
 meta-var->raw-meta-var combine unique-symbol 
 
 ;; convenience syntactic forms
 rec with-values define-who 
 
 ;; source information funtions
 syntax->source-info 
 
 ;;; stuff imported from implementation-helpers
 
 ;; formatting
 format printf pretty-print
 
 ;; listy stuff
 iota make-list list-head
 
 ;; gensym stuff (related to nongenerative languages)
 gensym regensym
 
 ;; library export stuff (needed for when used inside module to
 
 ;; useful for warning items
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
 with-extended-quasiquote with-auto-unquote extended-quasiquote
 (for-syntax with-implicit with-r6rs-quasiquote))

(require "implementation-helpers.rkt"
         racket/fixnum
         (for-syntax racket racket/fixnum "implementation-helpers.rkt"))

(module stxhelp racket
  (require "implementation-helpers.rkt")
  (provide datum with-r6rs-quasiquote)
  (define-syntax datum
    (syntax-rules ()
      [(_ e) (syntax->datum #'e)]))
  
  (define-syntax with-r6rs-quasiquote
    (lambda (x)
      (syntax-case x ()
        [(k . body)
         (with-implicit (k quasiquote)
                        #'(let-syntax ([quasiquote (syntax-rules () [(_ x) `x])]) . body))]))))

(require 'stxhelp (for-syntax 'stxhelp))

(define-syntax extended-quasiquote
  (lambda (x)
    (define gather-unquoted-exprs
      (lambda (body)
        (let f ([body body] [t* '()] [e* '()])
          (syntax-case body (unquote unquote-splicing)
            [(unquote x)
             (identifier? #'x)
             (values body (cons #'x t*) (cons #'x e*))]
            [(unquote-splicing x)
             (identifier? #'x)
             (values body (cons #'x t*) (cons #'x e*))]
            [(unquote e)
             (with-syntax ([(t) (generate-temporaries '(t))])
               (values #'(unquote t) (cons #'t t*) (cons #'e e*)))]
            [(unquote-splicing e)
             (with-syntax ([(t) (generate-temporaries '(t))])
               (values #'(unquote-splicing t) (cons #'t t*) (cons #'e e*)))]
            [(tmpl0 . tmpl1)
             (let-values ([(tmpl0 t* e*) (f #'tmpl0 t* e*)])
               (let-values ([(tmpl1 t* e*) (f #'tmpl1 t* e*)])
                 (values #`(#,tmpl0 . #,tmpl1) t* e*)))]
            [atom (values #'atom t* e*)]))))
    (define build-list
      (lambda (body orig-level)
        (let loop ([body body] [level orig-level])
          (syntax-case body (unquote unquote-splicing)
            [(tmpl0 ... (unquote e))
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
               (cond
                 [(fx=? level 0) #'(tmpl0 ... (unquote e))]
                 [(fx=? level 1) #'(tmpl0 ... (unquote-splicing e))]
                 [else (let loop ([level level] [e #'e])
                         (if (fx=? level 1)
                             #`(tmpl0 ... (unquote-splicing #,e))
                             (loop (fx- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... (unquote-splicing e))
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
               (cond
                 [(fx=? level 0) #'(tmpl0 ... (unquote-splicing e))]
                 [else (let loop ([level level] [e #'e])
                         (if (fx= level 0)
                             #`(tmpl0 ... (unquote-splicing #,e))
                             (loop (fx- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... tmpl1 ellipsis)
             (eq? (datum ellipsis) '...)
             (loop #'(tmpl0 ... tmpl1) (fx+ level 1))]
            [(tmpl0 ... tmpl1)
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
               (let-values ([(tmpl1 t* e*) (gather-unquoted-exprs #'tmpl1)])
                 (when (null? e*)
                   (raise-syntax-error 'extended-quasiquote
                                     "no variables found in ellipsis expression" body))
                 (let loop ([level level]
                            [e #`(map (lambda #,t*
                                        (extended-quasiquote
                                         #,tmpl1))
                                      . #,e*)])
                   (if (= level 1)
                       #`(tmpl0 ... (unquote-splicing #,e))
                       (loop (fx- level 1) #`(apply append #,e))))))]))))
    (define rebuild-body
      (lambda (body level)
        (syntax-case body (unquote unquote-splicing)
          [(unquote e) #'(unquote e)]
          [(unquote-splicing e) #'(unquote-splicing e)]
          [(tmpl0 ... tmpl1 ellipsis)
           (eq? (datum ellipsis) '...)
           (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (fx+ level 1))])
             #'(tmpl0 ...))]
          [(tmpl0 ... tmpl1 ellipsis . tmpl2)
           (eq? (datum ellipsis) '...)
           (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (fx+ level 1))]
                         [tmpl2 (rebuild-body #'tmpl2 level)])
             #'(tmpl0 ... . tmpl2))]
          [(tmpl0 ... tmpl1)
           (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) level)]
                         [tmpl1 (rebuild-body #'tmpl1 level)])
             #'(tmpl0 ... tmpl1))]
          [(tmpl0 ... tmpl1 . tmpl2)
           (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ... tmpl1) level)]
                         [tmpl2 (rebuild-body #'tmpl2 level)])
             #'(tmpl0 ... . tmpl2))]
          [other #'other])))
    (syntax-case x ()
      [(k body)
       (with-syntax ([body (rebuild-body #'body 0)])
         #'(quasiquote body))])))

(define-syntax with-extended-quasiquote
  (lambda (x)
    (syntax-case x ()
      [(k . body)
       (with-implicit (k quasiquote)
                      #'(let-syntax ([quasiquote (syntax-rules ()
                                                   [(_ x) (extended-quasiquote x)])])
                          
                          . body))])))

(define-syntax with-auto-unquote
  (lambda (x)
    (syntax-case x ()
      [(k (x* ...) . body)
       (with-implicit (k quasiquote)
                      #'(let-syntax ([quasiquote
                                      (lambda (x)
                                        (define replace-vars
                                          (let ([vars (list #'x* ...)])
                                            (lambda (b)
                                              (let f ([b b])
                                                (syntax-case b ()
                                                  [id (identifier? #'id)
                                                      (if (memp (lambda (var) (free-identifier=? var #'id)) vars)
                                                          #'(unquote id)
                                                          #'id)]
                                                  [(a . d) (with-syntax ([a (f #'a)] [d (f #'d)]) #'(a . d))]
                                                  [atom #'atom])))))
                                        (syntax-case x ()
                                          [(_ b)
                                           (with-syntax ([b (replace-vars #'b)])
                                             #'`b)]))])
                          . body))])))

(define all-unique-identifiers?
  (lambda (ls)
    (and (for-all identifier? ls)
         (let f ([ls ls])
           (if (null? ls)
               #t
               (let ([id (car ls)] [ls (cdr ls)])
                 (and (not (memp (lambda (x) (free-identifier=? x id)) ls))
                      (f ls))))))))

(define-syntax with-values
  (syntax-rules ()
    [(_ p c) (call-with-values (lambda () p) c)]))

(define-syntax rec
  (syntax-rules ()
    [(_ name proc) (letrec ([name proc]) name)]
    [(_ (name . arg) body body* ...)
     (letrec ([name (lambda arg body body* ...)]) name)]))

(define-syntax define-auxiliary-keyword
  (syntax-rules ()
    [(_ name)
     (define-syntax name 
       (lambda (x)
         (raise-syntax-error 'name "misplaced use of auxiliary keyword" x)))]))

(define-syntax define-auxiliary-keywords
  (syntax-rules ()
    [(_ name* ...)
     (begin
       (define-auxiliary-keyword name*) ...)]))

(define-auxiliary-keywords extends definitions entry terminals nongenerative-id)

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k name expr)
       (with-implicit (k who)
                      #'(define name (let () (define who 'name) expr)))]
      [(k (name . fmls) expr exprs ...)
       #'(define-who name (lambda (fmls) expr exprs ...))])))

;;; moved from meta-syntax-dispatch.ss and nano-syntax-dispatch.ss
(define combine
  (lambda (r* r)
    (if (null? (car r*))
        r
        (cons (map car r*) (combine (map cdr r*) r))))) 

;;; moved from meta-syntax-dispatch.ss and syntaxconvert.ss
(define ellipsis?
  (lambda (x)
    (and (identifier? x) (free-identifier=? x (syntax (... ...)))))) 

(define unquote?
  (lambda (x)
    (and (identifier? x) (free-identifier=? x (syntax unquote)))))

(define unquote-splicing?
  (lambda (x)
    (and (identifier? x) (free-identifier=? x (syntax unquote-splicing)))))

(define plus?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'+)
             (eq? (syntax->datum x) '+)))))

(define minus?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'-)
             (eq? (syntax->datum x) '-)))))

(define double-arrow?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'=>)
             (eq? (syntax->datum x) '=>)))))

(define colon?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #':)
             (eq? (syntax->datum x) ':)))))

(define arrow?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'->)
             (eq? (syntax->datum x) '->)))))

;;; unique-symbol produces a unique name derived the input name by
;;; adding a unique suffix of the form .<digit>+.  creating a unique
;;; name from a unique name has the effect of replacing the old
;;; unique suffix with a new one.

(define unique-suffix
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (number->string count))))

(define unique-symbol
  (lambda (id . id*)
    (string->symbol
     (string-append
      (fold-right
       (lambda (id str) (string-append str ":" (symbol->string (syntax->datum id))))
       (symbol->string (syntax->datum id)) id*)
      "."
      (unique-suffix)))))

; TODO: at some point we may want this to be a little bit more
; sophisticated, or we may want to have something like a regular
; expression style engine where we bail as soon as we can identify
; what the meta-var corresponds to.
(define meta-var->raw-meta-var
  (lambda (sym)
    (let ([s (symbol->string sym)])
      (let f ([i (fx- (string-length s) 1)])
        (cond
          [(fx=? i -1) sym]
          [(or 
            (char=? #\* (string-ref s i))
            (char=? #\^ (string-ref s i))
            (char=? #\? (string-ref s i)))
           (f (fx- i 1))]
          [else (let f ([i i])
                  (cond
                    [(fx=? i -1) sym]
                    [(char-numeric? (string-ref s i)) (f (fx- i 1))]
                    [else (string->symbol (substring s 0 (fx+ i 1)))]))])))))

;; This concatenates two identifiers with a string inbetween like "-"
(define construct-id
  (lambda (tid x . x*)
    (define ->str
      (lambda (x)
        (cond
          [(string? x) x]
          [(identifier? x) (symbol->string (syntax->datum x))]
          [(symbol? x) (symbol->string x)]
          [else (error 'construct-id "invalid input ~s" x)])))
    (unless (identifier? tid)
      (error 'construct-id "template argument ~s is not an identifier" tid))
    (datum->syntax 
     tid 
     (string->symbol (apply string-append (->str x) (map ->str x*))))))

(define-syntax partition-syn
  (lambda (x)
    (syntax-case x ()
      [(_ ls-expr () e0 e1 ...) #'(begin ls-expr e0 e1 ...)]
      [(_ ls-expr ([set pred] ...) e0 e1 ...)
       (with-syntax ([(pred ...) 
                      (let f ([preds #'(pred ...)])
                        (if (null? (cdr preds))
                            (if (free-identifier=? (car preds) #'otherwise)
                                (list #'(lambda (x) #t))
                                preds)
                            (cons (car preds) (f (cdr preds)))))])
         #'(let-values ([(set ...)
                         (let f ([ls ls-expr])
                           (if (null? ls)
                               (let ([set '()] ...) (values set ...))
                               (let-values ([(set ...) (f (cdr ls))])
                                 (cond
                                   [(pred (car ls))
                                    (let ([set (cons (car ls) set)])
                                      (values set ...))]
                                   ...
                                   [else (error 'partition-syn 
                                                "no home for ~s"
                                                (car ls))]))))])
             e0 e1 ...))])))

(define gentemp
  (lambda ()
    (car (generate-temporaries '(#'t))))) 

(define bound-id-member? 
  (lambda (id id*)
    (and (not (null? id*))
         (or (bound-identifier=? id (car id*))
             (bound-id-member? id (cdr id*)))))) 

(define bound-id-union ; seems to be unneeded
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(bound-id-member? (car ls1) ls2) (bound-id-union (cdr ls1) ls2)]
      [else (cons (car ls1) (bound-id-union (cdr ls1) ls2))]))) 

(define syntax->source-info
  (lambda (stx)
    (let ([si (syntax->source-information stx)])
      (and si
           (cond
             [(and (source-information-position-line si)
                   (source-information-position-column si))
              (format "~s line ~s, char ~s of ~a"
                      (source-information-type si)
                      (source-information-position-line si)
                      (source-information-position-column si)
                      (source-information-source-file si))]
             [(source-information-byte-offset-start si)
              (format "~s byte position ~s of ~a"
                      (source-information-type si)
                      (source-information-byte-offset-start si)
                      (source-information-source-file si))]
             [(source-information-char-offset-start si)
              (format "~s character position ~s of ~a"
                      (source-information-type si)
                      (source-information-char-offset-start si)
                      (source-information-source-file si))]
             [else (format "in ~a" (source-information-source-file si))])))))

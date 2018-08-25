;; Monadic parsing in Scheme

(define (string-car s) (string-ref s 0))
(define (string-cdr s) (substring s 1))

;; M Char
(define item
  (lambda (s)
    (if (string-null? s)
        '()
        (cons (string-car s) (string-cdr s)))))


(define (return a)
  (lambda (state)
    (cons a state)))


;; M a -> (a -> M b) -> M b
(define (>>= m k)
  (lambda (x)
    (let* ((ay (m x)))
      (if (null? ay)
          '()
          (let* ((a (car ay))
                 (y (cdr ay)))
            ((k a) y))))))

(define zero
  (lambda (s)
    '()))

(define-syntax letM*
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((name val) (name2 val2) ...) expr)
     (>>= val
          (lambda (name)
            (letM* ((name2 val2) ...)
                   expr))))))

(define +++
  (lambda (p q)
    (lambda (string)
      (let ((res (p string)))
        (if (null? res)
            (q string)
            res)))))

(define sat
  (lambda (p)
    (letM* ((c item))
           (if (p c)
               (return c)
               zero))))

(define (char c)
  (sat (lambda (t)
         (eq? t c))))

(define (many p)
  (+++ (many1 p) (return '())))

(define (many1 p)
  (letM* ((a p)
          (as (many p)))
         (return (cons a as))))

(define space
  (many (char #\space)))

(define (token p)
  (letM* ((a p)
          (_ space))
         (return a)))

(define (string s)
  (if (string-null? s)
      (return "")
      (let ((c (string-car s))
            (cs (string-cdr s)))
        (letM* ((_ (char c))
                (_ (string cs)))
               (return (cons c cs))))))


(define (symb cs)
  (token (string cs)))

(define (apply p)
  (letM* ((_ space))
         p))

(define (chainl p op a)
  (+++ (chainl1 p op)
       (return a)))

(define (chainl1 p op)
  (define (rest a)
    (+++ (letM* ((f op)
                 (b p))
                (rest (f a b)))
         (return a)))
  (letM* ((a p))
         (rest a)))

(define addop
  (+++ (letM* ((_ (symb "+")))
              (return '+))
       (letM* ((_ (symb "-")))
              (return '-))))

(define (:> m p)
  (letM* ((a m))
         (if (p a)
             (return a)
             zero)))

(define digit
  (letM* ((a (:> item char-numeric?)))
         (return (- (char->integer a)
                    (char->integer #\0)))))

(define (list->number nums)
  (if (null? (cdr nums))
      (car nums)
      (+ (* (car nums) (expt 10 (- (length nums) 1)))
         (list->number (cdr nums)))))

(define nat
  (letM* ((xs (many1 digit)))
         (return (list->number xs))))

(define natural
  (token nat))

;; Read a list of numbers like [1,2,3,4]
(define read-num-list
  (letM* ((_ (symb "["))
          (n natural)
          (ns (many (letM* ((_ (symb ","))) natural)))
          (_ (symb "]")))
         (return (cons n ns))))


;; Parse arithmetic expressions like
;; 2
;; 2 + 3
;; 2342  * 2839 + (3 +5)

;; Convert them into s-exps.
(define factor
  (+++ (letM* ((_ (symb "("))
               (e expr)
               (_ (symb ")")))
              (return e))
       natural))


(define term
  (letM* ((f factor))
         (+++ (letM* ((_ (symb "*"))
                      (t term))
                     ;; Change this line to
                     ;; (return (* f t))
                     ;; to evaluate.
                     (return `(* ,f ,t)))

              (return f))))

(define expr
  (letM* ((t term))
         (+++ (letM* ((_ (symb "+"))
                      (e expr))
                     ;; Change this line to
                     ;; (return (+ t e))
                     ;; to evaluate.
                     (return `(+ ,t ,e)))
              (return t))))


;; Get all the words in a sentence, space separated.
(define words
  (letM* ((_ space)
          (w (many (apply (many1 (sat (lambda (x) (not (eq? x #\space)))))))))
         (return (map list->string w))))


(define (sepby p sep)
  (+++ (sepby1 p sep)
       (return '())))

(define (sepby1 p sep)
  (letM* ((a p)
          (as (many (letM* ((_ sep)) p))))
         (return (cons a as))))

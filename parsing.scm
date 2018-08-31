;; Monadic parsing in Scheme

;; Scheme doesn't treat strings like lists, but we can!
(define (string-car s) (string-ref s 0))
(define (string-cdr s) (substring s 1))

;; Scheme doesn't have a `show' procedure like in Haskell, so make our
;; own.
(define (to-string o)
  (if (char? o) (string o) (object->string o)))

;; Strings in Scheme aren't lists like in Haskell.  Unfortunately this
;; causes problems later, so we fix it with cons-string.

;; (cons-string #\h "ello") => "hello".
(define (cons-string c s)
  (if (null? s)
      (to-string c)
      (string-concatenate/shared `(,(to-string c) ,s))))

;; Parse a single character. 
;; Parser Char
(define item
  (lambda (s)
    (if (string-null? s)
        '()
        (cons (string-car s) (string-cdr s)))))

(define (return a)
  (lambda (state)
    (cons a state)))

;; Parser a -> (a -> Parser b) -> Parser b
(define (>>= m k)
  (lambda (x)
    (let* ((ay (m x)))
      (if (null? ay)
          '()
          (let* ((a (car ay))
                 (y (cdr ay)))
            ((k a) y))))))

;; Failure.
;; Parser m -> ()
(define zero
  (lambda (s)
    '()))

;;; letM*
;; This allows for a similar notation to "do" in Haskell.
;; do a <- b
;;    c <- d
;;    return a : c

;; Is the same as

;; (letM* ((a b)
;;         (c d))
;;        (return (cons a c)))

(define-syntax letM*
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((name val) (name2 val2) ...) expr)
     (>>= val
          (lambda (name)
            (letM* ((name2 val2) ...)
                   expr))))))

;; Given two parsers p and q, try p then if that fails try q.
(define +++
  (lambda (p q)
    (lambda (string)
      (let ((res (p string)))
        (if (null? res)
            (q string)
            res)))))

;; Lift a predicate into a parser.
;; (Char -> Bool) -> Parser Char
(define sat
  (lambda (p)
    (letM* ((c item))
           (if (p c)
               (return c)
               zero))))

;; Make a parser that only accepts a certain character.
;; Char -> Parser Char
(define (char c)
  (sat (lambda (t)
         (eq? t c))))

;; Allows a parser p to be repeated zero or more times.
(define (many p)
  (+++ (many1 p) (return '())))

;; Allows a parser p to be repeated one or more times.
;; many and many1 are mutually recursive.
(define (many1 p)
  (letM* ((a p)
          (as (many p)))
         ;; We use cons-string here because we want to possibly
         ;; collect individual characters into strings.
         (return (cons-string a as))))

;; Eat whitespace.
(define space
  (many (char #\space)))

;; Turn a parser p into a "token" parser, i.e. one that also eats up
;; whitespsace following the parse.
(define (token p)
  (letM* ((a p)
          (_ space))
         (return a)))

;; Make a parser that only accepts a certain string s.
(define (str s)
  (if (string-null? s)
      (return "")
      (let ((c (string-car s))
            (cs (string-cdr s)))
        (letM* ((_ (char c))
                (_ (str cs)))
               ;; Use string-concatenate/shared for possible speedup,
               ;; also because no mutation is performed.
               (return (string-concatenate/shared `(,(string c) ,cs)))))))

;; Tokenize a string.
(define (symb cs)
  (token (str cs)))

;; Before applying parser p, eat up leading whitespace.
(define (apply p)
  (letM* ((_ space))
         p))

;; Haven't found a good use for chainl and chainl1, not sure if it
;; works as expected.  Taken from Hutton's paper on Monadic parsing.
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

;; Given a parser m and a predicate p, apply the parser and check the
;; result against the predicate, then succeed or fail based on that.
(define (:> m p)
  (letM* ((a m))
         (if (p a)
             (return a)
             zero)))

;; Parse a single numeric character.
(define digit
  (letM* ((a (:> item char-numeric?)))
         (return a)))

;; Parse a natural number.
(define nat
  (letM* ((xs (many1 digit)))
         (return (string->number xs))))

;; A natural number, with whitespace following.
(define natural
  (token nat))

;; Read a list of numbers in the format: [1,2,3,4]
(define read-num-list
  (letM* ((_ (symb "["))
          (n natural)
          (ns (many (letM* ((_ (symb ","))) natural)))
          (_ (symb "]")))
         (return (cons n ns))))


;;; Mathematical infix parsing.
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
          ;; FIXME: Looking bad!
          (w (many (apply (many1 (sat (lambda (x) (not (eq? x #\space)))))))))
         (return w)))

;; From a paper, forgot which one.
(define (sepby p sep)
  (+++ (sepby1 p sep)
       (return '())))

(define (sepby1 p sep)
  (letM* ((a p)
          (as (many (letM* ((_ sep)) p))))
         (return (cons a as))))

;; Parse an alphabetic character.
(define alpha
  (:> item char-alphabetic?))

;; Parse an alphanumeric character.
(define alpha-num
  (:> item (lambda (x)
             (or (char-alphabetic? x)
                 (char-numeric? x)))))

;; Consume a string until a character is hit.
(define (up-to c)
  (letM* ((a (many (sat (lambda (x) (not (eq? x c)))))))
         (return a)))

;; Given a string, treat it like a character set and create a parser
;; that only accepts characters pertaining to that character set.
(define (oneof string)
  (sat (lambda (x) (char-set-contains? (string->char-set string) x))))

;;; BNF parser.
;; Rules are taken directly from Wikipedia
;; (https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form#Further_examples).

;; <line-end> ::= <opt-whitespace> <EOL> | <line-end> <line-end>
(define line-end
  ;; Notice that the use of many can help eliminate uses of | in
  ;; recursive definitions.
  (many (letM* ((_ space))
               (char #\newline))))

;; <rule> ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
(define rule
  (letM* ((_ space)
          (_ (str "<"))
          (rn rule-name)
          (_ (str ">"))
          (_ space)
          (_ (str "::="))
          (_ space)
          (e bnf-expr)
          (_ line-end))
         (return `(bnf-rule ,rn ,e))))
;; <syntax> ::= <rule> | <rule> <syntax>
(define bnf-syntax
  (+++ (letM* ((r rule)
               (s bnf-syntax))
              (return (cons r s)))
       rule))

;; <literal> ::= '"' <text1> '"' | "'" <text2> "'"
(define literal
  (+++ (letM* ((_ (char #\"))
               (t1 text1)
               (_ (char #\")))
              (return `(l1 ,t1)))
       (letM* ((_ (char #\'))
               (t2 text2)
               (_ (char #\')))
              (return `(l2 ,t2)))))

;; <term> ::= <literal> | "<" <rule-name> ">"
(define bnf-term
  (+++ (letM* ((_ (char #\<))
               (rn rule-name)
               (_ (char #\>)))
              (return (list rn)))
       literal))

;; <list> ::= <term> | <term> <opt-whitespace> <list>
(define bnf-list
  (+++ (letM* ((t bnf-term)
               (_ space)
               (l bnf-list))
              (return (append t l)))
       bnf-term))

;; <expression> ::= <list> | <list> <opt-whitespace> "|" <opt-whitespace> <expression>
(define bnf-expr
  (+++ (letM* ((l bnf-list)
               (_ space)
               (_ (char #\|))
               (_ space)
               (e bnf-expr))
              (return `(or ,l ,e)))
       bnf-list))

;; <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
;; "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" |
;; "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" |
;; "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" |
;; "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
(define bnf-letter alpha)

;; <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" |
;; "9"
(define bnf-digit digit)

;; <symbol> ::= "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" |
;; "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" |
;; "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
(define bnf-symbol
  (oneof " |!#$%&()*+,-./:;>=<?@[\\^_`{}~"))

;; <character>      ::= <letter> | <digit> | <symbol>
(define bnf-character
  (+++ bnf-letter
       (+++ bnf-digit
            bnf-symbol)))

;; <character1> ::= <character> | "'"
(define character1
  (+++ bnf-character
       (char #\')))

;; <character2> ::= <character> | '"'
(define character2
  (+++ bnf-character
       (char #\")))

;; <text1> ::= "" | <character1> <text1>
(define text1
  (many character1))

;; <text2> ::= "" | <character2> <text2>
(define text2
  (many character2))

;; <rule-char> ::= <letter> | <digit> | "-"
(define rule-char
  (+++ bnf-letter
       (+++ bnf-digit
            (char #\-))))

;; <rule-name> ::= <letter> | <rule-name> <rule-char>
(define rule-name
  (letM* ((n (many (+++ bnf-letter
                        rule-char))))
         ;; Convert them to symbols so they look pretty.
         (return (string->symbol n))))


# monadic-parsing - Monadic parsing in Scheme

Normally boasted in Haskell, but this repository goes to show that the
same can be done in Scheme.

## Usage

A parser written in Haskell such as the following:
```haskell
my_parser :: Parser (Char, Char)
my_parser = do a <- item
               item
               b <- item
               return (a, b)
```

May be converted to the following Scheme equivalent (using the `doM*`
macro, which mimics Haskell).

```scheme
(define my_parser
  (doM* (a <- item)
        item
        (b <- item)
        (return `(,a . ,b))))
```

We can then use our parser, without needing to worry about all the
monadic magic happening underneath.

```scheme
(my-parser "hello") ;; => ((#\h . #\l) . "lo") ;; "lo" was not consumed.
(my-parser "ah") ;; => ()
```

You'll know that your parser failed when it returns the empty list.
Dive into the well-commented `parsing.scm` file to see operators like
`<:>`, `many`, `oneof`, `token` and more, with examples included!

## Included Parsers
- `expr`: Demonstration of parsing arithmetic expressions into s-exps
  _preserving_ operator precedence.  (i.e. `*` has higher precedence
  than `+`).
  - e.g. `2+(3*4)`, `2+3+10*8` etc.
- `read-num-list`: Read lists of numbers such as `[3,1,4,1,5]` and
  returns them as Scheme lists `(3 1 4 1 5)`.
  - Fails nicely (i.e. returns `()` on malformed input like `[1,2,]`
    etc.)
- `bnf-syntax`: Backus–Naur form parser.  BNF is a notation that is
  used to describe context-free grammars.
- `lisp-expr`: a reader for Lisp notation.  Supports numbers,
  booleans, strings, nested lists and dotted lists. For instance:
  `"((1 2) \"hello\" #t 'foo)"` => `((1 2) "hello" #t foo)` (don't
  forget to escape `"` in strings!).
  
Demo of the BNF parser below.
```scheme
(bnf-syntax "<integer> ::= <digit>|<integer><digit>")
;; => ((bnf-rule integer (+++ (digit) (integer digit))) . "")

```
## Context-Free Grammars
Because we can parse (and likely convert) BNF notation into our parser
syntax, we should be able to parse all _unambiguous_ context-free
grammars.  Of course, one could still write an ambiguous grammar but
this parser will not return all the possible parse trees.

Consider the following grammar, written in BNF notation, that
describes a language containing an equal amount of `0`s and `1`s, in
no particular order.
```
<S> ::= "0" <A> | "1" <B> | ""
<A> ::= "1" <S> | "0" <A> <A>
<B> ::= "0" <S> | "1" <B> <B>
```

We can convert this into Scheme.  Note that we annotate the list
passed to `return` with an annotation keeping track of which rule was
applied.

```scheme
(define eq-01
  (<:> (doM* (char #\0)
             (xs <- 01-a)
             (return `(s1 0 ,xs)))
       (doM* (char #\1)
             (xs <- 01-b)
             (return `(s2 1 ,xs)))
       (return `s3)))
(define 01-a
  (<:> (doM* (char #\1)
             (x <- eq-01)
             (return `(a1 ,1 ,x)))
       (doM* (char #\0)
             (x <- 01-a)
             (y <- 01-a)
             (return `(a2 (0 ,x ,y))))))
(define 01-b
  (<:> (doM* (char #\0)
             (x <- eq-01)
             (return `(b1 1 ,x)))
       (doM* (char #\1)
             (x <- 01-b)
             (y <- 01-b)
             (return `(b2 1 ,x ,y)))))
```

The output can be hard to read.  Fortunately, Guile provides a
`pretty-print` REPL directive.

```scheme
;; scheme@(guile-user)> ,pretty-print (parse 01-s "110000101001011011")
(s2 1
    (b2 1
        (b1 1 s3)
        (b1 1
            (s1 0
                (a2 (0
                     (a1 1
                         (s1 0
                             (a1 1
                                 (s1 0
                                     (a2 (0
                                          (a1 1 (s1 0 (a1 1 (s2 1 (b1 1 s3)))))
                                          (a1 1 s3)))))))
                     (a1 1 s3)))))))
```

Exercise: come up with an alternate context-free grammar describing
this language of equal occurrences of `0`s and `1`s.

## Papers and Resources
Most of these functions come from papers on monads and monadic
parsing:
- [Functional Pearl: _Monadic Parsing in Haskell_](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
- [Monads for Functional Programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
- Chapter 8 of the book _Programming in Haskell_ by Graham Hutton.
- [Oleg Kiselyov's blog post on monads in Scheme](http://okmij.org/ftp/Scheme/monad-in-Scheme.html)

Most of the functions were re-written in Scheme, so functions like
`>>=` (bind) and `<:>` (choice) which are normally infixed in Haskell
must be applied prefix-style in Scheme.

There were differences in the naming of functions in these resources
(e.g. `return` `(a -> M a)` is called `unit` in Wadler's paper).  I
use my own conventions here, yours may differ.

## To be implemented
- Support ambiguous grammars
  - Done in Haskell with list comprehensions and the List monad, which
    means we may need to implement monad transformers.
- XML parser.
- Parsing BNF into Scheme/Haskell parsers!
- Separate code to take advantage of the Scheme module system.

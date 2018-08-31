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

May be converted to the following Scheme equivalent (using the `letM*`
macro).

```scheme
(define my_parser
  (letM* ((a item)
          (_ item)
          (b item))
    (return (cons a b))))
```

The `_` binding isn't actually ignored, if we had put `(return _)`
then we would have returned the character consumed on the second
`item`.  However, one should use the convention of "assigning" to `_`
as saying the parsed thing in question will not be used further.

We can then use our parser, without needing to worry about all the
monadic magic happening underneath.

```scheme
(my-parser "hello") ;; => ((#\h . #\l) . "lo") ;; "lo" was not consumed.
(my-parser "ah") ;; => ()
```

You'll know that your parser failed when it returns the empty list.
Dive into the well-commented `parsing.scm` file to see operators like
`+++`, `many`, `oneof`, `token` and more, with examples included!

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
  

Demo of the BNF parser below.
```scheme
(bnf-syntax "<integer> ::= <digit>|<integer><digit>")
;; => ((bnf-rule integer (or (digit) (integer digit))) . "")
```
  
## Papers and Resources
Most of these functions come from papers on monads and monadic
parsing:
- [Functional Pearl _Monadic Parsing in Haskell_](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
- [Monads for Functional Programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
- Chapter 8 of the book /Programming in Haskell/ by Graham Hutton.
- [The blog post by Oleg that inspired it all](http://okmij.org/ftp/Scheme/monad-in-Scheme.html)

Most of the functions were just re-written in Scheme, so operations
like `>>=` and `+++` which are infixed in Haskell have been converted
to prefix notation.

There were differences in the naming of functions in these resources
(e.g. `return` `(a -> M a)` is called `unit` in Wadler's paper).  I
use my own conventions here, yours may differ.

## To be implemented
- Support ambiguous grammars
  - Done in Haskell with list comprehensions and the List monad, which
    means we may need to implement monad transformers.
- XML parser.
- Parsing BNF into Scheme/Haskell parsers!

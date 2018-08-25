# monadic-parsing
Monadic parsing in Scheme.

Admittedly it should be written in Haskell, but this was an experiment
in writing it in Scheme.  Things could be cleaned up with better
macros.

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
(define my-parser
  (letM* ((a item)
          (_ item)
          (b item))
    (return (cons a b))))
```

The `_` binding isn't actually ignored, if I had put `(return _)` then
it would have returned the character consumed on `item`.  However, one
should use the convention of "assigning" to `_` as saying the parsed
thing in question will not be used further.

## Included Parsers
- `expr`: Demonstration of parsing arithmetic expressions into s-exps
  _preserving_ operator precedence.  (i.e. `*` has higher precedence
  than `+`).
  - e.g. `2+(3*4)`, `2+3+10*8` etc.
- `read-num-list`: Read lists of numbers such as `[3,1,4,1,5]` and
  returns them as Scheme lists `(3 1 4 1 5)`.
  - Fails nicely (i.e. returns `()` on malformed input like `[1,2,]` etc.)
  
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

## Improvements
- Support ambiguous grammars

## Extensions
- XML parser.
- A parser for parsing regexps into Scheme/Haskell parsers (!)
- Converting BNF into parsers.

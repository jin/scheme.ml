# Scheme interpreter in OCaml

See `examples` folder for implemented Scheme syntax.

### How it works

```scheme
scheme> (|| #t #f)
DEBUG: Tokens: [LParen, ||, #t, #f, RParen]
DEBUG: S-Expression: List(Atom(||) Atom(#t) Atom(#f))
DEBUG: Result: #t
#t
scheme> (if (< 1 2) 4 5)
DEBUG: Tokens: [LParen, if, LParen, <, 1, 2, RParen, 4, 5, RParen]
DEBUG: S-Expression: List(Atom(if) List(Atom(<) Atom(1) Atom(2)) Atom(4) Atom(5))
DEBUG: Result: 4
4

scheme> (cons 1 '((* 1 2) (if (< 1 2) 3 4)))
DEBUG: Tokens: [LParen, Keyword(cons), Number(1), Quote, LParen, LParen, Multiply, Number(1), Number(2), RParen, LParen, Keyword(if), LParen, <, Number(1), Number(2), RParen, Number(3), Number(4), RParen, RParen, RParen]
DEBUG: S-Expression: List(Atom(cons) Atom(1) List(Atom(') List(Atom(Multiply) Atom(1) Atom(2)) List(Atom(if) List(Atom(<) Atom(1) Atom(2)) Atom(3) Atom(4))))
DEBUG: Result: (1, 2, 3)
28> (1, 2, 3)
```

### Building and running

```sh
opam install sedlex

make

# this starts the interpreter
bin/lexer 

# or feed Scheme code into STDIN
bin/lexer < examples/arithmetic.scm
```

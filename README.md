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

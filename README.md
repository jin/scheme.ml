# Scheme interpreter in OCaml

See `test.ml` for the set of implemented Scheme syntax.

### How it works

```scheme
scheme> (&& #f #t)
DEBUG: Tokens: [LParen, &&, #f, #t, RParen]
DEBUG: S-Expression: List(Atom(&&) Atom(#f) Atom(#t))
DEBUG: Result: #f
#f

scheme> (if (< 1 2) 4 5)
DEBUG: Tokens: [LParen, Keyword(if), LParen, <, Number(1), Number(2), RParen, Number(4), Number(5), RParen]
DEBUG: S-Expression: List(Atom(if) List(Atom(<) Atom(1) Atom(2)) Atom(4) Atom(5))
DEBUG: Result: 4
4

scheme> (car '(a b c))
DEBUG: Tokens: [LParen, Keyword(car), Quote, LParen, Variable(a), Variable(b), Variable(c), RParen, RParen]
DEBUG: S-Expression: List(Atom(car) List(Atom(') Atom(a) Atom(b) Atom(c)))
DEBUG: Result: a
a

scheme> (cdr (cons 1 '((* 1 2) (if (< 1 2) 3 4))))
DEBUG: Tokens: [LParen, Keyword(cdr), LParen, Keyword(cons), Number(1), Quote, LParen, LParen, Multiply, Number(1), Number(2), RParen, LParen, Keyword(if), LParen, <, Number(1), Number(2), RParen, Number(3), Number(4), RParen, RParen, RParen, RParen]
DEBUG: S-Expression: List(Atom(cdr) List(Atom(cons) Atom(1) List(Atom(') List(Atom(Multiply) Atom(1) Atom(2)) List(Atom(if) List(Atom(<) Atom(1) Atom(2)) Atom(3) Atom(4)))))
DEBUG: Result: (2, 3)
(2, 3)
```

### Building and running

```sh
# install dependencies
opam install sedlex alcotest

# Build with either Make or [Bazel](https://bazel.io)

# MAKE
make

# run all examples and diffs output against respective expected output in examples/test
make test

# ----

# Bazel

bazel build //:scheme-bytecode # compiles the binary into bazel-bin/scheme-bytecode

# ----

# start the interpreter
./scheme

# or load a source file
./scheme examples/arithmetic.scm
```

### What's next 

In the order of features I want to look into.

- [ ] More separation of eval logic from lexer/parser
- [x] Binary should accept filenames and not read from STDIN
- [ ] `cond`
- [ ] Lambdas

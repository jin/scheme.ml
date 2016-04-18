# Scheme interpreter in OCaml

See `examples` folder for implemented Scheme syntax.

### Building and running

```sh
opam install sedlex

make

# this starts the interpreter
bin/lexer 

# or feed Scheme code into STDIN
bin/lexer < examples/arithmetic.scm
```

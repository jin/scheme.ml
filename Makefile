SRCDIR=src/
BUILDDIR=bin/

lex:
	mkdir -p bin/
	ocamlfind ocamlc -c -package sedlex -package gen $(SRCDIR)lexer.ml
	ocamlfind ocamlc -o $(BUILDDIR)lexer -linkpkg -package sedlex -package gen $(SRCDIR)lexer.cmo

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj

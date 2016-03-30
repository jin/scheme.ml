lex:
	ocamlfind ocamlc -c -package sedlex -package gen lexer.ml
	ocamlfind ocamlc -o lexer -linkpkg -package sedlex -package gen lexer.cmo

clean:
	rm -f *~ *.cm* *.a *.lib *.o *.obj

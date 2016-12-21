PROGRAM=scheme
BUILDDIR=bin

all:
	ocamlbuild -j 0 -r -pkgs sedlex -use-ocamlfind scheme.byte
	mv scheme.byte scheme

test: 
	ocamlbuild -j 0 -r -pkgs alcotest,sedlex -use-ocamlfind test.native
	./test.native

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj

.PHONY: all clean test

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir $(MENHIRFLAGS)"

MAIN            := hazelnut

all:
	$(OCAMLBUILD) $(MAIN).native
	$(OCAMLBUILD) $(MAIN).d.byte

debug:
	@rlwrap ocamldebug $(MAIN).d.byte

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

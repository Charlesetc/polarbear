.PHONY: build clean test

MENHIRFLAGS     := --infer -v

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir $(MENHIRFLAGS)"

MAIN            := polarbear_main

test: build
	@./tools.bash run_all

build:
	@$(OCAMLBUILD) $(MAIN).native

build-debug:
	@$(OCAMLBUILD) $(MAIN).d.byte

debug: build-debug
	@rlwrap ocamldebug $(MAIN).d.byte

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

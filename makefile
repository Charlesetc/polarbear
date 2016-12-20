.PHONY: build clean test

MENHIRFLAGS     := --infer -v

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir $(MENHIRFLAGS)"

test: build
	@./tools.bash run_all

build:
	@rm -f *.native
	@$(OCAMLBUILD) snow_main.native
	@$(OCAMLBUILD) snow_frontend.native

# build-debug:
# 	@$(OCAMLBUILD) $(MAIN).d.byte

# debug: build-debug
# 	@rlwrap ocamldebug $(MAIN).d.byte

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

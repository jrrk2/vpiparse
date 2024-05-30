TRACE = --explain --trace

############################################################################

Input_top: Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm -linkpkg Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input.cmo: Input.ml Input.mli
	ocamlc.opt -g -c Input.mli Input.ml

Input.cmx: Input.ml Input.mli
	ocamlopt.opt -g -c Input.mli Input.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	menhir $(TRACE) --infer $<

############################################################################

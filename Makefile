TRACE = --explain # --trace

############################################################################

Input: Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlc -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_opt: Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlopt -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_top: Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	menhir $(TRACE) --infer $<

############################################################################

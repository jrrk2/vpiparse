TRACE = --explain ## --trace

############################################################################

Input_remapp: Input.ml Input_types.mli ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_rewrite.ml Input_remapp.ml Input_detect.ml Input_pat.ml Input_pat2.ml Input_dump.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input_types.mli Input.ml ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_rewrite.ml Input_remapp.ml Input_detect.ml Input_pat.ml Input_pat2.ml Input_dump.ml

Input_top: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_remapp.ml Input_main.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_remapp.ml Input_main.ml

Input: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlc -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_opt: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlopt -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	menhir $(TRACE) --infer $<

############################################################################

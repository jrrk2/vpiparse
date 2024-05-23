TRACE = --explain # --trace

############################################################################

Input_top: Input.cmo Input_types.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml vpi_types.ml 
	ocamlmktop -g -o $@ Input_types.cmo  vpi_types.ml Input.cmo ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input.cmo: Input_types.ml Input.ml Input.mli
	ocamlc.opt -g -c Input.mli Input_types.ml vpi_types.ml Input.ml

Input.cmx: Input_types.ml Input.ml Input.mli
	ocamlopt.opt -g -c Input.mli Input_types.ml vpi_types.ml Input.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	menhir $(TRACE) --infer $<

############################################################################

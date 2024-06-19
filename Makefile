TRACE = --explain ## --trace

############################################################################

Input_dump_top: dump_types.mli Input.ml Input_types.mli ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_pat4.ml Input_dump.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml  Input_rewrite.ml Input_remapp.ml Input_pat.ml Input_pat2.ml Input_pat3.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg dump_types.mli Input.mli Input_types.mli Input.ml ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_dump.ml Input_pat4.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml Input_rewrite.ml Input_remapp.ml  Input_pat.ml Input_pat2.ml Input_pat3.ml

Input_dump: dump_types.mli Input.ml Input_types.mli ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_rewrite.ml Input_remapp.ml Input_pat.ml Input_pat2.ml Input_pat3.ml Input_pat4.ml Input_dump.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml
	ocamlfind ocamlopt -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg dump_types.mli Input.mli Input_types.mli Input.ml ord_input.ml Input_lex.ml Input_scan.ml Input_pp.ml Input_pat.ml Input_pat2.ml Input_pat3.ml Input_dump.ml Input_pat4.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml Input_rewrite.ml Input_remapp.ml

Input_top: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_remapp.ml Input_main.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_remapp.ml Input_main.ml

Input: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlc -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_opt: Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml
	ocamlfind ocamlopt -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg Input.mli Input.ml ord_input.ml Input_lex.ml Input_rewrite.ml Input_main.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	ocamlc -g -c dump_types.mli
	menhir $(TRACE) --infer $<

############################################################################

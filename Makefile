TRACE = --explain ## --trace

############################################################################

Input_equiv_top: dump_types.mli Input.ml Input_types.mli ord_input.ml Input_lex.ml Input_pat4.ml rtl_parser.mli rtl_parser.ml rtl_lexer.ml rtl_dump.ml Input_hardcaml.ml Input_dump.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml  Input_rewrite.ml Input_remapp.ml Input_pat.ml Input_pat2.ml Input_pat3.ml
	ocamlfind ocamlmktop -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg dump_types.mli Input.mli Input_types.mli rtl_parser.mli rtl_parser.ml rtl_lexer.ml Input_dump.ml Input_pat4.ml rtl_dump.ml Input.ml ord_input.ml Input_lex.ml Input_hardcaml.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml Input_rewrite.ml Input_remapp.ml  Input_pat.ml Input_pat2.ml Input_pat3.ml Input_scan.ml Input_pp.ml

Input_equiv: dump_types.mli Input.ml Input_types.mli ord_input.ml Input_lex.ml Input_pat4.ml rtl_parser.mli rtl_parser.ml rtl_lexer.ml rtl_dump.ml Input_hardcaml.ml Input_dump.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml  Input_rewrite.ml Input_remapp.ml Input_pat.ml Input_pat2.ml Input_pat3.ml
	ocamlfind ocamlopt -g -o $@  -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg dump_types.mli Input.mli Input_types.mli rtl_parser.mli rtl_parser.ml rtl_lexer.ml Input_dump.ml Input_pat4.ml rtl_dump.ml Input.ml ord_input.ml Input_lex.ml Input_hardcaml.ml Input_equiv.ml # apb_uart_top.ml sample.ml uitms.ml tmp.ml Input_rewrite.ml Input_remapp.ml  Input_pat.ml Input_pat2.ml Input_pat3.ml Input_scan.ml Input_pp.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly Makefile
	ocamlfind ocamlc -package base,hardcaml -g -c dump_types.mli
	menhir $(TRACE) --infer $<

rtl_lexer.ml: rtl_lexer.mll
	ocamllex $<

rtl_parser.ml: rtl_parser.mly Makefile
	menhir $(TRACE) --infer $<

############################################################################

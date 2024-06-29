TRACE = --explain ## --trace
TRACET = --explain --trace

############################################################################

LOBJ= dump_types.mli Input.mli Input_types.mli rtl_parser.mli rtl_parser.ml rtl_lexer.ml Input_dump.ml Input_pat4.ml File.mli File.ml File_lex.ml File_rewrite.ml Formula.mli Formula_types.ml Formula.ml Formula_lex.ml Formula_rewrite.ml rtl_dump.ml Input_hardcaml.ml rtl_map.ml Input.ml ord_input.ml Input_lex.ml Input_equiv.ml

Input_equiv_top: $(LOBJ)
	ocamlfind ocamlmktop -g -o $@ -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg $(LOBJ)

Input_equiv: $(LOBJ)
	ocamlfind ocamlopt -g -o $@ -package base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg $(LOBJ)

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly Makefile
	ocamlfind ocamlc -package base,hardcaml -g -c dump_types.mli
	menhir $(TRACE) --infer $<

rtl_lexer.ml: rtl_lexer.mll
	ocamllex $<

rtl_parser.ml rtl_parser.mli: rtl_parser.mly Makefile
	menhir $(TRACE) --infer $<

############################################################################

.PHONY: everything
PARSER=ocamlyacc
MENHIRFLAGS= --infer # --trace
MENHIRFLAGST= --infer --trace
PARSER=menhir $(MENHIRFLAGS)

File_lex.ml: File_lex.mll
	ocamllex $<

File.ml File.mli: File.mly
	menhir $(MENHIRFLAGS) $<

Formula_lex.ml: Formula_lex.mll
	ocamllex $<

Formula.ml Formula.mli: Formula.mly
	menhir $(MENHIRFLAGS) $<

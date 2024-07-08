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


############################################################################

VOBJ=Source_text_verible_types.cmo Source_text_verible.mli outputparser/verible_pat.ml outputparser/Source_text_verible_rewrite_types.mli Source_text_verible.ml Source_text_verible_tokens.ml Source_text_verible_lex.ml outputparser/Source_text_verible_rewrite.ml dump_types.mli Input.mli Input_types.mli Input_dump.ml rtl_parser.mli Input_hardcaml.ml

Source_verible_top: $(VOBJ)
	ocamlfind ocamlmktop -package msat,hardcaml,hardcaml_circuits,unix -linkpkg -g -o $@ -I +unix -I outputparser $(VOBJ)

Source_text_verible.mly Source_text_verible_tokens.ml: outputparser/verible.output outputparser/output_parser Makefile
	env OCAMLRUNPARAM=b STRING_LITERAL=string TK_StringLiteral=string SymbolIdentifier=string SystemTFIdentifier=string TK_DecNumber=string TK_BinBase=string TK_BinDigits=string TK_DecBase=string TK_DecDigits=string TK_OctBase=string TK_OctDigits=string TK_HexBase=string TK_HexDigits=string TK_UnBasedNumber=string TK_RealTime=string STRING=string outputparser/output_parser $<

Source_text_verible_types.cmo Source_text_verible.ml outputparser/Source_text_verible_types.ml: Source_text_verible.mly
	ocamlc -c outputparser/Source_text_verible_types.ml -o Source_text_verible_types.cmo
	menhir $(MENHIRFLAGS) $<

Source_text_verible_lex.ml: outputparser/Source_text_verible_lex.mll
	ocamllex $< -o $@

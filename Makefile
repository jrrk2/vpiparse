TRACE = --explain ## --trace
TRACET = --explain --trace

############################################################################

XOBJ= dump_types.mli Input.mli Input_types.mli rtl_parser.mli rtl_parser.ml rtl_lexer.ml Input_dump.ml Input_cnv.ml Input_verilator.ml File.mli File.ml File_lex.ml File_rewrite.ml Formula.mli Formula_types.ml Formula.ml Formula_lex.ml Formula_rewrite.ml rtl_dump.ml Input_hardcaml.ml rtl_map.ml ord_input.ml Input_equiv_verilator.ml

Input_verilator_top: $(XOBJ)
	ocamlfind ocamlmktop -g -o $@ -package xml-light,base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg $(XOBJ)

Input_verilator: $(XOBJ)
	ocamlfind ocamlopt -g -o $@ -package xml-light,base,hardcaml,hardcaml_waveterm,hardcaml_circuits -linkpkg $(XOBJ)

############################################################################

LOBJ= dump_types.mli Input.mli Input_types.mli rtl_parser.mli rtl_parser.ml rtl_lexer.ml Input_dump.ml Input_cnv.ml Input_pat4.ml File.mli File.ml File_lex.ml File_rewrite.ml Formula.mli Formula_types.ml Formula.ml Formula_lex.ml Formula_rewrite.ml rtl_dump.ml Input_hardcaml.ml rtl_map.ml Input.ml ord_input.ml Input_lex.ml Input_equiv.ml

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
MENHIRFLAGS=--ocamlc 'ocamlc -I outputparser' --infer # --trace
MENHIRFLAGST= $(MENHIRFLAGS) --trace
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

GEN = outputparser/String_lit.mli outputparser/String_lit.ml outputparser/Msat_sat_slit.mli outputparser/Msat_sat_slit.ml outputparser/Msat_tseitin.mli outputparser/Msat_tseitin.ml Rtlil_input.mli outputparser/Rtlil_input_rewrite_types.mli dump_types.mli Input.mli outputparser/generic_rewrite.ml outputparser/convert_edited.ml
GEN2 =  Rtlil_input.ml  Rtlil_input_tokens.ml  ord_input.ml Rtlil_input_lex.ml outputparser/Rtlil_dump.ml outputparser/Rtlil_input_rewrite.ml
Source_generic_top: outputparser/Rtlil_input_types.ml $(GEN) $(GEN2) outputparser/Source_generic_main.ml
	ocamlfind ocamlmktop -package msat,hardcaml,hardcaml_circuits -linkpkg -g -o $@ -I +unix unix.cma -I outputparser outputparser/Rtlil_input_types.ml  $(GEN) $(GEN2) outputparser/Source_generic_main.ml

VSRC=outputparser/Source_text_verible_types.mli outputparser/Rtlil_input_types.mli outputparser/Source_text_verible_types.ml outputparser/Rtlil_input_types.ml Source_text_verible.mli outputparser/Source_text_verible_rewrite_types.mli Source_text_verible.ml Source_text_verible_tokens.ml Source_text_verible_lex.ml outputparser/Source_text_verible_rewrite.ml dump_types.mli Input.mli rtl_parser.mli File.mli File.ml File_lex.ml File_rewrite.ml Input_types.mli Input_dump.ml Input_cnv.ml rtl_parser.ml rtl_lexer.ml Formula.mli Formula_types.ml Formula.ml Formula_lex.ml Formula_rewrite.ml rtl_dump.ml Input_hardcaml.ml rtl_map.ml Input.ml ord_input.ml Input_lex.ml outputparser/verible_pat.ml outputparser/Rtlil_input_rewrite_types.mli Rtlil_input.mli outputparser/Rtlil_dump.ml Rtlil_input.ml Rtlil_input_tokens.ml Rtlil_input_lex.ml outputparser/Rtlil_input_rewrite.ml outputparser/dump_rtlil.ml cnv_ilang.ml
VOBJ=$(VSRC) $(GEN) outputparser/Source_generic_main.ml Input_equiv_verible.ml myluaclient.ml

Source_verible_top: $(VOBJ)
	ocamlfind ocamlmktop -package msat,hardcaml,hardcaml_circuits,unix,lua-ml -linkpkg -g -o $@ -I +unix -I outputparser $(VOBJ)

Source_verible: $(VOBJ)
	ocamlfind ocamlopt -package msat,hardcaml,hardcaml_circuits,unix,lua-ml -linkpkg -g -o $@ -I +unix -I outputparser $(VOBJ)

Source_text_verible.mly Source_text_verible_tokens.ml: outputparser/verible.output outputparser/output_parser Makefile
	env OCAMLRUNPARAM=b STRING_LITERAL=string TK_StringLiteral=string SymbolIdentifier=string SystemTFIdentifier=string TK_DecNumber=string TK_BinBase=string TK_BinDigits=string TK_DecBase=string TK_DecDigits=string TK_OctBase=string TK_OctDigits=string TK_HexBase=string TK_HexDigits=string TK_UnBasedNumber=string TK_RealTime=string STRING=string outputparser/output_parser $<

Source_text_verible.ml: Source_text_verible.mly outputparser/Source_text_verible_types.cmi
	menhir $(MENHIRFLAGS) $<

outputparser/Source_text_verible_types.mli: outputparser/Source_text_verible_types.ml
	ocamlc -i $< > $@

outputparser/Source_text_verible_types.cmi: outputparser/Source_text_verible_types.mli
	ocamlc -c $< -o $@

Source_text_verible_lex.ml: outputparser/Source_text_verible_lex.mll
	ocamllex $< -o $@

Rtlil_input_lex.ml: outputparser/Rtlil_input_lex.mll
	ocamllex $< -o $@

Rtlil_input.ml Rtlil_input.mli: Rtlil_input.mly outputparser/Rtlil_input_types.cmi
	menhir $(MENHIRFLAGS) $<

outputparser/Rtlil_input_types.cmi: outputparser/Rtlil_input_types.mli
	ocamlc -c $< -o $@

outputparser/Rtlil_input_types.mli: outputparser/Rtlil_input_types.ml
	ocamlc -i $< > $@

Rtlil_input.mly Rtlil_input_tokens.ml: outputparser/rtlil_parser.output
	env OCAMLRUNPARAM=b TOK_STRING=string TOK_ID=string TOK_INT=int TOK_VALUE=string outputparser/output_parser $<

###########################################################################

YSRC=Input_yosys.ml

Source_yosys_top: $(YSRC)
	ocamlfind ocamlmktop -package msat,hardcaml,hardcaml_circuits,unix,ppx_let,core_unix.filename_unix,jsonaf,ppx_jsonaf_conv -thread -linkpkg -g -o $@ -I +unix -I hardcaml_of_verilog/_build/install/default/lib/hardcaml_of_verilog hardcaml_of_verilog.cma $(YSRC)

Source_yosys: $(YSRC)
	ocamlfind ocamlopt -package msat,hardcaml,hardcaml_circuits,unix,ppx_let -linkpkg -g -o $@ -I +unix -I hardcaml_of_verilog/_build/install/default/lib/hardcaml_of_verilog hardcaml_of_verilog.cma $(YSRC)

###########################################################################

EOBJ= Input_verilator.ml Input_equiv_verilator.ml

IOBJ= Input_pat4.ml Input_equiv.ml

COBJ=$(VSRC) $(GEN) outputparser/Source_generic_main.ml Input_equiv_verible.ml $(EOBJ) $(IOBJ) myluaclient.ml

Source_combined_top: $(COBJ)
	ocamlfind ocamlmktop -package xml-light,msat,hardcaml,hardcaml_circuits,unix,lua-ml -linkpkg -g -o $@ -I +unix -I outputparser $(COBJ)

Source_combined: $(COBJ)
	ocamlfind ocamlopt -package xml-light,msat,hardcaml,hardcaml_circuits,unix,lua-ml -linkpkg -g -o $@ -I +unix -I outputparser $(COBJ)

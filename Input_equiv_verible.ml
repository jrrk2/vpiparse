(*
 MIT License

Copyright (c) 2024 Jonathan Kimmitt

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

let eqv stem =
  let script = stem^".eqy" in
  let fd = open_out script in
  output_string fd ("[options]\n");
  output_string fd ("\n");
  output_string fd ("[gold]\n");
  output_string fd ("read_ilang "^stem^"_gold.ilang\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[gate]\n");
  output_string fd ("read_ilang "^stem^"_rev.ilang\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[strategy simple]\n");
  output_string fd ("use sat\n");
  output_string fd ("depth 10\n");
  output_string fd ("\n");
  close_out fd;
  print_endline ("Status = "^string_of_int (Sys.command ("eqy -f "^script)))

let sta gate stem liberty =
  let env = "STA_"^stem in
  print_endline env;
  let script = stem^".tcl" in
  let fd = open_out script in
  output_string fd ("read_lib "^liberty^".lib\n");
  output_string fd ("read_verilog "^gate^"\n");
  output_string fd ("current_design "^stem^"\n");
  output_string fd ("link\n");
  output_string fd (try let cmds = String.concat "\n" (String.split_on_char ';' (Sys.getenv env))^"\n" in print_endline cmds; cmds with _ -> "report_checks -unconstrained\nexit\n");
  close_out fd;
  print_endline ("Status = "^string_of_int (Sys.command ("../OpenSTA/app/sta -no_splash -threads max -no_init "^script)))

  (*
   open Base__String
   *)
open Source_text_verible_rewrite_types
open Source_text_verible_lex
open Source_text_verible
open Source_text_verible_rewrite
open Verible_pat
open Input_dump

let othrawp = ref End_of_file
let othp = ref End_of_file
let othp' = ref End_of_file
let othp'' = ref Vempty
let othitms = ref (Input_dump.empty_itms [])

let tran v =
  let rawp = parse_output_ast_from_file v in
  othrawp := rawp;
  let p = Source_text_verible_rewrite.rw rawp in
  othp := p;
  let p' = Source_text_verible_rewrite.rw' p in
  othp' := p';
  let p'' = pat p' in
  othp'' := p'';
  print_endline "verible_pat_cnv";
  let gold = snd (Rtlil_input_rewrite.parse (Rtlil_input_rewrite.parse_output_ast_from_pipe v)) in
  List.iter (Rtlil_dump.dumprtl "_gold") gold;
  List.iter (fun (modnam, uitms) ->
  print_endline "hardcaml_cnv";
  let rtl = Input_hardcaml.cnv (modnam, uitms) in
  let yliberty, ycells = Rtl_map.read_lib "liberty/simcells" in
  let ilang = Cnv_ilang.cnv_ilang (Rtl_map.map ycells modnam rtl) in
  let _ = Rtlil_dump.dumprtl "_rev" ilang in
  let _ = Source_generic_main.rewrite_rtlil gold [ilang] in
 
  eqv modnam;
  let liberty, cells = Rtl_map.read_lib (Rtl_map.dflt_liberty None) in
  dump' "_map" (modnam, ((), (Rtl_map.map cells modnam rtl)));
  sta (modnam^"_map.v") modnam liberty;
  ) (Verible_pat.cnv' othitms p'')

let _ = if Array.length Sys.argv > 1 then ignore (tran Sys.argv.(1))
        else if (try int_of_string (Sys.getenv ("LIBERTY_DUMP")) > 0 with _ -> false) then
                (let liberty, cells = Rtl_map.read_lib (Rtl_map.dflt_liberty None) in
		  print_endline ("Dumping cells: "^string_of_int (List.length cells)); Rtl_map.dumpv cells liberty);

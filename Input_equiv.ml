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

let eqv gold gate stem liberty =
  let script = stem^".eqy" in
  let fd = open_out script in
  output_string fd ("[options]\n");
  output_string fd ("\n");
  output_string fd ("[gold]\n");
  output_string fd ("read -sv "^gold^"\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[gate]\n");
  output_string fd ("read_liberty -ignore_miss_func "^liberty^".lib\n");
  output_string fd ("read -sv "^gate^"\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[strategy simple]\n");
  output_string fd ("use sat\n");
  output_string fd ("depth 10\n");
  output_string fd ("\n");
  close_out fd;
  print_endline ("Status = "^string_of_int (Sys.command ("eqy -f "^script)))

open Input
open Input_types
open Input_pat4
open Input_dump
open Input_hardcaml

let p' = ref []

let tran f =
  let ch = if f = "-" then stdin else open_in f in
  let cache, p = Input_lex.parse_output_ast_from_chan ch in
  close_in ch;
  p' := p;
  let _ = List.map (top_pat (empty_itms [])) (List.filter (function TUPLE2 (Weaklyreferenced, _) -> false | _ -> true) p) in
  if true then List.iter (dump' "_all") !allmods;
  List.iter (dump' "_top") !topmods;
  let liberty = Rtl_map.read_lib (Rtl_map.dflt_liberty None) in
  if Array.length Sys.argv > 4 then (print_endline ("Dumping cells: "^string_of_int (List.length !(Rtl_map.cells'))); Rtl_map.dumpv Sys.argv.(4));
  List.iter (fun (modnam, (_, modul)) -> let rtl = cnv (modnam, modul) in Rtl_dump.dump modnam rtl; Rtl_map.map modnam rtl) !topmods;
  if Array.length Sys.argv > 2 then match !topmods with (modnam,_)::[] -> eqv Sys.argv.(2) (modnam^"_map.v") modnam liberty | _ -> failwith "multiple top modules"

let _ = if Array.length Sys.argv > 3 then eqv Sys.argv.(3) Sys.argv.(2) Sys.argv.(1) (Rtl_map.dflt_liberty None)
        else if Array.length Sys.argv > 1 then tran Sys.argv.(1)
        else (let liberty = Rtl_map.read_lib (Rtl_map.dflt_liberty None) in
		  print_endline ("Dumping cells: "^string_of_int (List.length !(Rtl_map.cells'))); Rtl_map.dumpv liberty);

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

open Source_text_verible_rewrite_types
open Source_text_verible_lex
open Source_text_verible
open Source_text_verible_rewrite
open Verible_pat
open Input_dump
open Verible_typ

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
  string_of_int (Sys.command ("eqy -f "^script))

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

type libcell = string *
          ((string * string) list *
           (string * Rtl_parser.token * File_rewrite.liberty) list)

type sat_netlist = (Generic_rewrite.E.signal * string) list *
         (Generic_rewrite.E.signal * Generic_rewrite.F.t) list *
         (string * int) list
	  
type luaitm =
| Cnvlst of string * Input_types.itms
| Rtlil of string * Rtlil_input_rewrite_types.ilang list
| Lib of string * libcell list
| Rtl of string * string
| Sat of sat_netlist list

let othrawp = ref End_of_file
let othp = ref End_of_file
let othp' = ref End_of_file
let othp'' = ref Vempty
let othitms = ref (Input_dump.empty_itms [])

let lhash = Hashtbl.create 257

let nxtitm' =
  let itm = ref 0 in fun () -> incr itm; "itm"^string_of_int !itm

let tranlst v =
  let rawp = parse_output_ast_from_file v in
  othrawp := rawp;
  let p = Source_text_verible_rewrite.rw rawp in
  othp := p;
  let p' = Source_text_verible_rewrite.rw' p in
  othp' := p';
  let p'' = pat p' in
  othp'' := p'';
  print_endline "verible_pat_cnv";
  Verible_pat.cnv' othitms p''

(*
let tranitm lib gold modnam uitms =
  print_endline "hardcaml_cnv";
  let rtl = Input_hardcaml.cnv (modnam, uitms) in
  let yliberty, ycells = Rtl_map.read_lib "liberty/simcells" in
  let ilang = Cnv_ilang.cnv_ilang modnam (Rtl_map.map ycells modnam rtl) in
  let _ = Rtlil_dump.dumprtl "_rev" ilang in
  let _ = Source_generic_main.rewrite_rtlil gold [ilang] in
  print_endline ("Status = "^eqv modnam);
  let liberty, cells = Rtl_map.read_lib lib in
  dump' "_map" (modnam, ((), (Rtl_map.map cells modnam rtl)));
  sta (modnam^"_map.v") modnam liberty
*)
  
let find_cnv itm = match Hashtbl.find lhash itm with
    | Cnvlst (nam, itms) -> nam, itms
    | oth -> failwith ("item "^itm^" is not a verilog uitm")

let find_rtl itm = match Hashtbl.find lhash itm with
    | Rtl (nam, itms) -> nam, itms
    | oth -> failwith ("item "^itm^" is not a verilog uitm")

let find_ilang itm = match Hashtbl.find lhash itm with
    | Rtlil (nam, itms) -> nam, itms
    | oth -> failwith ("item "^itm^" is not a RTLIL itm")

let find_sat itm = match Hashtbl.find lhash itm with
    | Sat itms -> itms
    | oth -> failwith ("item "^itm^" is not a SAT itm")

let find_lib lib = match Hashtbl.find lhash lib with
    | Lib (liberty, cells) -> liberty, cells
    | oth -> failwith ("item "^lib^" is not a library")

let lcnvitm lib rtlitm = 
  let modnam, rtl = find_rtl rtlitm in
  let yliberty, ycells = find_lib lib in
  let nam,ilang = Cnv_ilang.cnv_ilang modnam (Rtl_map.map ycells modnam rtl) in
  let nxtitm = nxtitm' () in
  Hashtbl.add lhash nxtitm (Rtlil (nam, ilang));
  nxtitm

let lmapitm lib rtlitm = 
  let modnam, rtl = find_rtl rtlitm in
  let liberty, cells = find_lib lib in
  let map = Rtl_map.map cells modnam rtl in
  let nxtitm = nxtitm' () in
  Hashtbl.add lhash nxtitm (Cnvlst (modnam, map));
  nxtitm

let lcnvsat itm =
  let ilang = find_ilang itm in
  let lst = Source_generic_main.cnv_sat_tree [ilang] in
  let nxtitm = nxtitm' () in
  Hashtbl.add lhash nxtitm (Sat lst);
  nxtitm
  
let lcmpitm gold rev = 
  let goldlst = find_sat gold in
  let revlst = find_sat rev in
  let status = Source_generic_main.cmp_sat goldlst revlst in
  if false then print_endline status;
  status

let gold_rtlil v =
  snd (Rtlil_input_rewrite.parse (Rtlil_input_rewrite.parse_output_ast_from_rtlil_pipe v))

let ldumplib lib =
  let liberty, cells = find_lib lib in
  print_endline ("Dumping cells: "^string_of_int (List.length cells));
  Rtl_map.dumpv cells liberty

let lhardcnv itm =
  let modnam, itms = find_cnv itm in
  let rtl = Input_hardcaml.cnv (modnam, itms) in
  let nxtitm = nxtitm' () in
  Hashtbl.add lhash nxtitm (Rtl (modnam, rtl));
  nxtitm

let ltranlst v =
  let cnvlst = tranlst v in
  let nxtitm = ref "" in
  List.iter (fun (nam,itm) -> nxtitm := nxtitm'(); Hashtbl.add lhash !nxtitm (Cnvlst (nam,itm))) cnvlst;
  !nxtitm

let lrtlil v =
  let gold = gold_rtlil v in 
  let nxtitm = ref "" in
  List.iter (fun (nam, itm) -> nxtitm := nxtitm'(); Hashtbl.add lhash !nxtitm (Rtlil (nam, itm))) gold;
  !nxtitm

let lreadlib lib =
  let liberty, cells = Rtl_map.read_lib lib in
  let nxtitm = nxtitm' () in
  Hashtbl.add lhash nxtitm (Lib (liberty, cells));
  nxtitm

let lnam itm =
  match Hashtbl.find lhash itm with
  | Cnvlst (nam, itms) -> nam
  | Rtlil (nam, itms) -> nam
  | Lib (nam, itms) -> nam
  | Rtl (nam, itms) -> nam
  | Sat itms -> itm

let ldump stem itm =
  match Hashtbl.find lhash itm with
  | Cnvlst (nam, itms) -> dump' stem (nam, ((), (itms)))
  | Rtlil (nam, itms) -> Rtlil_dump.dumprtl stem (nam, itms)
  | Lib (nam, itms) -> failwith ("Library "^nam^" cannot be dumped with this command")
  | Rtl (nam, itms) -> failwith ("RTL "^nam^" cannot be dumped with this command")
  | Sat itms -> failwith ("Sat item "^itm^" cannot be dumped with this command")

let litms () =
  let itmlst = ref [] in
  Hashtbl.iter (fun k -> function
  | Cnvlst (nam, itms) -> itmlst := (k^"\t"^nam^"\tverilog item") :: !itmlst
  | Rtlil (nam, itms) -> itmlst := (k^"\t"^nam^"\trtlil item") :: !itmlst
  | Lib (nam, itms) -> itmlst := (k^"\t"^nam^"\tlibrary item") :: !itmlst
  | Rtl (nam, itms) -> itmlst := (k^"\t"^nam^"\tcompiled item") :: !itmlst
  | Sat itms -> itmlst := (k^"\t\tSAT netlist item") :: !itmlst
) lhash;
  String.concat "\n" (List.sort compare !itmlst)

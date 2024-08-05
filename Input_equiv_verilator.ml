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

open Input
open Input_types
open Input_cnv
open Input_dump
open Input_hardcaml

let othmod = ref ("", empty_itms [])
let othxml = ref (Xml.PCData "")

let read' lst x = try
  let entry = Unix.readdir x in
  print_endline entry;
  lst := entry :: !lst; true with End_of_file -> Unix.closedir x; false

let status f =
  let stat = Unix.stat f in
  match stat.st_kind with
  | S_REG -> f
  | S_DIR -> let x=Unix.opendir f in
    let lst = ref [] and busy = ref true in
    while !busy do busy := read' lst x done; f^"/"^(List.hd !lst)
  | oth -> failwith "filt type not handled"

let tran f' src =
  let f = status f' in
  print_endline ("tran "^f);
  let (line,range,rwxml,xml,mods,toplst,topattr,modules,packages,interfaces) = Input_verilator.translate () f in
  othxml := xml;
  if true then Hashtbl.iter (fun k x -> othmod := x; dump' "_check" (k,x)) modules;
  let liberty, cells = Rtl_map.read_lib (Rtl_map.dflt_liberty None) in
  Hashtbl.iter (fun modnam (_, modul) -> let rtl = cnv (modnam, modul) in Rtl_dump.dump modnam rtl;
  dump' "_map" (modnam, ((), (Rtl_map.map cells modnam rtl)))) modules;
  if Hashtbl.length modules = 1 then Hashtbl.iter (fun modnam (_, modul) -> 
    eqv src (modnam^"_map.v") modnam liberty; sta (modnam^"_map.v") modnam liberty) modules

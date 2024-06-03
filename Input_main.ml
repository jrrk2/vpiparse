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

let eqv gold stem =
  let name = (gold^".v") in
  let script = stem^".eqy" in
  let fd = open_out script in
  output_string fd ("[options]\n");
  output_string fd ("\n");
  output_string fd ("[gold]\n");
  output_string fd ("read -sv "^gold^"\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[gate]\n");
  output_string fd ("read -sv "^stem^".v\n");
  output_string fd ("prep -top "^stem^"\n");
  output_string fd ("\n");
  output_string fd ("[strategy simple]\n");
  output_string fd ("use sat\n");
  output_string fd ("depth 10\n");
  output_string fd ("\n");
  close_out fd;
  print_endline ("Status = "^string_of_int (Sys.command ("eqy -f "^script)))

open Input
open Input_rewrite
open Hardcaml
open Always
open Signal

open Input
open Input_rewrite
open Input_lex

type remapp =
  | Void
  | Id of string
  | Alw of Hardcaml.Always.t
  | Bin of string * int
  | Oct of string * int
  | Dec of string * int
  | Hex of string * int
  | Eq of remapp * remapp
  | If_ of remapp * remapp list * remapp list
  | Mux2 of remapp * remapp * remapp
  | Asgn of remapp * remapp
  | Block of remapp * remapp
  | Concat of remapp * remapp
  | Select of remapp * string * string
  | Bitsel of remapp * remapp
  | Seq of remapp list
  | Unary of token * remapp
  | Dyadic of token * remapp * remapp

type remap =
  | Invalid
  | Con of Hardcaml.Constant.t
  | Sig of Hardcaml.Signal.t
  | Alw of Hardcaml.Always.t
  | Var of Hardcaml.Always.Variable.t

type attr = {
  pass: bool;
  clock: string option;
}

let othr = ref Invalid
let othlhs = ref Void
let othrhs = ref Void
let othrmlhs = ref Invalid
let othrmrhs = ref Invalid
let othlhs' = ref Void
let othrhs' = ref Void
let othrmlhs' = ref Invalid
let othrmrhs' = ref Invalid
let othp = ref Void
let otht = ref None
let othremapp' = ref []
let othremapp'' = ref []
let othremapp''' = ref []
let othdecl = ref []
let alst = ref []
let othedg = ref Work
let p' = ref []

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n/2)

let exact_log2 n = 1 lsl (log2 n) = n

let add_fast a_sig b_sig =
          (Hardcaml_circuits.Prefix_sum.create
             ~config:(if exact_log2 (width a_sig) && exact_log2 (width b_sig) then Kogge_stone else Sklansky)
             (module Signal)
             ~input1:(a_sig)
             ~input2:(b_sig)
             ~carry_in:(Signal.zero 1))

let mult_wallace a_sig b_sig =
          (Hardcaml_circuits.Mul.create
             ~config:Wallace
             (module Signal)
             (a_sig)
             (b_sig))

(*
let div =
      Hardcaml_circuits.Divider.Div.create
        (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
	*)

let sim_mult n_bits =
  let circuit =
    Circuit.create_exn
      ~name:"lfsr"
      [ output "o" (mult_wallace (input "a" n_bits) (input "b" n_bits))]
  in
  let sim = Cyclesim.create circuit in
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  fun () ->
    for i = 0 to (1 lsl n_bits) - 1 do
      for j = 0 to (1 lsl n_bits) - 1 do
        a := Bits.of_int ~width:n_bits i;
        b := Bits.of_int ~width:n_bits j;
        Cyclesim.cycle sim
      done
    done

let cnv (modnam, p) =
begin
let declare_lst = ref [] in
let exists k = List.mem_assoc k !declare_lst in
let add_decl k x =
  if exists k then print_endline (k^": redeclared") else
  begin
    declare_lst := (k, x) :: !declare_lst;
    othdecl := (k, match x with
      | Con _ -> x
      | Var _ -> Var (Always.Variable.wire ~default:(Signal.zero 1))
      | Sig _ -> x) :: !othdecl
  end in
let find_decl k = List.assoc k !declare_lst in
let iter_decl fn = List.iter (fun (k,x) -> fn k x) !declare_lst in

let sig' = function
| Con x -> Signal.of_constant x
| Sig x -> x
| Var x -> x.value
| oth -> othr := oth; failwith "sig'" in

let alw' = function
| Alw x -> x
| oth -> othr := oth; failwith "alw'" in

let var' = function
| Var x -> x
| oth -> othr := oth; failwith "var'" in

let declare_input wid port =
  if false then print_endline port;
  add_decl port (Sig (Signal.input port wid)) in

let declare_wire = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING wire) ->
  if false then print_endline wire;
  let wid = pwid (STRING wire) loc in
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid)))
| oth -> othrw := Some oth; failwith "declare_wire" in

let declare_reg attr = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING reg) -> if exists reg then () else
  begin
  if false then print_endline reg;
  let wid = pwid (STRING reg) loc in
  let clock = match attr.clock with Some clk -> sig' (find_decl clk) | None -> failwith ("failed to indentify clock for register: "^reg) in
(*
  let clear = sig' (find_decl "clear") in
*) 
  let r_sync = Reg_spec.create ~clock () in
  add_decl reg (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid));
  end
| oth -> othrw := Some oth; failwith "declare_reg" in

let rec traverse (attr:attr) = function
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> List.iter (function
    | TUPLE4 ((Assignment|Vpiblocking), optype, lhs, rhs) ->
        traverse attr lhs;
        traverse attr rhs;
        if not attr.pass then declare_wire lhs
    | oth -> otht := Some oth; failwith "traverse failed with otht") stmtlst
| TUPLE3 (Always, Vpiassignstmt,
   TUPLE4 (Assignment, optype, lhs, rhs)) ->
   traverse attr lhs;
   traverse attr rhs;
   if not attr.pass then declare_wire lhs
| TUPLE4 (Assignment, optype, lhs, rhs) ->
   traverse attr lhs;
   traverse attr rhs;
   if attr.pass then declare_reg attr lhs
| TUPLE3 (Cont_assign, lhs, rhs) ->
   traverse attr lhs;
   traverse attr rhs;
   if attr.pass then declare_wire lhs
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING clk) as edg)), stmt) ->
   othedg := edg;
   if false then print_endline clk;
   traverse {attr with clock=Some clk} stmt
| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> traverse attr cond; traverse attr lhs; traverse attr rhs
| TUPLE3 (If_stmt, cond, lhs) -> traverse attr cond; traverse attr lhs
| TUPLE4 (If_else, cond, lhs, rhs) -> traverse attr cond; traverse attr lhs; traverse attr rhs
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), rhs) -> traverse attr rhs
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpilshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop), lhs, rhs) -> traverse attr lhs; traverse attr rhs
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop), TLIST lst) -> List.iter (traverse attr) lst
| TUPLE4 (Part_select, STRING nam, lft, rght) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC _))), STRING wire) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Part_select, Work)), STRING wire) -> ()
| TUPLE3 (Vpioutput, STRING port, VpiNum wid)	    -> ()
| TUPLE3 (Vpiinput, STRING port, VpiNum wid)	    -> if not attr.pass then declare_input (int_of_string wid) port
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> ()
| TUPLE3 (Logic_net, Vpinet, STRING net) -> ()
| TUPLE3 (Logic_net, Vpialways, STRING net) -> ()
| TUPLE2 (Logic_net, STRING net) -> ()
| TUPLE3 ((Vpibinaryconst|Vpioctconst|Vpidecconst|Vpihexconst|Vpiuintconst), (BIN _|OCT _|DEC _|HEX _|VpiNum _), VpiNum _) -> ()
| TUPLE3 (Begin, TLIST pth, TLIST lst) -> List.iter (traverse attr) lst
| STRING s -> ()
| TLIST [] -> ()
| VpiNum s -> ()
| (Always|Vpitopmodule|Vpitop|Vpiname|Vpiparent|Vpitask|Task) -> ()
| TUPLE2 (Parameter, STRING p) -> ()
| TUPLE2 (Parameter, Work) -> ()
| TUPLE2 (Case_stmt, TLIST lst) -> ()
| TUPLE3 (Bit_select, STRING nam, idx) -> traverse attr idx
| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> ()
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned")) -> ()
| TUPLE2 (Initial, stmts) -> ()
| TUPLE2 (Vpigenstmt, stmts) -> ()
| oth -> otht := Some oth; failwith "traverse failed with otht" in

let _ = List.iter (traverse {pass=false; clock=None}) p in
let _ = List.iter (traverse {pass=true; clock=None}) p in
  
let edgstmt = ref Work in
let edgstmt' = ref Void in
let conlst = ref [] in
let conlst' = ref [] in

let rec (remapp:token->remapp) = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC _))), STRING wire) ->
if exists wire then Id (wire) else failwith ("Logic_net: "^wire^" not declared")
| TUPLE3 (Vpieqop, lhs, rhs) -> Eq (remapp lhs, remapp rhs)
| TUPLE3 (If_stmt, cond, lhs) ->
  let cond_ =  (remapp cond) in
  let then_ =  (remapp lhs) in
  If_ (cond_, [then_], [])
| TUPLE4 (If_else, cond, lhs, rhs) ->
  let then_ =  (remapp lhs) in
  let else_ =  (remapp rhs) in
  let cond_ =  (remapp cond) in
  If_ (cond_, [then_], [else_])
| TUPLE3 (Cont_assign, lhs, rhs) ->
   let lhs = (remapp lhs) in
   let rhs = (remapp rhs) in
   Asgn (lhs, rhs)
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> Seq (List.map (function
    | TUPLE4 (Vpiblocking, Vpirhs, lhs, rhs) -> Block (remapp lhs, remapp rhs)
    | TUPLE4 (Vpiblocking, unhandled, lhs, rhs) -> Block (remapp lhs, Dyadic (unhandled, remapp lhs, remapp rhs))
    | TUPLE4 (Assignment, optype, lhs, rhs) as a -> remapp a
    | oth -> otht := Some oth; failwith "remapp failed with otht") stmtlst)
| TUPLE3 (Always, Vpiassignstmt, stmt) -> remapp stmt
| TUPLE4 (Assignment, Vpirhs, lhs, rhs) -> Asgn (remapp lhs, remapp rhs)
| TUPLE4 (Assignment, unhandled, lhs, rhs) -> Asgn (remapp lhs, Dyadic (unhandled, remapp lhs, remapp rhs))
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, edg), stmt) ->
   edgstmt := stmt; 
   let stmt' = remapp stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), rhs)
-> Unary (op, remapp rhs)

| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> Mux2 (remapp cond, remapp lhs, remapp rhs)
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpilshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), lhs, rhs) -> Dyadic (op, remapp lhs, remapp rhs)
| TUPLE2 (Vpiconcatop, TLIST lst) ->
   conlst := lst;
   let lst' = List.map remapp lst in
   conlst' := lst';
   let rec concat = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> Concat(hd, concat tl) in
   concat lst'
| TUPLE4 (Part_select, STRING nam, TUPLE3 (Vpiuintconst, VpiNum lft, VpiNum _), TUPLE3 (Vpiuintconst, VpiNum rght, VpiNum _)) ->
   Select(Id nam, lft, rght)
| TUPLE3 (Vpioutput, STRING port, VpiNum wid)	    -> Void
| TUPLE3 (Vpiinput, STRING port, VpiNum wid)	    -> Void
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> Void
| TUPLE3 (Logic_net, Vpinet, STRING net) -> Void
| TUPLE3 (Logic_net, Vpialways, STRING net) -> Void
| TUPLE2 (Logic_net, STRING net) -> Void
| STRING s -> Void
| TLIST [] -> Void
| TUPLE3 (Vpibinaryconst, BIN s, VpiNum wid) -> Bin (s,int_of_string wid)
| TUPLE3 (Vpioctconst, OCT s, VpiNum wid) -> Oct (s,int_of_string wid)
| TUPLE3 (Vpiuintconst, VpiNum s, VpiNum wid) -> Dec (s,int_of_string wid)
| TUPLE3 (Vpihexconst, HEX s, VpiNum wid) -> Hex (s,int_of_string wid)
| (Always|Vpitopmodule|Vpitop|Vpiname) -> Void
| TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent :: TLIST [] :: lst)) -> Seq (List.map remapp lst)
| TUPLE2 (Parameter, STRING p) -> Void
| TUPLE2 (Parameter, Work) -> Void
| TUPLE2 (Case_stmt, TLIST lst) -> Void
| TUPLE3 (Bit_select, STRING nam, idx) -> if exists nam then Bitsel (Id nam, remapp idx) else Void

| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> Void
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned")) -> Void
| TUPLE2 (Initial, stmts) -> Void
| TUPLE2 (Vpigenstmt, stmts) -> Void
| Vpiparent -> Void
| TUPLE6 (For_stmt, TLIST [], _, _, _, _) -> Void
| oth -> otht := Some oth; failwith "remapp failed with otht"; in

let remapp' = List.filter (function Void -> false |_ -> true) (List.map remapp p) in

let rec strength_reduce = function
| Asgn (Id c, 
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Id c, itm)) then_, List.map (fun itm -> Asgn (Id c, itm)) else_)
| oth -> oth in

let rec combiner = function
| If_ (Eq (a, b), c, d) ::
  If_ (Eq (a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Eq (a, b), c@c', d@d') :: []
| Seq (Block (Id blk, exp1) :: Block (Id blk', Dyadic (op, Id blk'', exp2)) :: tl) :: tl' when blk=blk' && blk'=blk'' ->
  combiner (Asgn(Id blk', Dyadic(op, exp1, exp2)) :: tl @ tl')
| Seq lst :: tl -> combiner (lst @ tl)
| oth -> oth in

let remapp'' = List.map strength_reduce remapp' in
let remapp''' = combiner remapp'' in

let sub' lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
lhs -: rhs
else if wlhs < wrhs then
(uresize lhs wrhs) -: rhs
else if wlhs > wrhs then
lhs -: (uresize rhs wlhs)
else failwith "sub'" in

let add' lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
add_fast lhs rhs
else if wlhs < wrhs then
add_fast (uresize lhs wrhs) rhs
else if wlhs > wrhs then
add_fast lhs (uresize rhs wlhs)
else failwith "mult'" in

let equal' lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
lhs ==: rhs
else if wlhs < wrhs then
(uresize lhs wrhs) ==: rhs
else if wlhs > wrhs then
lhs ==: (uresize rhs wlhs)
else failwith "equal'" in

let mux2' cond lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
mux2 cond lhs rhs
else if wlhs < wrhs then
mux2 cond (uresize lhs wrhs) rhs
else if wlhs > wrhs then
mux2 cond lhs (uresize rhs wlhs)
else failwith "mux2'" in

let mux' sel inputs =
let wid = width inputs in
mux sel (List.init wid (bit inputs)) in

let assign' (lhs:Hardcaml.Always.Variable.t) rhs =
let wlhs = width lhs.value in
let wrhs = width rhs in
if wlhs = wrhs then
lhs <-- rhs
else if wlhs < wrhs then
lhs <-- (uresize rhs wlhs)
else if wlhs > wrhs then
lhs <-- (uresize rhs wlhs)
else failwith "equal'" in

let unimp' lhs rhs = failwith "unimp'" in

let unimp lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
unimp' lhs rhs
else if wlhs < wrhs then
unimp' (uresize lhs wrhs) rhs
else if wlhs > wrhs then
unimp' lhs (uresize rhs wlhs)
else failwith "unimp" in

let remapop = function
|Vpiaddop -> add'
|Vpisubop -> sub'
|Vpimultop -> mult_wallace
|Vpidivop -> unimp
|Vpimodop -> unimp
|Vpilshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpirshiftop -> (fun lhs rhs -> Signal.log_shift srl rhs lhs)
|Vpiarithrshiftop -> (fun lhs rhs -> Signal.log_shift sra rhs lhs)
|Vpiarithlshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpilogandop -> unimp
|Vpilogorop -> unimp
|Vpibitandop -> (&:)
|Vpibitorop -> (|:)
|Vpibitxorop -> (^:)
|Vpibitxnorop -> (fun lhs rhs -> lhs ^: (~: rhs))
|Vpieqop -> unimp
|Vpineqop -> unimp
|Vpiltop -> unimp
|Vpileop -> unimp
|Vpigeop -> unimp
|Vpigtop -> unimp
|oth -> otht := Some oth; failwith "remapop" in

let rec (remap:remapp->remap) = function
| Id wire ->
if exists wire then find_decl wire else Invalid
| Eq (lhs, rhs) -> Sig (equal' (sig' (remap lhs)) (sig' (remap rhs)))
| Dyadic (op, lhs, rhs) -> Sig (remapop op (sig' (remap lhs)) (sig' (remap rhs)))
| Mux2 (cond, lhs, rhs) ->
  let cond_ = sig' (remap cond) in
  let then_ = sig' (remap lhs) in
  let else_ = sig' (remap rhs) in
  Sig (mux2' cond_ then_ else_)
| If_ (cond, lhs, rhs) ->
  let cond_ = sig' (remap cond) in
  let then_ = List.map (fun itm -> alw' (remap itm)) lhs in
  let else_ = List.map (fun itm -> alw' (remap itm)) rhs in
  Alw (if_ cond_ then_ else_)
| Asgn (lhs, rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Alw (assign' lhs rhs)
| Hex (s,width) -> Con (Constant.of_hex_string ~width ~signedness:Unsigned s)
| Dec (s,width) -> Con (Constant.of_z ~width (Z.of_string s))
| Oct (s,width) -> Con (Constant.of_octal_string ~width ~signedness:Unsigned s)
| Bin (s,width) -> Con (Constant.of_binary_string_hum s)
| Concat (lhs, rhs) -> 
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = sig' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Sig (lhs @: rhs)
| Select(nam, lft, rght) -> Sig (select (sig' (remap nam)) (int_of_string lft) (int_of_string rght))
| Bitsel(nam, sel) -> Sig (mux' (sig' (remap sel)) (sig' (remap nam)))
| oth -> othp := oth; failwith "remap"
 in

othremapp' := remapp';
othremapp'' := remapp'';
othremapp''' := remapp''';
let remap' = List.filter (function Invalid -> false | Sig _ -> false |_ -> true) (List.map remap remapp''') in
let remap'' = List.map alw' remap' in
let _ = Always.compile remap'' in
alst := [];
let oplst = ref [] in iter_decl (fun k -> function
        | Var v -> oplst := output k v.value :: !oplst
        | Con v -> ()
        | Alw v -> ()
        | Sig v -> alst := (k, v) :: !alst
        | Invalid -> ());
Hardcaml.Rtl.output Verilog (Hardcaml.Circuit.create_exn ~name:modnam !oplst);
if Array.length Sys.argv > 2 then eqv Sys.argv.(2) modnam
end

let modlst = ref []

let parseall f = let p,lst = parse f in p' := p; modlst := lst; List.iter cnv lst

let _ = if Array.length Sys.argv > 1 then parseall Sys.argv.(1)

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
  | Void of int
  | Id of string
  | Alw of Hardcaml.Always.t
  | Bin of string * int
  | Oct of string * int
  | Dec of string * int
  | Hex of string * int
  | If_ of remapp * remapp list * remapp list
  | Mux2 of remapp * remapp * remapp
  | Asgn of remapp * remapp
  | Block of remapp * remapp
  | Concat of token * remapp * remapp
  | Selection of remapp * int * int * int * int
  | Update of remapp * int * int * int * int
  | Bitsel of remapp * remapp
  | Seq of remapp list
  | Unary of token * remapp
  | Dyadic of token * remapp * remapp
  | Case of remapp * remapp list
  | Item of remapp * remapp * remapp

type remap =
  | Invalid
  | Con of Hardcaml.Constant.t
  | Sig of Hardcaml.Signal.t
  | Alw of Hardcaml.Always.t
  | Var of Hardcaml.Always.Variable.t
  | Itm of (Hardcaml.Signal.t * Hardcaml.Always.t list)

type attr = {
  pass: bool;
  clock: string option;
  reset: string option;
}

let othr = ref Invalid
let othlhs = ref (Void 0)
let othrhs = ref (Void 0)
let othrmlhs = ref Invalid
let othrmrhs = ref Invalid
let othlhs' = ref (Void 0)
let othrhs' = ref (Void 0)
let othrmlhs' = ref Invalid
let othrmrhs' = ref Invalid
let othp = ref (Void 0)
let otht = ref None
let othremapp' = ref []
let othremapp'' = ref []
let othremapp''' = ref []
let othdecl = ref []
let alst = ref []
let othckedg = ref Work
let othrstedg = ref Work
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

let sub' lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
lhs -: rhs
else if wlhs < wrhs then
(uresize lhs wrhs) -: rhs
else if wlhs > wrhs then
lhs -: (uresize rhs wlhs)
else failwith "sub'"

let add' lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
add_fast lhs rhs
else if wlhs < wrhs then
add_fast (uresize lhs wrhs) rhs
else if wlhs > wrhs then
add_fast lhs (uresize rhs wlhs)
else failwith "mult'"

let relational relation lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
relation lhs rhs
else if wlhs < wrhs then
relation (uresize lhs wrhs) rhs
else if wlhs > wrhs then
relation lhs (uresize rhs wlhs)
else failwith "relational"

let mux2' cond lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
mux2 cond lhs rhs
else if wlhs < wrhs then
mux2 cond (uresize lhs wrhs) rhs
else if wlhs > wrhs then
mux2 cond lhs (uresize rhs wlhs)
else failwith "mux2'"

let othsel = ref None

let mux' sel inputs =
let wid = width inputs in
othsel := Some sel;
print_endline ("mux width: "^string_of_int wid);
mux sel (List.init wid (bit inputs))

let assign' (lhs:Hardcaml.Always.Variable.t) rhs =
let wlhs = width lhs.value in
let wrhs = width rhs in
if wlhs = wrhs then
lhs <-- rhs
else if wlhs < wrhs then
lhs <-- (uresize rhs wlhs)
else if wlhs > wrhs then
lhs <-- (uresize rhs wlhs)
else failwith "assign'"

let cnv (modnam, p) =
begin
let declare_lst = ref [] in
let exists k = List.mem_assoc k !declare_lst in
let add_decl k wid x =
  if exists k then print_endline (k^": redeclared") else
  begin
    declare_lst := (k, x) :: !declare_lst;
    othdecl := (k, wid, match x with
      | Con _ -> x
      | Var _ -> Var (Always.Variable.wire ~default:(Signal.zero 1))
      | Sig _ -> x
      | Alw _ | Invalid -> failwith "add_decl") :: !othdecl
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
  add_decl port wid (Sig (Signal.input port wid)) in

let declare_wire = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width(hi,lo)))), STRING wire) -> let wid=hi-lo+1 in
  add_decl wire wid (Var (Always.Variable.wire ~default:(Signal.zero wid)))
| TUPLE5 (Part_select, STRING wire,
      TUPLE3 (Vpiuintconst, Int lft, Int _),
      TUPLE3 (Vpiuintconst, Int rght, Int _), Width(hi, lo)) -> let wid = hi-lo+1 in
  add_decl wire wid (Var (Always.Variable.wire ~default:(Signal.zero wid)))
| oth -> othrw := Some oth; failwith "declare_wire" in

let declare_reg attr = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width(hi,lo)))), STRING reg) -> if exists reg then () else
  begin
  if false then print_endline reg;
  let clock = match attr.clock with Some clk -> sig' (find_decl clk) | None -> failwith ("failed to indentify clock for register: "^reg) in
  match attr.reset with
  | Some rst -> let reset = sig' (find_decl rst) in
    let r_sync = Reg_spec.create ~clock ~reset () in
    let wid = hi-lo+1 in
    add_decl reg wid (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid));
  | None -> 
    let r_sync = Reg_spec.create ~clock () in
    let wid = hi-lo+1 in
    add_decl reg wid (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid));
  end
| oth -> othrw := Some oth; failwith "declare_reg" in

let rec traverse (attr:attr) = function
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> List.iter (function
    | TUPLE4 ((Assignment|Vpiblocking), optype, lhs, rhs) ->
        traverse attr lhs;
        traverse attr rhs;
        if not attr.pass then declare_wire lhs
    | oth -> otht := Some oth; failwith "always traverse failed with otht") stmtlst
| TUPLE3 (Always, Vpiassignstmt,
   TUPLE4 ((Assignment|Vpiblocking), optype, lhs, rhs)) ->
   traverse attr lhs;
   traverse attr rhs;
   if not attr.pass then declare_wire lhs
| TUPLE4 ((Assignment|Vpiblocking), optype, lhs, rhs) ->
   traverse attr lhs;
   traverse attr rhs;
   if attr.pass then declare_reg attr lhs
| TUPLE3 (Cont_assign, lhs, rhs) ->
   traverse attr lhs;
   traverse attr rhs;
   if attr.pass then declare_wire lhs
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING clk) as edg)), stmt) ->
   othckedg := edg;
   if false then print_endline clk;
   traverse {attr with clock=Some clk} stmt
| TUPLE3 (Always, TUPLE3(Vpieventorop,
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST ckpth, ckloc))), STRING clk) as ckedg)),
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST rstpth, rstloc))), STRING rst) as rstedg))), stmt) ->
   othckedg := ckedg;
   othrstedg := rstedg;
   if false then print_endline clk;
   traverse {attr with clock=Some clk; reset=Some rst} stmt
| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> traverse attr cond; traverse attr lhs; traverse attr rhs
| TUPLE3 (If_stmt, cond, lhs) -> traverse attr cond; traverse attr lhs
| TUPLE4 (If_else, cond, lhs, rhs) -> traverse attr cond; traverse attr lhs; traverse attr rhs
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), rhs) -> traverse attr rhs
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop), lhs, rhs) -> traverse attr lhs; traverse attr rhs
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop), TLIST lst) -> List.iter (traverse attr) lst
| TUPLE5 (Part_select, STRING nam, lft, rght, Width(hi,lo)) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width _))), STRING wire) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Part_select, Work)), STRING wire) -> ()
| TUPLE3 (Vpioutput, STRING port, Width(lft,rght))	    -> ()
| TUPLE3 (Vpiinput, STRING port, Width(lft,rght))	    -> if not attr.pass then declare_input (lft-rght+1) port
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> ()
| TUPLE3 (Logic_net, Vpinet, STRING net) -> ()
| TUPLE3 (Logic_net, Vpialways, STRING net) -> ()
| TUPLE2 (Logic_net, STRING net) -> ()
| TUPLE3 ((Vpibinaryconst|Vpioctconst|Vpidecconst|Vpihexconst|Vpiuintconst), (BIN _|OCT _|DEC _|HEX _|Int _), Int _) -> ()
| TUPLE3 (Begin, TLIST pth, TLIST lst) -> List.iteri (fun ix itm -> traverse attr itm) lst
| STRING s -> ()
| TLIST [] -> ()
| Int s -> ()
| (Always|Vpitopmodule|Vpitop|Vpiname|Vpiparent|Vpitask|Task) -> ()
| TUPLE2 (Parameter, STRING p) -> ()
| TUPLE2 (Parameter, Work) -> ()
| TUPLE2 (Case_stmt, TLIST (TUPLE2 (Vpicasetype, Int 1) :: TUPLE2 (Vpicondition, cond) :: lst)) ->
    traverse attr cond;
    List.iteri (fun ix -> function
      | TUPLE3 (Case_item, cons, stmt) ->
        traverse attr cons;
        traverse attr stmt;
        otht := Some stmt;
      | oth -> otht := Some oth; failwith "traverse case") lst
| TUPLE3 (Bit_select, STRING nam, idx) -> traverse attr idx
| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> ()
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned")) -> ()
| TUPLE2 (Initial, stmts) -> ()
| TUPLE2 (Vpigenstmt, stmts) -> ()
| oth -> otht := Some oth; failwith "traverse failed with otht" in

let _ = List.iter (traverse {pass=false; clock=None; reset=None}) p in
let _ = List.iter (traverse {pass=true; clock=None; reset=None}) p in

let edgstmt = ref Work in
let edgstmt' = ref (Void 0) in
let conlst = ref [] in
let conlst' = ref [] in

let rec (remapp:token->remapp) = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width(hi,lo)))), STRING wire) ->
if exists wire then Id (wire) else failwith ("Logic_net: "^wire^" not declared")
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
   let rhs = (remapp rhs) in
   let lhs = match remapp lhs with
      | Selection(id, lft, rght, hi, lo) -> Update(id, lft, rght, hi, lo)
      | oth -> othlhs := oth; failwith "remapp cont_assign failed with otht"; in
   Asgn (lhs, rhs)
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> Seq (List.map remapp stmtlst)
| TUPLE3 (Always, Vpiassignstmt, stmt) -> remapp stmt
| TUPLE4 (Assignment, Vpirhs, lhs, rhs) -> Asgn (remapp lhs, remapp rhs)
| TUPLE4 (Vpiblocking, Vpirhs, lhs, rhs) -> Block (remapp lhs, remapp rhs)
| TUPLE4 (Assignment, op, lhs, rhs) -> Asgn (remapp lhs, Dyadic (op, remapp lhs, remapp rhs))
| TUPLE4 (Vpiblocking, op, lhs, rhs) -> Block (remapp lhs, Dyadic (op, remapp lhs, remapp rhs))
(*
| TUPLE4 (Vpiblocking, Vpirhs, lhs, rhs) -> Block (remapp lhs, remapp rhs)
| TUPLE4 (Vpiblocking, unhandled, lhs, rhs) -> Block (remapp lhs, Dyadic (unhandled, remapp lhs, remapp rhs))
| TUPLE4 (Assignment, optype, lhs, rhs) as a -> remapp a
*)
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, edg), stmt) ->
   edgstmt := stmt; 
   let stmt' = remapp stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE3 (Always, TUPLE3(Vpieventorop,
			 TUPLE2 (Vpiposedgeop, ckedg),
			 TUPLE2 (Vpiposedgeop, rstedg)), stmt) ->
   edgstmt := stmt; 
   let stmt' = remapp stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), rhs)
-> Unary (op, remapp rhs)

| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> Mux2 (remapp cond, remapp lhs, remapp rhs)
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), lhs, rhs) -> Dyadic (op, remapp lhs, remapp rhs)
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop as op), TLIST lst) ->
   conlst := lst;
   let lst' = List.map remapp lst in
   conlst' := lst';
   let rec concat = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> Concat(op, hd, concat tl) in
   concat lst'
| TUPLE5 (Part_select, STRING nam, TUPLE3 (Vpiuintconst, Int lft, Int _), TUPLE3 (Vpiuintconst, Int rght, Int _), Width(hi,lo)) ->
   Selection(Id nam, lft, rght, hi, lo)
| TUPLE3 (Vpioutput, STRING port, Width(hi,lo))	    -> Void 382
| TUPLE3 (Vpiinput, STRING port, Width(hi,lo))	    -> Void 383
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> Void 384
| TUPLE3 (Logic_net, Vpinet, STRING net) -> Void 385
| TUPLE3 (Logic_net, Vpialways, STRING net) -> Void 386
| TUPLE2 (Logic_net, STRING net) -> Void 387
| STRING s -> Void 388
| TLIST [] -> Void 389
| TUPLE3 (Vpibinaryconst, BIN s, Int wid) -> Bin (s,wid)
| TUPLE3 (Vpioctconst, OCT s, Int wid) -> Oct (s,wid)
| TUPLE3 ((Vpidecconst|Vpiuintconst), DEC s, Int wid) -> Dec (s,wid)
| TUPLE3 (Vpihexconst, HEX s, Int wid) -> Hex (s,wid)
| TUPLE3 (Vpiuintconst, Int n, Int wid) -> Dec(string_of_int n, wid)
| (Always|Vpitopmodule|Vpitop|Vpiname) -> Void 394
| TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent :: TLIST [] :: lst)) -> Seq (List.map remapp lst)
| TUPLE2 (Parameter, STRING p) -> Void 396
| TUPLE2 (Parameter, Work) -> Void 397
| TUPLE2 (Case_stmt, TLIST (TUPLE2 (Vpicasetype, Int 1) :: TUPLE2 (Vpicondition, cond) :: lst)) ->
    let cond' = remapp cond in
    Case (cond', List.map (function TUPLE3 (Case_item, cons, stmt) -> Item (cond', remapp cons, remapp stmt) | oth -> failwith "Case") lst)
| TUPLE3 (Bit_select, STRING nam, idx) -> if exists nam then Bitsel (Id nam, remapp idx) else Void 399

| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> Void 401
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned")) -> Void 402
| TUPLE2 (Initial, stmts) -> Void 403
| TUPLE2 (Vpigenstmt, stmts) -> Void 404
| Vpiparent -> Void 405
| TUPLE6 (For_stmt, TLIST [], _, _, _, _) -> Void 406
| oth -> otht := Some oth; failwith "remapp failed with otht"; in

let remapp' = List.filter (function Void _ -> false |_ -> true) (List.map remapp p) in

let rec strength_reduce = function
| Asgn (Id c, 
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Id c, itm)) then_, List.map (fun itm -> Asgn (Id c, itm)) else_)
| oth -> oth in

let rec combiner = function
| [] -> []
| If_ (Dyadic (Vpieqop, a, b), c, d) ::
  If_ (Dyadic (Vpieqop, a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Dyadic (Vpieqop, a, b), c@c', d@d') :: []
| Seq (Block (Id blk, exp1) :: Block (Id blk', Dyadic (op, Id blk'', exp2)) :: tl) :: tl' when blk=blk' && blk'=blk'' ->
  combiner (Asgn(Id blk', Dyadic(op, exp1, exp2)) :: tl @ tl')
| Seq lst :: tl -> combiner (lst @ tl)
| Asgn (Update (Id dest, lfthi, lftlo, hi, lo), a) :: Asgn (Update (Id dest', rghthi, rghtlo, hi', lo'), b) :: tl
when dest=dest' && lfthi=hi && lftlo=rghthi+1 && rghtlo=lo && hi=hi' && lo=lo' -> Asgn(Id dest, Concat ( Vpiconcatop, a, b )) :: combiner tl
| Block(id, expr) :: tl -> Asgn(id, expr) :: combiner tl
| hd :: tl -> hd :: combiner tl in

let remapp'' = List.map strength_reduce remapp' in
let remapp''' = combiner remapp'' in

let unimpld = let seen = ref [] in fun lbl lhs rhs ->
if not (List.mem lbl !seen) then print_endline ("unimpld: "^lbl); seen := lbl :: !seen; lhs ^: rhs in

let unimp lbl lhs rhs =
let wlhs = width lhs in
let wrhs = width rhs in
if wlhs = wrhs then
unimpld lbl lhs rhs
else if wlhs < wrhs then
unimpld lbl (uresize lhs wrhs) rhs
else if wlhs > wrhs then
unimpld lbl lhs (uresize rhs wlhs)
else failwith "unimp" in

let negate fn = fun lhs rhs -> fn lhs (~: rhs) in

let remapop = function
|Vpiaddop -> add'
|Vpisubop -> sub'
|Vpimultop -> mult_wallace
|Vpidivop -> unimp "div"
|Vpimodop -> unimp "mod"
|Vpipowerop -> unimp "power"
|Vpilshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpirshiftop -> (fun lhs rhs -> Signal.log_shift srl rhs lhs)
|Vpiarithrshiftop -> (fun lhs rhs -> Signal.log_shift sra rhs lhs)
|Vpiarithlshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpilogandop -> (&&:)
|Vpilogorop -> (||:)
|Vpibitandop -> (&:)
|Vpibitorop -> (|:)
|Vpibitxorop -> (^:)
|Vpibitxnorop -> negate (^:)
|Vpieqop -> relational (==:)
|Vpineqop -> relational (<>:)
|Vpiltop -> relational (<:)
|Vpileop -> relational (<=:)
|Vpigeop -> relational (>=:)
|Vpigtop -> relational (>:)
|oth -> otht := Some oth; failwith "remapop" in

let unimplu = let seen = ref [] in fun lbl rhs ->
if not (List.mem lbl !seen) then print_endline ("unimplu: "^lbl); seen := lbl :: !seen; ~: rhs in

let fold' fn rhs = let expl = List.init (width rhs) (bit rhs) in List.fold_left fn (List.hd expl) (List.tl expl) in

let remapop' = function
|Vpiplusop -> (fun rhs -> rhs)
|Vpiminusop -> (fun rhs -> Signal.zero (width rhs) -: rhs)
|Vpiunaryandop -> fold' (&:)
|Vpiunarynandop -> fold' (negate (&:))
|Vpiunaryorop -> fold' (|:)
|Vpiunarynorop -> fold' (negate (|:))
|Vpiunaryxorop -> fold' (^:)
|Vpiunaryxnorop -> fold' (negate (^:))
|Vpibitnegop -> (~:)
|Vpinotop -> (~:)
|oth -> otht := Some oth; failwith "remapop'" in

let rec (remap:remapp->remap) = function
| Id wire ->
if exists wire then find_decl wire else Invalid
| Unary (op, rhs) -> Sig (remapop' op (sig' (remap rhs)))
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
| Asgn (Update(lhs, lft, rght, hi, lo), rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   let rhs' = rhs @: select lhs.value lft rght in (* placeholder, not working yet *)
   Alw (assign' lhs rhs')
| Asgn (lhs, rhs) ->
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Alw (assign' lhs rhs)
| Hex (s,width) -> Con (Constant.of_hex_string ~width ~signedness:Unsigned s)
| Dec (s,width) -> Con (Constant.of_z ~width (Z.of_string s))
| Oct (s,width) -> Con (Constant.of_octal_string ~width ~signedness:Unsigned s)
| Bin (s,width) -> Con (Constant.of_binary_string_hum s)
| Concat (op, lhs, rhs) -> 
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = sig' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Sig (lhs @: rhs)
| Selection(nam, lft, rght, hi, lo) -> Sig (select (sig' (remap nam)) (lft) (rght))
| Bitsel(nam, Dec (n, _)) -> Sig (bit (sig' (remap nam)) (int_of_string n))
| Bitsel(nam, sel) as b -> othp := b; Sig (mux' (sig' (remap sel)) (sig' (remap nam)))
| Item (cond, Dec (n, _), stmt) -> let wid = width (sig' (remap cond)) in Itm (of_int ~width:wid (int_of_string n), [ alw' (remap stmt) ])
| Case (mode, lst) -> Alw (Always.switch (sig' (remap mode)) (List.map (fun itm -> match remap itm with Itm c -> c | _ -> failwith "itm") lst))
| oth -> othp := oth; failwith "remap" in

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

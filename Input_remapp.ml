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
open Input_types
open Input_rewrite
open Hardcaml
open Always
open Signal

open Input
open Input_rewrite
open Input_lex

let edgstmt = ref Work
let edgstmt' = ref (Void 0)
let dbgp = ref []
let dbgr = ref []
let othact = ref Work
let othasgn = ref Work
let othr = ref (Void 0)
let othlhs = ref (Void 0)
let othrhs = ref (Void 0)
let othrmlhs = ref (Void 0)
let othrmrhs = ref (Void 0)
let othlhs' = ref (Void 0)
let othrhs' = ref (Void 0)
let othrmlhs' = ref (Void 0)
let othrmrhs' = ref (Void 0)
let othp = ref (Void 0)
let otht = ref None
let othremapp' = ref []
let othremapp'' = ref []
let othremapp''' = ref []
let othckedg = ref Work
let othrstedg = ref Work
let othop = ref (Work, (Void 0), (Void 0))
let p' = ref []

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n/2)

let exact_log2 n = 1 lsl (log2 n) = n

let signed_zero n = Signed.of_signal (Signal.zero n)
let arithnegate rhs = Signal.zero (width rhs) -: rhs
let signedwidth rhs = width (Signed.to_signal rhs)
let signednegate rhs = let open Signed in signed_zero (signedwidth rhs) -: rhs

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

let mult_wallace_signed a_sig b_sig =
          let open Signed in
          let nega = to_signal (a_sig <: signed_zero (signedwidth a_sig)) in
          let negb = to_signal (b_sig <: signed_zero (signedwidth b_sig)) in
          let mult' = Hardcaml_circuits.Mul.create
             ~config:Wallace
             (module Signal)
             (mux2' (nega) (to_signal (signednegate a_sig)) (to_signal a_sig))
             (mux2' (negb) (to_signal (signednegate b_sig)) (to_signal b_sig))
          in (of_signal (mux2' (nega ^: negb) (arithnegate mult') mult'))

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
Sigpp (relation lhs rhs)
else if wlhs < wrhs then
Sigpp (relation (uresize lhs wrhs) rhs)
else if wlhs > wrhs then
Sigpp (relation lhs (uresize rhs wlhs))
else failwith "relational"

let relationalc relation lhs rhs = Sigpp (relation lhs rhs)
let relationalc' relation lhs rhs = Sigspp (relation lhs rhs)

let relational' relation lhs rhs =
let wlhs = signedwidth lhs in
let wrhs = signedwidth rhs in
if wlhs = wrhs then
Sigspp (relation lhs rhs)
else if wlhs < wrhs then
Sigspp (relation (Signed.resize lhs wrhs) rhs)
else if wlhs > wrhs then
Sigspp (relation lhs (Signed.resize rhs wlhs))
else failwith "relational'"

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

let modlst = ref []
let modlst' = ref []
let crntmod = ref ("", [])
let remap_lst = ref []

let cnv_remapp (modnam, p) =
begin
print_endline ("cnv_remapp: "^modnam);
let declare_lst = ref [] in
let exists k = List.mem_assoc k !declare_lst in
let add_decl (k:string) (x:remapp) = function
| Width(hi,lo,signed) as wid ->
  if exists k then print_endline (k^": redeclared") else
  begin
    declare_lst := (k, x) :: !declare_lst;
  end
| oth -> otht := Some oth; failwith "add_decl" in

let find_decl k = if exists k then List.assoc k !declare_lst else failwith ("find_decl: "^k) in
let iter_decl fn = List.iter (fun (k,x) -> fn k x) !declare_lst in

let unsigned x = Signed.to_signal x in

(*
let sig' = function
| Conpp x -> x
| Sigpp x -> x
| Sigspp x -> x
| Wirepp x -> x
| oth -> othr := oth; failwith "sig'" in
*)

let alw' = function
| Alwpp x -> x
| oth -> othr := oth; failwith "alw'" in

let var' = function
| Wirepp x -> x
| oth -> othr := oth; failwith "var'" in

let declare_input port = function
| Width(hi,lo,signed) as w ->
  if false then print_endline port;
  add_decl port (if signed then Inpspp (Primary (hi,lo,signed)) else Inppp (Primary (hi,lo,signed))) w
| oth -> otht := Some oth; failwith "declare_input" in

let logf = open_out "logfile.txt" in

let declare_wire = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, (Width(hi,lo,signed) as wid)))), STRING wire) -> let wid' = hi-lo+1 in
  output_string logf ("wire "^wire^"\n");
  add_decl wire (Wirepp (Primary (hi,lo,signed))) wid
| TUPLE3 (Work, STRING wire, (Width(hi,lo,signed) as wid)) -> let wid' = hi-lo+1 in
  output_string logf ("wire "^wire^"\n");
  add_decl wire (Wirepp (Primary (hi,lo,signed))) wid
| TUPLE5 (Part_select, STRING wire,
      TUPLE3 (Vpiuintconst, Int lft, Int _),
      TUPLE3 (Vpiuintconst, Int rght, Int _), (Width(hi, lo, signed) as wid)) -> let wid' = hi-lo+1 in
  add_decl wire (Wirepp (Primary (hi,lo,signed))) wid
| TUPLE3 (Bit_select, STRING wire, TUPLE3 (Vpiuintconst, Int lft, Int _)) -> let wid' = 1 in
  output_string logf ("wire "^wire^"\n");
  let wid = Width(0,0,false) in add_decl wire (Wirepp (Primary (0,0,false))) wid
| oth -> othrw := Some oth; failwith "declare_wire" in

let declare_orphan = function
| TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
					     TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, (Width(hi, lo, signed) as wid))))) ->
  let wire = List.hd (List.rev (List.map (function STRING s -> s | oth -> failwith "declare_wire'") pth)) in
  let wid' = hi-lo+1 in
  output_string logf ("orphan "^wire^"\n");
 let s = Signal.input wire (hi-lo+1) in
  add_decl wire (if signed then Sigspp (Primary (hi,lo,signed)) else Sigpp (Primary (hi,lo,signed))) wid
| oth -> othrw := Some oth; failwith "declare_orphan" in

let clock attr = match attr.clock with
| Some clk -> if true then print_endline ("clock: "^clk); (find_decl clk)
| None -> failwith ("failed to identify clock for declare_reg") in

let declare_reg attr = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, (Width(hi,lo, signed) as wid)))), STRING reg) -> if exists reg then () else
  begin
  let width = hi-lo+1 in
  if true then print_endline ("declare_reg "^reg^": "^string_of_int width);
  add_decl reg (Regpp (hi,lo,signed,attr) ) wid;
  end
| TUPLE5 (Part_select, STRING reg,
      TUPLE3 (Vpiuintconst, Int lft, Int _),
      TUPLE3 (Vpiuintconst, Int rght, Int _), (Width(hi, lo, signed) as wid)) -> let wid' = hi-lo+1 in
  add_decl reg (Regpp (hi,lo,signed,attr)) wid;
| TUPLE3 (Bit_select, STRING reg, TUPLE3 (Vpiuintconst, Int lft, Int _)) -> let wid' = 1 in
  let wid = Width(0,0,false) in add_decl reg (Regpp (0,0,false,attr)) wid
| oth -> othrw := Some oth; failwith "declare_reg" in

let conlst = ref [] in
let conlst' = ref [] in
let othfunc = ref "" in

let sys_func x = function
| "$signed" -> Signed x
| "$unsigned" -> Unsigned x
| oth -> othfunc := oth; failwith "sys_func" in

let rec remapp (attr:attr) = function
| Enum_typespec -> Void 354
| STRING nam -> Void 355
| TUPLE2 (Vpiinstance, STRING inst) -> Void 356
| TUPLE2 (Parameter, (Work|STRING _)) -> Void 357
| TUPLE3 (Vpioutput, STRING port, Width(hi,lo, signed))	    -> Void 382
| TUPLE3 (Vpiinput, STRING port, (Width(hi,lo, signed) as wid))	    -> declare_input port wid; Void 383
| Vpideffile | Vpideflineno | Always | Cont_assign -> Void 359
| TUPLE2 ((Vpimodule|Gen_scope_array|Cont_assign), _) -> Void 60
| TUPLE2 (Enum_var, STRING _) -> Void 361
| TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) -> Void 362
| TUPLE4 ((Vpialways|Vpinet|Vpireg),
          TUPLE2 (Vpitypespec,
            TUPLE3 (Ref_typespec,
              TLIST pth,
              TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))),
          STRING pin,
          TLIST pth') -> Void 369
| TUPLE2 (Vpimodule, TLIST lst) -> Seq (List.map (remapp attr) lst)
| TUPLE2 (Enum_typespec, TLIST lst) -> List.iter (function
  | TUPLE3(STRING enum, Int n, Int width) -> add_decl enum (Conpp (Int n)) (Width(width-1, 0, false))
  | TUPLE2 (Vpitypespec,
      TUPLE3 (Ref_typespec, TLIST pth,
        TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))) -> ()
  | oth -> othrw := Some oth; failwith "enum") lst; Void 444
| TUPLE4 (Vpireg, TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
          TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))), STRING nam, TLIST pth') -> Void 446
| TUPLE4 ((Logic_net|Vpialways|Vpinet), TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth, TUPLE2 (Vpiactual, spec))),
      STRING nam, TLIST pth') -> Void 448

| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, (Width(hi,lo,signed) as wid)))), STRING wire) ->
if not (exists wire) then add_decl wire (if signed then Sigspp (Primary (hi,lo,signed)) else Sigpp (Primary (hi,lo,signed))) wid; Id (find_decl wire)
| TUPLE3 (If_stmt, cond, lhs) ->
  let cond_ =  ((remapp attr) cond) in
  let then_ =  ((remapp attr) lhs) in
  If_ (cond_, [then_], [])
| TUPLE4 (If_else, cond, lhs, rhs) ->
  let then_ =  ((remapp attr) lhs) in
  let else_ =  ((remapp attr) rhs) in
  let cond_ =  ((remapp attr) cond) in
  If_ (cond_, [then_], [else_])
| TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) ->
   let rhs = ((remapp attr) rhs) in
   let lhs = match (remapp attr) lhs with
   | Selection(id, lft, rght, hi, lo) -> Update(id, lft, rght, hi, lo)
   | Id _ as id -> id
   | Bitsel (Id id, Dec (sel, _)) -> let ix = int_of_string sel in Update(Id id, ix, ix, ix, ix)
   | oth -> othlhs := oth; failwith "remapp cont_assign failed with othlhs"; in
   Asgn (lhs, rhs)
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> Seq (List.map (remapp attr) stmtlst)
| TUPLE3 (Always, Vpiassignstmt, stmt) -> (remapp attr) stmt
| TUPLE3 (Always, TUPLE2 (Vpieventorop, TLIST lst), stmt) -> Seq (List.map (remapp attr) lst)

| TUPLE4 (Assignment, Vpirhs, lhs, rhs) ->
(match attr.clock with Some _ -> declare_reg attr lhs | None -> declare_wire lhs);
Asgn ((remapp attr) lhs, (remapp attr) rhs)
| TUPLE4 (Vpiblocking, Vpirhs, lhs, rhs) ->
(match attr.clock with Some _ -> declare_reg attr lhs | None -> declare_wire lhs);
Block ((remapp attr) lhs, (remapp attr) rhs)
| TUPLE4 (Assignment, op, lhs, rhs) ->
(match attr.clock with Some _ -> declare_reg attr lhs | None -> declare_wire lhs);
Asgn ((remapp attr) lhs, Dyadic (op, (remapp attr) lhs, (remapp attr) rhs))
| TUPLE4 (Vpiblocking, op, lhs, rhs) ->
(match attr.clock with Some _ -> declare_reg attr lhs | None -> declare_wire lhs);
Block ((remapp attr) lhs, Dyadic (op, (remapp attr) lhs, (remapp attr) rhs))
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING clk) as edg)), stmt) ->
   edgstmt := stmt; 
   let stmt' = (remapp {attr with clock=Some clk}) stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE3 (Always, TUPLE3(Vpieventorop,
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST ckpth, ckloc))), STRING clk) as ckedg)),
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST rstpth, rstloc))), STRING rst) as rstedg))), stmt) ->
   edgstmt := stmt; 
   let stmt' = (remapp {attr with clock=Some clk; reset=Some rst}) stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), rhs)
-> Unary (op, (remapp attr) rhs)

| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> Mux2 ((remapp attr) cond, (remapp attr) lhs, (remapp attr) rhs)
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), lhs, rhs) -> Dyadic (op, (remapp attr) lhs, (remapp attr) rhs)
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop as op), TLIST lst) ->
   conlst := lst;
   let lst' = List.map (remapp attr) lst in
   conlst' := lst';
   Input_pp.concat op lst'
| TUPLE5 (Part_select, STRING wire, TUPLE3 (Vpiuintconst, Int lft, Int _), TUPLE3 (Vpiuintconst, Int rght, Int _), (Width(hi,lo, signed) as wid)) ->
  if not (exists wire) then add_decl wire (if signed then Sigspp (Primary (hi,lo,signed)) else Sigpp (Primary (hi,lo,signed))) wid;
  Selection(Id (find_decl wire), lft, rght, hi, lo)
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
| TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent :: TLIST [] :: lst)) -> Seq (List.map (remapp attr) lst)
| TUPLE2 (Parameter, STRING p) -> Void 396
| TUPLE2 (Parameter, Work) -> Void 397
| TUPLE2 (Case_stmt, TLIST (TUPLE2 (Vpicasetype, Int 1) :: TUPLE2 (Vpicondition, cond) :: lst)) ->
    let cond' = (remapp attr) cond in
    Case (cond', List.map (function
			   | TUPLE3 (Case_item, cons, stmt) -> Item (cond', (remapp attr) cons, (remapp attr) stmt)
			   | TUPLE2 (Case_item, stmt) -> Item (Void 536, Void 536, (remapp attr) stmt)
			   | oth -> otht := Some oth; failwith "Case") lst)
| TUPLE3 (Bit_select, STRING nam, idx) ->
if not (exists nam) then add_decl nam (Othpp (Primary (0,0,false))) (Width(0,0,false));
Bitsel (Id (find_decl nam), (remapp attr) idx)

| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> Void 401
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned" as func)) -> sys_func ((remapp attr) actual) func
| TUPLE2 (Initial, stmts) -> Void 403
| TUPLE2 (Vpigenstmt, stmts) -> Void 404
| Vpiparent -> Void 405
| TUPLE6 (For_stmt, TLIST [], _, _, _, _) -> Void 406
| TUPLE2 (TUPLE2 (Vpiactual, Enum_const), STRING e) -> Void 543
| TUPLE4 (Ref_obj, STRING nam, TLIST pth, actual) -> othact := actual; Id (find_decl nam)
| TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, loc))) -> Void 550
| TUPLE2 (Enum_var, STRING _) -> Void 584
| Enum_typespec | Cont_assign | Constant -> Void 585
| oth -> otht := Some oth; failwith "remapp failed with otht"; in

let _ = print_endline ("List length p = "^string_of_int (List.length p)) in
dbgp := p;

let q = List.map (remapp {clock=None; reset=None; enable=None}) p in
let _ = print_endline ("List length q = "^string_of_int (List.length q)) in

let remapp' = List.filter (function Void _ -> false |_ -> true) q in

dbgr := q;

let rec strength_reduce = function
| Asgn (Id c, 
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Id c, itm)) then_, List.map (fun itm -> Asgn (Id c, itm)) else_)
| oth -> oth in

let rec combiner = function
| [] -> []
| If_ (Dyadic (Vpieqop, a, b), c, d) ::
  If_ (Dyadic (Vpieqop, a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Dyadic (Vpieqop, a, b), c@c', d@d') :: []
| If_ (Dyadic _ as a, b, c) :: tl -> If_ ( a, combiner b, combiner c) :: combiner tl
| Seq (Block (Id blk, exp1) :: Block (Id blk', Dyadic (op, Id blk'', exp2)) :: tl) :: tl' when blk=blk' && blk'=blk'' ->
  combiner (Asgn(Id blk', Dyadic(op, exp1, exp2)) :: tl @ tl')
| Seq lst :: tl -> combiner (lst @ tl)
| Asgn (Update (Id dest, lfthi, lftlo, hi, lo), a) :: Asgn (Update (Id dest', rghthi, rghtlo, hi', lo'), b) :: tl
when dest=dest' && lfthi=hi && lftlo=rghthi+1 && rghtlo=lo && hi=hi' && lo=lo' -> Asgn(Id dest, Concat ( Vpiconcatop, a, b )) :: combiner tl
| Block(id, expr) :: tl -> Asgn(id, expr) :: combiner tl
| Asgn (_, Void 585) :: tl -> combiner tl
| hd :: tl -> hd :: combiner tl in

othremapp' := remapp';
othremapp'' := List.map strength_reduce !othremapp';
othremapp''' := combiner !othremapp'';
!declare_lst, !othremapp'''
end

let parseall f =
  let p,lst,lst' = parse f in p' := p;
  modlst := lst;
  modlst' := lst';
  let lst'' = if Array.length Sys.argv > 3 then lst else lst' in
  let remapp_lst = List.map (fun body -> crntmod := body; cnv_remapp body) lst''
  in remapp_lst

let _ = if Array.length Sys.argv > 1 then remap_lst := parseall Sys.argv.(1)

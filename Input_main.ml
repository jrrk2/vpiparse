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
  | Signed of remapp
  | Unsigned of remapp

type remap =
  | Invalid
  | Con of Hardcaml.Constant.t
  | Sig of Hardcaml.Signal.t
  | Sigs of Hardcaml.Signal.Signed.v
  | Alw of Hardcaml.Always.t
  | Var of Hardcaml.Always.Variable.t
  | Itm of (Hardcaml.Signal.t * Hardcaml.Always.t list)

type attr = {
  pass: bool;
  clock: string option;
  reset: string option;
}

let othasgn = ref Work
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
let othop = ref (Work, Invalid, Invalid)
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
Sig (relation lhs rhs)
else if wlhs < wrhs then
Sig (relation (uresize lhs wrhs) rhs)
else if wlhs > wrhs then
Sig (relation lhs (uresize rhs wlhs))
else failwith "relational"

let relationalc relation lhs rhs = Sig (relation lhs rhs)
let relationalc' relation lhs rhs = Sigs (relation lhs rhs)

let relational' relation lhs rhs =
let wlhs = signedwidth lhs in
let wrhs = signedwidth rhs in
if wlhs = wrhs then
Sigs (relation lhs rhs)
else if wlhs < wrhs then
Sigs (relation (Signed.resize lhs wrhs) rhs)
else if wlhs > wrhs then
Sigs (relation lhs (Signed.resize rhs wlhs))
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

let summary = function
      | Con x -> Con x
      | Var _ -> Var (Always.Variable.wire ~default:(Signal.zero 1))
      | Sig _ -> Sig (Signal.zero 1)
      | Sigs _ -> Sigs (signed_zero 1)
      | Itm _ 
      | Alw _ | Invalid -> failwith "summary"

let cnv (modnam, p) =
begin
let declare_lst = ref [] in
let exists k = List.mem_assoc k !declare_lst in
let add_decl k x = function
| Width(hi,lo,signed) as wid ->
  if exists k then print_endline (k^": redeclared") else
  begin
    declare_lst := (k, x) :: !declare_lst;
    othdecl := (k, (wid, summary x)) :: !othdecl
  end
| oth -> otht := Some oth; failwith "add_decl" in

let find_decl k = List.assoc k !declare_lst in
let iter_decl fn = List.iter (fun (k,x) -> fn k x) !declare_lst in

let unsigned x = Signed.to_signal x in

let sig' = function
| Con x -> Signal.of_constant x
| Sig x -> x
| Sigs x -> unsigned x
| Var x -> x.value
| oth -> othr := oth; failwith "sig'" in

let alw' = function
| Alw x -> x
| oth -> othr := oth; failwith "alw'" in

let var' = function
| Var x -> x
| oth -> othr := oth; failwith "var'" in

let declare_input port = function
| Width(hi,lo,signed) as w ->
  if false then print_endline port;
  let s = Signal.input port (hi-lo+1) in
  add_decl port (if signed then Sigs (Signed.of_signal s) else Sig s) w
| oth -> otht := Some oth; failwith "declare_input" in

let logf = open_out "logfile.txt" in

let declare_wire = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, (Width(hi,lo,signed) as wid)))), STRING wire) -> let wid' = hi-lo+1 in
  output_string logf ("wire "^wire^"\n");
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) wid
| TUPLE3 (Work, STRING wire, (Width(hi,lo,signed) as wid)) -> let wid' = hi-lo+1 in
  output_string logf ("wire "^wire^"\n");
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) wid
| TUPLE5 (Part_select, STRING wire,
      TUPLE3 (Vpiuintconst, Int lft, Int _),
      TUPLE3 (Vpiuintconst, Int rght, Int _), (Width(hi, lo, signed) as wid)) -> let wid' = hi-lo+1 in
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) wid
| TUPLE3 (Bit_select, STRING wire, TUPLE3 (Vpiuintconst, Int lft, Int _)) -> let wid' = 1 in
  output_string logf ("wire "^wire^"\n");
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) (Width(0,0,false))
| oth -> othrw := Some oth; failwith "declare_wire" in

let declare_orphan = function
| TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
					     TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, (Width(hi, lo, signed) as wid))))) ->
  let wire = List.hd (List.rev (List.map (function STRING s -> s | oth -> failwith "declare_wire'") pth)) in
  let wid' = hi-lo+1 in
  output_string logf ("orphan "^wire^"\n");
 let s = Signal.input wire (hi-lo+1) in
  add_decl wire (if signed then Sigs (Signed.of_signal s) else Sig s) wid
| oth -> othrw := Some oth; failwith "declare_orphan" in

let clock attr = match attr.clock with
| Some clk -> if true then print_endline ("clock: "^clk); sig' (find_decl clk)
| None -> failwith ("failed to identify clock for declare_reg") in

let r_sync attr = match attr.reset with
  | Some rst -> let reset = sig' (find_decl rst) in
    let clock = clock attr in
    Reg_spec.create ~clock ~reset ()
  | None -> 
    let clock = clock attr in
    Reg_spec.create ~clock () in

let declare_reg attr = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, (Width(hi,lo, signed) as wid)))), STRING reg) -> if exists reg then () else
  begin
  let width = hi-lo+1 in let r_sync = r_sync attr in
  if true then print_endline (reg^": "^string_of_int width);
  add_decl reg (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width)) wid;
  end
| TUPLE5 (Part_select, STRING reg,
      TUPLE3 (Vpiuintconst, Int lft, Int _),
      TUPLE3 (Vpiuintconst, Int rght, Int _), (Width(hi, lo, signed) as wid)) -> let wid' = hi-lo+1 in let r_sync = r_sync attr in
  add_decl reg (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid')) wid;
| TUPLE3 (Bit_select, STRING reg, TUPLE3 (Vpiuintconst, Int lft, Int _)) -> let wid' = 1 in let r_sync = r_sync attr in
  add_decl reg (Var (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid')) (Width(0,0,false))
| oth -> othrw := Some oth; failwith "declare_reg" in

let rec traverse (attr:attr) = function
| TUPLE2 (Vpimodule, TLIST lst) -> List.iter (function
    | Enum_typespec -> ()
    | STRING nam -> ()
    | TUPLE2 (Vpiinstance, STRING inst) -> ()
    | TUPLE2 (Parameter, (Work|STRING _)) -> ()
    | TUPLE3 ((Vpiinput|Vpioutput), STRING port, Width (hi, lo, signed)) -> ()
    | Vpideffile | Vpideflineno | Always | Cont_assign -> ()
    | TUPLE2 ((Vpimodule|Gen_scope_array|Cont_assign), _) -> ()
    | TUPLE2 (Enum_var, STRING _) -> ()
    | TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) ->
       print_endline ("Cont_assign_vpimodule: "^topmod^" "^dest);
       traverse attr lhs;
       traverse attr rhs;
       if not (attr.pass) then declare_wire lhs
    | TUPLE4 ((Vpialways|Vpinet|Vpireg),
          TUPLE2 (Vpitypespec,
            TUPLE3 (Ref_typespec,
              TLIST pth,
              TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))),
          STRING pin,
          TLIST pth') -> print_endline pin
    | oth -> otht := Some oth; failwith "Vpimodule") lst
| TUPLE2 (Enum_typespec, TLIST lst) -> List.iter (function
  | TUPLE3(STRING enum, Int n, Int width) -> add_decl enum (Con (Constant.of_int ~width n)) (Width(width-1, 0, false))
  | TUPLE2 (Vpitypespec,
      TUPLE3 (Ref_typespec, TLIST pth,
        TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))) -> ()
  | oth -> othrw := Some oth; failwith "enum") lst
| TUPLE4 (Vpireg, (TUPLE2 (Vpitypespec, _) as spec), _, _) -> if attr.pass then declare_orphan spec
| TUPLE4 ((Logic_net|Vpialways|Vpinet), TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth, TUPLE2 (Vpiactual, spec))),
      STRING nam, TLIST pth') -> ()
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
| TUPLE4 ((Assignment|Vpiblocking), optype, lhs, rhs) as asgn ->
   otht := Some asgn;
   traverse attr lhs;
   traverse attr rhs;
   if attr.pass then (match attr.clock with Some _ -> declare_reg attr lhs | None -> declare_wire lhs)
| TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) ->
   print_endline ("Cont_assign_other: "^topmod^" "^dest);
   traverse attr lhs;
   traverse attr rhs;
   if not (attr.pass) then declare_wire lhs
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, loc))), STRING clk) as edg)), stmt) ->
   othckedg := edg;
   if true then print_endline ("Always clk: "^clk);
   traverse {attr with clock=Some clk} stmt
| TUPLE3 (Always, TUPLE3(Vpieventorop,
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST ckpth, ckloc))), STRING clk) as ckedg)),
			 TUPLE2 (Vpiposedgeop, (TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST rstpth, rstloc))), STRING rst) as rstedg))), stmt) ->
   othckedg := ckedg;
   othrstedg := rstedg;
   if true then print_endline ("Always clk or rst: "^clk);
   traverse {attr with clock=Some clk; reset=Some rst} stmt
| TUPLE3 (Always, TUPLE2 (Vpieventorop, TLIST lst), stmt) -> List.iter (traverse attr) lst; traverse attr stmt
| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> traverse attr cond; traverse attr lhs; traverse attr rhs
| TUPLE3 (If_stmt, cond, lhs) -> traverse attr cond; traverse attr lhs
| TUPLE4 (If_else, cond, lhs, rhs) as tup -> 
    traverse attr cond;
    print_endline "lhs";
    otht := Some tup;
    traverse attr lhs;
    print_endline "rhs";
    traverse attr rhs
| TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop), rhs) -> traverse attr rhs
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop), lhs, rhs) -> traverse attr lhs; traverse attr rhs
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop), TLIST lst) -> List.iter (traverse attr) lst
| TUPLE5 (Part_select, STRING nam, lft, rght, Width(hi,lo, signed)) -> ()
| TUPLE4 (Ref_obj, STRING nam, TLIST pth, actual) -> traverse attr actual
| TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, loc))) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width _))), STRING wire) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Part_select, Work)), STRING wire) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, Enum_const), STRING e) -> if false then print_endline e
| TUPLE3 (Vpioutput, STRING port, Width(lft,rght, signed))	    -> ()
| TUPLE3 (Vpiinput, STRING port, (Width(lft,rght, signed) as wid))  -> if not attr.pass then declare_input port wid
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
      | TUPLE2 (Case_item, stmt) ->
        traverse attr stmt;
        otht := Some stmt;
      | oth -> otht := Some oth; failwith "traverse case") lst
| TUPLE3 (Bit_select, STRING nam, idx) -> traverse attr idx
| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> List.iter (function
    | TUPLE3 (Port, LOC _, conn) -> if not attr.pass then declare_wire conn
    | TUPLE3 (Work, STRING _, Width _) as conn -> if not attr.pass then declare_wire conn
    | TUPLE2 (Port, LOC _) -> ()
    | oth -> otht := Some oth; failwith "traverse ref mod") lst
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned")) -> ()
| TUPLE2 (Initial, stmts) -> ()
| TUPLE2 (Vpigenstmt, stmts) -> ()
| TUPLE2 (Enum_var, STRING _) -> ()
| Enum_typespec | Cont_assign | Constant -> ()
| oth -> otht := Some oth; failwith "traverse failed with otht" in

let _ = List.iter (traverse {pass=false; clock=None; reset=None}) p in
let _ = List.iter (traverse {pass=true; clock=None; reset=None}) p in

let edgstmt = ref Work in
let edgstmt' = ref (Void 0) in
let conlst = ref [] in
let conlst' = ref [] in
let othfunc = ref "" in

let sys_func x = function
| "$signed" -> Signed x
| "$unsigned" -> Unsigned x
| oth -> othfunc := oth; failwith "sys_func" in

let rec (remapp:token->remapp) = function
| TUPLE2 (Vpimodule, TLIST lst) -> List.iter (function
    | Enum_typespec -> ()
    | STRING nam -> ()
    | TUPLE2 (Vpiinstance, STRING inst) -> ()
    | TUPLE2 (Parameter, (Work|STRING _)) -> ()
    | TUPLE3 ((Vpiinput|Vpioutput), STRING port, Width (hi, lo, signed)) -> ()
    | Vpideffile | Vpideflineno | Always | Cont_assign -> ()
    | TUPLE2 ((Vpimodule|Gen_scope_array|Cont_assign), _) -> ()
    | TUPLE2 (Enum_var, STRING _) -> ()
    | TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) -> print_endline ("Cont_assign_vpimodule_remapp: "^topmod^" "^dest);
    | TUPLE4 ((Vpialways|Vpinet|Vpireg),
          TUPLE2 (Vpitypespec,
            TUPLE3 (Ref_typespec,
              TLIST pth,
              TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))),
          STRING pin,
          TLIST pth') -> ()
    | oth -> otht := Some oth; failwith "Vpimodule remapp") lst; Void 495

| TUPLE2 (Enum_typespec, TLIST lst) -> List.iter (function
  | TUPLE3(STRING enum, Int n, Int width) -> add_decl enum (Con (Constant.of_int ~width n)) (Width(width-1, 0, false))
  | TUPLE2 (Vpitypespec,
      TUPLE3 (Ref_typespec, TLIST pth,
        TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))) -> ()
  | oth -> othrw := Some oth; failwith "enum") lst; Void 444
| TUPLE4 (Vpireg, TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
          TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Width (hi, lo, signed))))), STRING nam, TLIST pth') -> Void 446
| TUPLE4 ((Logic_net|Vpialways|Vpinet), TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth, TUPLE2 (Vpiactual, spec))),
      STRING nam, TLIST pth') -> Void 448

| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2(TLIST pth, Width(hi,lo, signed)))), STRING wire) ->
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
| TUPLE5 (Cont_assign, lhs, rhs, STRING topmod, STRING dest) -> print_endline ("Cont_assign_remapp: "^topmod^" "^dest);
   let rhs = (remapp rhs) in
   let lhs = match remapp lhs with
   | Selection(id, lft, rght, hi, lo) -> Update(id, lft, rght, hi, lo)
   | Id _ as id -> id
   | Bitsel (Id _ as id, Dec (sel, _)) -> let ix = int_of_string sel in Update(id, ix, ix, ix, ix)
   | oth -> othlhs := oth; failwith "remapp cont_assign failed with othlhs"; in
   Asgn (lhs, rhs)
| TUPLE2 (Always,
   TUPLE3 (Begin, TLIST pth, TLIST (Vpiparent:: TLIST [] :: stmtlst))) -> Seq (List.map remapp stmtlst)
| TUPLE3 (Always, Vpiassignstmt, stmt) -> remapp stmt
| TUPLE3 (Always, TUPLE2 (Vpieventorop, TLIST lst), stmt) -> Seq (List.map remapp lst)

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
| TUPLE5 (Part_select, STRING nam, TUPLE3 (Vpiuintconst, Int lft, Int _), TUPLE3 (Vpiuintconst, Int rght, Int _), Width(hi,lo, signed)) ->
   Selection(Id nam, lft, rght, hi, lo)
| TUPLE3 (Vpioutput, STRING port, Width(hi,lo, signed))	    -> Void 382
| TUPLE3 (Vpiinput, STRING port, Width(hi,lo, signed))	    -> Void 383
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
    Case (cond', List.map (function
			   | TUPLE3 (Case_item, cons, stmt) -> Item (cond', remapp cons, remapp stmt)
			   | TUPLE2 (Case_item, stmt) -> Item (Void 536, Void 536, remapp stmt)
			   | oth -> otht := Some oth; failwith "Case") lst)
| TUPLE3 (Bit_select, STRING nam, idx) -> if exists nam then Bitsel (Id nam, remapp idx) else Void 399

| TUPLE4 (Ref_module, STRING nam1, STRING nam2, TLIST lst) -> Void 401
| TUPLE3 (Sys_func_call, actual, STRING ("$signed"|"$unsigned" as func)) -> sys_func (remapp actual) func
| TUPLE2 (Initial, stmts) -> Void 403
| TUPLE2 (Vpigenstmt, stmts) -> Void 404
| Vpiparent -> Void 405
| TUPLE6 (For_stmt, TLIST [], _, _, _, _) -> Void 406
| TUPLE2 (TUPLE2 (Vpiactual, Enum_const), STRING e) -> Void 543
| TUPLE4 (Ref_obj, STRING nam, TLIST pth, actual) -> Id nam
| TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, loc))) -> Void 550
| TUPLE2 (Enum_var, STRING _) -> Void 584
| Enum_typespec | Cont_assign | Constant -> Void 585
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
| If_ (Dyadic _ as a, b, c) :: tl -> If_ ( a, combiner b, combiner c) :: combiner tl
| Seq (Block (Id blk, exp1) :: Block (Id blk', Dyadic (op, Id blk'', exp2)) :: tl) :: tl' when blk=blk' && blk'=blk'' ->
  combiner (Asgn(Id blk', Dyadic(op, exp1, exp2)) :: tl @ tl')
| Seq lst :: tl -> combiner (lst @ tl)
| Asgn (Update (Id dest, lfthi, lftlo, hi, lo), a) :: Asgn (Update (Id dest', rghthi, rghtlo, hi', lo'), b) :: tl
when dest=dest' && lfthi=hi && lftlo=rghthi+1 && rghtlo=lo && hi=hi' && lo=lo' -> Asgn(Id dest, Concat ( Vpiconcatop, a, b )) :: combiner tl
| Block(id, expr) :: tl -> Asgn(id, expr) :: combiner tl
| Asgn (_, Void 585) :: tl -> combiner tl
| hd :: tl -> hd :: combiner tl in

let remapp'' = List.map strength_reduce remapp' in
let remapp''' = combiner remapp'' in

let unimp = let seen = ref [] in fun lbl lhs rhs -> 
if not (List.mem lbl !seen) then print_endline ("unimp: "^lbl); seen := lbl :: !seen; lhs ^: rhs in

let unimps = let seen = ref [] in fun lbl lhs rhs -> let open Signed in
if not (List.mem lbl !seen) then print_endline ("unimps: "^lbl); seen := lbl :: !seen; of_signal (to_signal lhs ^: to_signal rhs) in

let lognegate fn = fun lhs rhs -> fn lhs (~: rhs) in

let signed_relational x = let open Signed in match x with
|Vpieqop -> (==:) 
|Vpineqop -> (<>:) 
|Vpiltop -> (<:) 
|Vpileop -> (<=:) 
|Vpigeop -> (>=:) 
|Vpigtop -> (>:) 
|Vpilshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift sll (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Vpirshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift srl (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Vpiarithlshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift sll (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Vpiarithrshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.log_shift sra (Signed.to_signal rhs) (Signed.to_signal lhs)))
|Vpiaddop -> (+:)
|Vpisubop -> (-:)
|Vpimultop -> mult_wallace_signed
|Vpidivop -> unimps "div"
|Vpimodop -> unimps "mod"
|Vpipowerop -> unimps "power"
|Vpilogandop -> (fun lhs rhs -> of_signal (to_signal lhs &&: to_signal rhs))
|Vpilogorop -> (fun lhs rhs -> of_signal (to_signal lhs ||: to_signal rhs))
|Vpibitandop -> (fun lhs rhs -> of_signal (to_signal lhs &: to_signal rhs))
|Vpibitorop -> (fun lhs rhs -> of_signal (to_signal lhs |: to_signal rhs))
|Vpibitxorop -> (fun lhs rhs -> of_signal (to_signal lhs ^: to_signal rhs))
|Vpibitxnorop -> (fun lhs rhs -> of_signal ( ~: (to_signal lhs ^: to_signal rhs)))
|oth -> otht := Some oth; failwith "signed_relational" in

let unsigned_relational = function
|Vpieqop -> (==:) 
|Vpineqop -> (<>:) 
|Vpiltop -> (<:) 
|Vpileop -> (<=:) 
|Vpigeop -> (>=:) 
|Vpigtop -> (>:)
|Vpilshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpirshiftop -> (fun lhs rhs -> Signal.log_shift srl rhs lhs)
|Vpiarithlshiftop -> (fun lhs rhs -> Signal.log_shift sll rhs lhs)
|Vpiarithrshiftop -> (fun lhs rhs -> Signal.log_shift sra rhs lhs)
|Vpiaddop -> (+:)
|Vpisubop -> (-:)
|Vpimultop -> mult_wallace
|Vpidivop -> unimp "div"
|Vpimodop -> unimp "mod"
|Vpipowerop -> unimp "power"
|Vpilogandop -> (&&:) 
|Vpilogorop -> (||:) 
|Vpibitandop -> (&:) 
|Vpibitorop -> (|:) 
|Vpibitxorop -> (^:) 
|Vpibitxnorop -> lognegate (^:)
|oth -> otht := Some oth; failwith "unsigned_relational" in

let unsigned_relationalc = function
|Vpilshiftop -> Signal.sll
|Vpirshiftop -> Signal.srl
|Vpiarithrshiftop -> Signal.sra
|oth -> otht := Some oth; failwith "unsigned_relationalc" in

let signed_relationalc x = let open Signed in match x with
|Vpilshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.sll (Signed.to_signal lhs) rhs))
|Vpirshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.srl (Signed.to_signal lhs) rhs))
|Vpiarithrshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.sra (Signed.to_signal lhs) rhs))
|oth -> otht := Some oth; failwith "unsigned_relationalc" in

let detect_dyadic = function
| op,Sig lhs, Sig rhs -> relational (unsigned_relational op) lhs rhs
| op,Sigs lhs, Sig rhs -> relational (unsigned_relational op) (Signed.to_signal lhs) rhs
| op,Sig lhs, Sigs rhs -> relational (unsigned_relational op) lhs (Signed.to_signal rhs)
| op,Sigs lhs, Sigs rhs -> relational' (signed_relational op) lhs rhs
| (Vpisubop as op), Sig lhs, Con rhs -> relational (unsigned_relational op) lhs (Signal.of_constant rhs)
| op, Con lhs, Sig rhs -> relational (unsigned_relational op) (Signal.of_constant lhs) rhs
| op, Con lhs, Sigs rhs -> relational' (signed_relational op) (Signed.of_signal (Signal.of_constant lhs)) rhs
| (Vpirshiftop|Vpiarithrshiftop as op), Sig lhs, Con rhs -> relationalc (unsigned_relationalc op) lhs (Constant.to_int rhs)
| op,Sig lhs, Con rhs -> relational (unsigned_relational op) lhs (Signal.of_constant rhs)
| op,Sigs lhs, Con rhs -> relationalc' (signed_relationalc op) lhs (Constant.to_int rhs)
| (Vpilshiftop|Vpirshiftop|Vpiarithrshiftop as op), Var lhs, Con rhs -> relationalc (unsigned_relationalc op) lhs.value (Constant.to_int rhs)
| op, Var lhs, Con rhs -> relational (unsigned_relational op) lhs.value (Signal.of_constant rhs)
| (Vpilshiftop|Vpirshiftop|Vpiarithrshiftop as op), Con lhs, Con rhs -> relationalc (unsigned_relationalc op) (Signal.of_constant lhs) (Constant.to_int rhs)
| op,lhs,rhs -> othop := (op, summary lhs, summary rhs); failwith "detect_dyadic" in

(*
let unimplu = let seen = ref [] in fun lbl rhs ->
if not (List.mem lbl !seen) then print_endline ("unimplu: "^lbl); seen := lbl :: !seen; ~: rhs in
*)

let fold' fn rhs = let expl = List.init (width rhs) (bit rhs) in List.fold_left fn (List.hd expl) (List.tl expl) in

let remapop' = function
|Vpiplusop -> (fun rhs -> rhs)
|Vpiminusop -> arithnegate
|Vpiunaryandop -> fold' (&:)
|Vpiunarynandop -> fold' (lognegate (&:))
|Vpiunaryorop -> fold' (|:)
|Vpiunarynorop -> fold' (lognegate (|:))
|Vpiunaryxorop -> fold' (^:)
|Vpiunaryxnorop -> fold' (lognegate (^:))
|Vpibitnegop -> (~:)
|Vpinotop -> (~:)
|oth -> otht := Some oth; failwith "remapop'" in

let rec (remap:remapp->remap) = function
| Id wire ->
if exists wire then find_decl wire else Invalid
| Unary (op, rhs) -> Sig (remapop' op (sig' (remap rhs)))
| Dyadic (op, lhs, rhs) -> let lhs = remap lhs and rhs = remap rhs in detect_dyadic (op,lhs, rhs)
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
| Signed x -> (match remap x with Sig x -> Sigs (Signed.of_signal x) | Sigs _ as x -> x | _ -> failwith "Signed")
| Unsigned x -> (match remap x with Sigs x -> Sig (Signed.to_signal x) | Sig _ as x -> x | _ -> failwith "Unsigned")
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
        | Itm v -> ()
        | Con v -> ()
        | Alw v -> ()
        | Sig v -> alst := (k, Sig v) :: !alst
        | Sigs v -> alst := (k, Sigs v) :: !alst
        | Invalid -> ());
Hardcaml.Rtl.output Verilog (Hardcaml.Circuit.create_exn ~name:modnam !oplst);
if Array.length Sys.argv > 2 then eqv Sys.argv.(2) modnam
end

let modlst = ref []
let modlst' = ref []
let crntmod = ref ("", [])

let parseall f = let p,lst,lst' = parse f in p' := p; modlst := lst; modlst' := lst';
let lst'' = if Array.length Sys.argv > 3 then lst else lst' in
List.iter (fun body -> crntmod := body; cnv body) lst''

let _ = if Array.length Sys.argv > 1 then parseall Sys.argv.(1)

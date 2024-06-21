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

open Input
open Input_types
open Dump_types
open Hardcaml
open Always
open Signal

let othalwystran = ref None
let othasgn = ref Work
let othr = ref Invalid
let othv = ref None
let othdecl' = ref ("", Work)
let othio = ref None
let othrw = ref Work
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
let othckedg = ref Work
let othrstedg = ref Work
let othop = ref (Work, Invalid, Invalid)

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
      | Itm _ -> Itm (Signal.zero 1, [])
      | Alw _ -> Alw (Always.switch (Signal.zero 1) [])
      | Invalid -> Invalid

let othtok = ref INVALID

let arithopvpi = function
| Aadd -> Vpiaddop
| Asub -> Vpisubop
| Amul -> Vpimultop
| Amuls -> Vpimultop
| Aunknown -> Vpirhs

let logopvpi = function
| Lunknown -> Vpirhs
| Land -> Vpibitandop
| Lredand -> Vpilogandop
| Lor -> Vpibitorop
| Lredor -> Vpilogorop
| Lxor -> Vpibitxorop
| Lxnor -> Vpibitxnorop
| Lredxor -> Vpibitxorop
| Lredxnor -> Vpibitxnorop
| Lshiftl -> Vpilshiftop
| Lshiftr -> Vpirshiftop
| Lshiftrs -> Vpiarithrshiftop

let cnv_op oplst k = function
        | Var v -> oplst := output k v.value :: !oplst
        | Itm v -> ()
        | Con v -> ()
        | Alw v -> ()
        | Sig v -> ()
        | Sigs v -> ()
        | Invalid -> ()

let cnv (modnam, (_, modul)) =
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

let declare_input port = function
| Width(hi,lo,signed) as w ->
  let s = Signal.input port (hi-lo+1) in
  add_decl port (if signed then Sigs (Signed.of_signal s) else Sig s) w
| oth -> otht := Some oth; failwith "declare_input" in

let declare_wire wire = function
| Width(hi,lo,signed) as wid -> let wid' = hi-lo+1 in
  add_decl wire (Var (Always.Variable.wire ~default:(Signal.zero wid'))) wid
| oth -> otht := Some oth; failwith "declare_wire" in

let iofunc declare_input declare_wire = function
   | (io, ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", [])) -> declare_input io (Width(0,0,false))
   | (io, ("", (BASDTYP, "logic", TYPRNG (HEX hi, HEX lo), []), Dinput, "logic", [])) -> declare_input io (Width(hi,lo,false))
   | (io, ("", (BASDTYP, "logic", TYPRNG (HEX hi, HEX lo), []), Doutput, "logic", [])) -> declare_wire io (Width(hi,lo,false))
   | (io, ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", [])) -> declare_wire io (Width(0,0,false))
   | oth -> othio := Some oth; failwith "othio" in

let vfunc declare_input declare_wire = function
   | (v, ("", (BASDTYP, "reg", TYPNONE, []), "reg", (UNKDTYP, "", TYPNONE, []))) -> declare_wire v (Width(0,0,false))
   | (v, ("", (BASDTYP, "reg", TYPRNG (HEX hi, HEX lo), []), "reg", (UNKDTYP, "", TYPNONE, []))) -> declare_wire v (Width(hi,lo,false))
   | oth -> othv := Some oth; failwith "othv" in

let tran_search declare_input declare_wire id =
  if List.mem_assoc id !(modul.io) then iofunc declare_input declare_wire (id, List.assoc id !(modul.io))
  else if List.mem_assoc id !(modul.v) then vfunc declare_input declare_wire (id, List.assoc id !(modul.v))
  else print_endline (id^": not found") in

let find_decl k = if not (List.mem_assoc k !declare_lst) then tran_search declare_input declare_wire k; List.assoc k !declare_lst in
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

let declare_orphan = function
| TUPLE2 (Vpitypespec, TUPLE3 (Ref_typespec, TLIST pth,
					     TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, (Width(hi, lo, signed) as wid))))) ->
  let wire = List.hd (List.rev (List.map (function Input.STRING s -> s | oth -> failwith "declare_orphan") pth)) in
  let wid' = hi-lo+1 in
 let s = Signal.input wire (hi-lo+1) in
  add_decl wire (if signed then Sigs (Signed.of_signal s) else Sig s) wid
| oth -> othrw := oth; failwith "declare_orphan" in

let declare_reg attr reg =
  if exists reg then () else
  begin
  let wid' _ = function Width(hi,lo,signed) as wid -> let width = hi-lo+1 in
  let r_sync = match attr.r_sync with Some x -> x | None -> failwith "r_sync" in
  let enable = attr.enable in
  if false then print_endline (reg^": "^string_of_int width);
  add_decl reg (Var (Always.Variable.reg ~enable r_sync ~width)) wid
  | _ -> failwith "declare_reg" in
  tran_search wid' wid' reg;
  end in

let rec tranitm attr = function
| UNKNOWN -> Void 0
| VRF (id, _, []) -> if attr.dest then declare_reg attr id; Ident id
| ASGN (bool, str2, src::dst::[]) -> Asgn((tranitm {attr with dest=true}) dst, (tranitm attr) src)
| ASGN (bool, str2, rw_lst) -> failwith "ASGN"
| ARITH (arithop, lft::rght::[]) -> Dyadic(arithopvpi arithop, (tranitm attr) lft, (tranitm attr) rght)
| LOGIC (logop, lft::rght::[]) -> Dyadic(logopvpi logop, (tranitm attr) lft, (tranitm attr) rght)
| IF (str1, cond::then_::else_::[]) -> If_ ((tranitm attr) cond, (tranitm attr) then_::[], (tranitm attr) else_::[])
| IF (str1, rw_lst) -> failwith "IF"
| CNST (w, HEX n) -> Hex (string_of_int n, w)
(*
| CNST (_, BIN n) -> (String.make 1 n)
| CNST (_, SHEX n) -> (string_of_int n)
| CNST (w, BIGINT n) -> hex_of_bigint w n
| CNST (_, STRING s) -> String.map (function '0'..'9' | 'a'..'f' | 'A'..'F' as ch -> ch | oth -> '_') s
| CNST (_, FLT f) -> (string_of_float f)
| CNST (_, ERR err) -> failwith "ERR"
*)
| CNST (_, err) -> failwith "CNST"
| XML (rw_lst) -> failwith "XML"
| EITM (str1, str2, str3, int2, rw_lst) -> failwith "EITM"
| IO (str1, str2lst, typ2, dirop, str3, rw_lst) -> failwith "IO"
| VAR (str1, str2lst, typ2, str3) -> failwith "VAR"
| IVAR (str1, str2, typ2, rw_lst, int3) -> failwith "IVAR"
| VRF (str1, typ', rw_lst) -> failwith "VRF"
| TYP (idx, (typenc, str1, typmap, typ_lst)) -> failwith "TYP"
| FNC (str1, str2, typ2, rw_lst) -> failwith "FNC"
| TASKDEF (str1, str2, rw_lst) -> failwith "TASKDEF"
| TASKRF (str1, str2, rw_lst) -> failwith "TASKRF"
| INST (str1, kind, str2lst, (str3, rw_lst)) -> failwith "INST"
| SFMT (str1, rw_lst) -> failwith "SFMT"
| SYS (str1, str2, rw_lst) -> failwith "SYS"
| TPLSRGS (str1, str2, int2, rw_lst) -> failwith "TPLSRGS"
| VPLSRGS (str1, int2, rw_lst) -> failwith "VPLSRGS"
| PORT (str1, str2, dirop, rw_lst) -> failwith "PORT"
| CA (str1, rw_lst) -> failwith "CA"
| UNRY (unaryop, rw_lst) -> failwith "UNRY"
| SEL (str1, rw_lst) -> failwith "SEL"
| ASEL (rw_lst) -> failwith "ASEL"
| SNITM (str1, rw_lst) -> failwith "SNITM"
| CMP (cmpop, rw_lst) -> failwith "CMP"
| FRF (str1, str2, rw_lst) -> failwith "FRF"
| XRF (str1, str2, str3, str4, dirop) -> failwith "XRF"
| PKG (str1, str2, rw_lst) -> failwith "PKG"
| CAT (str1, rw_lst) -> failwith "CAT"
| CPS (str1, rw_lst) -> failwith "CPS"
| CND (str1, rw_lst) -> failwith "CND"
| REPL (str1, int2, rw_lst) -> failwith "REPL"
| MODUL (str1, str2, rw_lst, tmp) -> failwith "MODUL"
| BGN (None, rw_lst) -> failwith "BGN"
| BGN (Some str1, rw_lst) -> failwith "BGN"
| RNG (rw_lst) -> failwith "RNG"
| ALWYS (str1, rw_lst) -> failwith "ALWYS"
| SNTRE (rw_lst) -> failwith "SNTRE"
| INIT (str1, str2, rw_lst) -> failwith "INIT"
| IRNG (str1, rw_lst) -> failwith "IRNG"
| IFC (str1, str2, rw_lst) -> failwith "IFC"
| IMP (str1, str2, rw_lst) -> failwith "IMP"
| IMRF (str1, str2, dir, rw_lst) -> failwith "IMRF"
| JMPL (str1, rw_lst) -> failwith "JMPL"
| JMPG (str1, rw_lst) -> failwith "JMPG"
| JMPBLK (str1, rw_lst) -> failwith "JMPBLK"
| CS (str1, rw_lst) -> failwith "CS"
| CSITM (str1, rw_lst) -> failwith "CSITM"
| WHL (rw_lst) -> failwith "WHL"
| FORSTMT _ -> failwith "FORSTMT"
| ARG (rw_lst) -> failwith "ARG"
| DSPLY (str1, str2, rw_lst) -> failwith "DSPLY"
| FILS (str1, rw_lst) -> failwith "FILS"
| FIL (str1, str2) -> failwith "FIL"
| NTL (rw_lst) -> failwith "NTL"
| CELLS (rw_lst, attr) -> failwith "CELLS"
| CELL (str1, str2, str3, str4, rw_lst) -> failwith "CELL"
| POSPOS (str1, str2) -> failwith "POSPOS"
| POSNEG (str1, str2) -> failwith "POSNEG"
| NEGNEG (str1, str2) -> failwith "NEGNEG"
| POSEDGE (str1) -> failwith "POSEDGE"
| NEGEDGE (str1) -> failwith "NEGEDGE"
| COMB -> failwith "COMB"
| MODPORTFTR (str1, str2) -> failwith "MODPORTFTR"
| TYPETABLE arr -> failwith "TYPETABLE"
| TIM _ -> failwith "TIM"
| SCOPE tid -> failwith "SCOPE"
| ITM _ -> failwith "ITM"
| CONTAINER (itms, SCOPE top) -> failwith "CONTAINER" in

let edgstmt = ref Work in
let edgstmt' = ref (Void 0) in
let conlst = ref [] in
let conlst' = ref [] in
let othfunc = ref "" in

let sys_func x = function
| "$signed" -> Signed x
| "$unsigned" -> Unsigned x
| oth -> othfunc := oth; failwith "sys_func" in

let alwystran = List.flatten (List.map (function
  | ("", COMB, (SNTRE [] :: lst)) -> let attr = {enable=Signal.vdd; r_sync=None; dest=false} in List.map (tranitm attr) lst
  | ("", POSEDGE clk, lst) -> let attr = {enable=Signal.vdd; r_sync=Some (Reg_spec.create ~clock:(sig' (find_decl clk)) ()); dest=false} in List.map (tranitm attr) lst
  | ("", POSPOS (clk, rst), lst) -> let attr = {enable=Signal.vdd; r_sync=Some (Reg_spec.create ~clock:(sig' (find_decl clk)) ~reset:(sig' (find_decl rst)) ()); dest=false} in List.map (tranitm attr) lst
  | oth -> othalwystran := Some oth; failwith "alwystran") !(modul.alwys)) in

let _ = List.iter (fun (io,_ as args) -> if not (exists io) then iofunc declare_input declare_wire args) !(modul.io) in

let _ = List.iter (fun (v,_ as args) -> if not (exists v) then vfunc declare_input declare_wire args) !(modul.v) in

let remapp' = List.filter (function Void _ -> false |_ -> true) alwystran in

let rec strength_reduce = function
| Asgn (Ident c, 
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Ident c, itm)) then_, List.map (fun itm -> Asgn (Ident c, itm)) else_)
| oth -> oth in

let rec combiner = function
| [] -> []
| If_ (Dyadic (Vpieqop, a, b), c, d) ::
  If_ (Dyadic (Vpieqop, a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Dyadic (Vpieqop, a, b), c@c', d@d') :: []
| If_ (Dyadic _ as a, b, c) :: tl -> If_ ( a, combiner b, combiner c) :: combiner tl
| Seq (Block (Ident blk, exp1) :: Block (Ident blk', Dyadic (op, Ident blk'', exp2)) :: tl) :: tl' when blk=blk' && blk'=blk'' ->
  combiner (Asgn(Ident blk', Dyadic(op, exp1, exp2)) :: tl @ tl')
| Seq lst :: tl -> combiner (lst @ tl)
| Asgn (Update (Ident dest, lfthi, lftlo, hi, lo), a) :: Asgn (Update (Ident dest', rghthi, rghtlo, hi', lo'), b) :: tl
when dest=dest' && lfthi=hi && lftlo=rghthi+1 && rghtlo=lo && hi=hi' && lo=lo' -> Asgn(Ident dest, Concat ( Vpiconcatop, a, b )) :: combiner tl
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
| op, Var lhs, Sig rhs -> relational (unsigned_relational op) lhs.value rhs
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
| Ident wire ->
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
let oplst = ref [] in
List.iter (function
    | (io, ("", (BASDTYP, "logic", _, []), Dinput, "logic", [])) -> ()
    | (io, ("", (BASDTYP, "logic", TYPRNG (HEX hi, HEX lo), []), Doutput, "logic", [])) ->
       cnv_op oplst io (find_decl io)
    | oth -> othio := Some oth; failwith "othio'") !(modul.io);

Hardcaml.Rtl.output Verilog (Hardcaml.Circuit.create_exn ~name:modnam !oplst);
end

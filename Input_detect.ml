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
open Input_rewrite
open Input_remapp

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
let othckedg = ref Work
let othrstedg = ref Work
let othop = ref (Work, (Void 0), (Void 0))
let othrel = ref (Remap (Void 0))
let rec log2 n = if n <= 1 then 0 else 1 + log2 (n/2)

let exact_log2 n = 1 lsl (log2 n) = n

let (widthsig:signal->int) = function
| oth -> failwith "widthsig"

let resizepp rhs siz = failwith "resizepp"

let signed_resizepp rhs siz = failwith "signed_resizepp"

let arithnegate rhs = ANeg rhs
let signedwidthsig rhs = failwith "signedwidth"
let signednegate rhs = SNeg rhs

let mux2 cond lst = MUX2 (cond,List.map (fun itm -> itm) lst)
let sub lhs rhs = SUB (lhs,rhs)
let add lhs rhs = ADD (lhs,rhs)

let mux2' cond lhs rhs =
let wlhs = widthsig lhs in
let wrhs = widthsig rhs in
if wlhs = wrhs then
mux2 cond [lhs; rhs]
else if wlhs < wrhs then
mux2 cond [(resizepp lhs wrhs); rhs]
else if wlhs > wrhs then
mux2 cond [lhs; (resizepp rhs wlhs)]
else failwith "mux2'"

let sub' lhs rhs =
let wlhs = widthsig lhs in
let wrhs = widthsig rhs in
if wlhs = wrhs then
sub lhs rhs
else if wlhs < wrhs then
sub (resizepp lhs wrhs) rhs
else if wlhs > wrhs then
sub lhs (resizepp rhs wlhs)
else failwith "sub'"

let add' lhs rhs =
let wlhs = widthsig lhs in
let wrhs = widthsig rhs in
if wlhs = wrhs then
add lhs rhs
else if wlhs < wrhs then
add (resizepp lhs wrhs) rhs
else if wlhs > wrhs then
add lhs (resizepp rhs wlhs)
else failwith "mult'"

let relation lhs rhs = failwith "relation"

let relational relation lhs rhs =
let wlhs = widthsig lhs in
let wrhs = widthsig rhs in
if wlhs = wrhs then
Relation (relation, lhs, rhs)
else if wlhs < wrhs then
Relation (relation, (resizepp lhs wrhs), rhs)
else if wlhs > wrhs then
Relation (relation, lhs, (resizepp rhs wlhs))
else failwith "relational"

let relationalc relation lhs rhs = Sigpp (relation lhs rhs)
let relationalc' relation lhs rhs = Sigspp (relation lhs rhs)

let relational' relation lhs rhs =
let wlhs = signedwidthsig lhs in
let wrhs = signedwidthsig rhs in
if wlhs = wrhs then
Relations (relation, lhs, rhs)
else if wlhs < wrhs then
Relations (relation, (signed_resizepp lhs wrhs), rhs)
else if wlhs > wrhs then
Relations (relation, lhs, (signed_resizepp rhs wlhs))
else failwith "relational'"

let othsel = ref None

let mux' sel inputs =
let wid = widthsig inputs in
othsel := Some sel;
print_endline ("mux width: "^string_of_int wid);
mux2 sel (List.init wid (fun ix -> BITSEL (inputs, ix)))

let assign' lhs rhs =
let wlhs = widthsig lhs in
let wrhs = widthsig rhs in
if wlhs = wrhs then
ASSIGN (lhs, rhs)
else if wlhs < wrhs then
ASSIGN (lhs, resizepp rhs wlhs)
else if wlhs > wrhs then
ASSIGN (lhs, resizepp rhs wlhs)
else failwith "assign'"

let alw' = function
| Alw x -> x
| oth -> othr := oth; failwith "alw'"

let var' = function
| Regpp x -> Var_of_reg x
| oth -> othr := oth; failwith "var'"

let to_signal x = Sig_of_signed x
let of_signal x = Signed_of_sig x

let unimp = let seen = ref [] in fun lbl lhs rhs -> 
if not (List.mem lbl !seen) then print_endline ("unimp: "^lbl); seen := lbl :: !seen; XOR (lhs, rhs)

let unimps = let seen = ref [] in fun lbl lhs rhs ->
if not (List.mem lbl !seen) then print_endline ("unimps: "^lbl); seen := lbl :: !seen; of_signal (XOR (to_signal lhs, to_signal rhs))

let lognegate fn = fun lhs rhs -> fn lhs (LNeg rhs)

let signed_relational op = Signed_relational op

let unsigned_relational op = Unsigned_relational op

(*
let unsigned_relationalc = function
|Vpilshiftop -> Signal.sll
|Vpirshiftop -> Signal.srl
|Vpiarithrshiftop -> Signal.sra
|oth -> otht := Some oth; failwith "unsigned_relationalc"

let signed_relationalc x = let open Signed in match x with
|Vpilshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.sll (Signed.to_signal lhs) rhs))
|Vpirshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.srl (Signed.to_signal lhs) rhs))
|Vpiarithrshiftop -> (fun lhs rhs -> Signed.of_signal (Signal.sra (Signed.to_signal lhs) rhs))
|oth -> otht := Some oth; failwith "unsigned_relationalc"
*)

let detect_dyadic = function
| op,Sigpp lhs, Sigpp rhs -> relational (unsigned_relational op) lhs rhs
| op,Sigspp lhs, Sigpp rhs -> relational (unsigned_relational op) (to_signal lhs) rhs
| op,Sigpp lhs, Sigspp rhs -> relational (unsigned_relational op) lhs (to_signal rhs)
| op,Sigspp lhs, Sigspp rhs -> relational' (signed_relational op) lhs rhs
| (Vpisubop as op), Sigpp lhs, Conpp rhs -> relational (unsigned_relational op) lhs (Sig_of_const rhs)
| op, Conpp lhs, Sigpp rhs -> relational (unsigned_relational op) (Sig_of_const lhs) rhs
| op, Conpp lhs, Sigspp rhs -> relational' (signed_relational op) (of_signal (Sig_of_const lhs)) rhs
(*
 | (Vpirshiftop|Vpiarithrshiftop as op), Sigpp lhs, Conpp rhs -> relationalc (unsigned_relationalc op) lhs (Constant.to_int rhs)
*)
| op,Sigpp lhs, Conpp rhs -> relational (unsigned_relational op) lhs (Sig_of_const rhs)
(*
| op,Sigspp lhs, Conpp rhs -> relationalc' (signed_relationalc op) lhs (Constant.to_int rhs)
| (Vpilshiftop|Vpirshiftop|Vpiarithrshiftop as op), Regpp lhs, Conpp rhs -> relationalc (unsigned_relationalc op) lhs.value (Constant.to_int rhs)
| (Vpilshiftop|Vpirshiftop|Vpiarithrshiftop as op), Conpp lhs, Conpp rhs -> relationalc (unsigned_relationalc op) (Sig_of_const lhs) (Constant.to_int rhs)
*)
| op, Regpp lhs, Conpp rhs -> relational (unsigned_relational op) (Sig_of_reg lhs) (Sig_of_const rhs)
| op, (Id id), (Bin _ as rhs) -> relational (unsigned_relational op) (Sig_of_remap id) (Sig_of_remap rhs)
| op,lhs,rhs -> othop := (op, lhs, rhs); failwith "detect_dyadic"

let fold' fn rhs = let expl = List.init (widthsig rhs) (fun ix -> BITSEL(rhs,ix)) in List.fold_left fn (List.hd expl) (List.tl expl)

let remapop' = function
|Vpiplusop -> (fun rhs -> rhs)
|Vpiminusop -> arithnegate
|Vpiunaryandop -> fold' (fun lhs rhs -> AND(lhs,rhs))
|Vpiunarynandop -> fold' (lognegate (fun lhs rhs -> AND(lhs,rhs)))
|Vpiunaryorop -> fold' (fun lhs rhs -> OR(lhs,rhs))
|Vpiunarynorop -> fold' (lognegate (fun lhs rhs -> OR(lhs,rhs)))
|Vpiunaryxorop -> fold' (fun lhs rhs -> XOR(lhs,rhs))
|Vpiunaryxnorop -> fold' (lognegate (fun lhs rhs -> XOR(lhs,rhs)))
|Vpibitnegop -> (fun rhs -> LNeg rhs)
|Vpinotop -> (fun rhs -> NOT rhs)
|oth -> otht := Some oth; failwith "remapop'"

let rec (detect:remapp->relational) = function
| Dyadic (op, lhs, rhs) -> detect_dyadic (op,lhs, rhs)
| Id _ as wire -> Remap wire
| Unary (op, rhs) -> Remap (Sigpp (remapop' op (sig' (rhs))))
| Mux2 (cond, lhs, rhs) ->
  let cond_ = sig' (cond) in
  let then_ = sig' (lhs) in
  let else_ = sig' (rhs) in
  Remap (Sigpp (mux2' cond_ then_ else_))
| If_ (cond, lhs, rhs) ->
  let cond_ = sig' (cond) in
  let then_ = List.map (fun itm -> sig' (itm)) lhs in
  let else_ = List.map (fun itm -> sig' (itm)) rhs in
  If_else (cond_, then_, else_)
(*
 | Asgn (Update(lhs, lft, rght, hi, lo), rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := lhs;
   othrmrhs' := rhs;
   let lhs = var' (lhs) in
   let rhs = sig' (rhs) in
   let rhs' = rhs @: select lhs.value lft rght in (* placeholder, not working yet *)
   assign' lhs rhs'
*)
| Asgn (lhs, rhs) ->
   let lhs = var' (lhs) in
   let rhs = sig' (rhs) in
   Signal (assign' (Sig_of_var lhs) rhs)
(*
| Hex (s,width) -> Remap ( Conpp (Constant.of_hex_string ~widthsig ~signedness:Unsigned s))
| Dec (s,width) -> Conpp (Constant.of_z ~widthsig (Z.of_string s))
| Oct (s,width) -> Conpp (Constant.of_octal_string ~widthsig ~signedness:Unsigned s)
| Bin (s,width) -> Conpp (Constant.of_binary_string_hum s)
*)
| Concat (op, lhs, rhs) -> 
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := lhs;
   othrmrhs' := rhs;
   let lhs = sig' (lhs) in
   let rhs = sig' (rhs) in
   Signal (CONCAT(lhs, rhs))
| Bitsel(nam, sel) -> Signal (mux' (sig' (sel)) (sig' (nam)))
| Selection(nam, lft, rght, hi, lo) -> Signal (PARTSEL (sig' nam, lft, rght))
(*
| Bitsel(nam, Dec (n, _)) -> Sigpp (bit (sig' (nam)) (int_of_string n))
| Item (cond, Dec (n, _), stmt) -> let wid = widthsig (sig' (cond)) in Itm (of_int ~width:wid (int_of_string n), [ alw' (stmt) ])
| Case (mode, lst) -> Alw (Always.switch (sig' (mode)) (List.map (fun itm -> match itm with Itm c -> c | _ -> failwith "itm") lst))
*)
| Signed x -> (match x with Sigpp x -> Signal (of_signal x) | Sigspp x -> Signal (x) | _ -> failwith "Signed")
| Unsigned x -> (match x with Sigspp x -> Signal (to_signal x) | Sigpp _ as x -> Remap (Unsigned x) | _ -> failwith "Unsigned")
| oth -> othp := oth; failwith "detect"

and sig' = function
| Conpp x -> Sig_of_const x
| Sigpp x -> x
| Sigspp x -> Sig_of_signed x
| Regpp x -> Sig_of_reg x
| Dyadic(a,b,c) -> (match detect_dyadic (a,b,c) with
    | oth -> othrel := oth; failwith "sig''")
| oth -> othr := oth; failwith "sig'"

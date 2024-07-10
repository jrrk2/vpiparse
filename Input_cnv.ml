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
open Input_dump
open Dump_types

let seqpat = ref Work
let oldpat = ref Work
let oldpat' = ref Work
let othrng' = ref TYPNONE
let othrng2' = ref (TYPNONE, TYPNONE)
let patlst = ref []
let othvoid = ref Work
let crntp = ref [Work]
let othedg = ref (Void 0)
let oth' = ref UNKNOWN
let othlst' = ref [UNKNOWN]
let othpat = ref Work
let othpat' = ref UNKNOWN
let othpat2 = ref (UNKNOWN, UNKNOWN)
let othalwys = ref (UNKNOWN, [UNKNOWN])
let marklst = ref []

let fullpth pth = String.concat "$" (List.rev (List.map (function Work -> "$" | STRING s -> s |  _ -> "_") pth))
let mark n = if not (List.mem n !marklst) then marklst := n :: !marklst
let dump () = List.iter (fun n -> print_endline (string_of_int n)) (List.sort_uniq compare !marklst)

let _Concat lst = CAT ("", lst)
let _Concat_multi lst = CAT ("", lst)

let _Unary = function
| Vpiunaryandop -> Lredand
| Vpiunarynandop -> Lrednand
| Vpiunaryorop -> Lredor
| Vpiunarynorop -> Lrednor
| Vpiunaryxorop -> Lredxor
| Vpiunaryxnorop -> Lredxnor
| oth -> othpat := oth; failwith "_Unary"

let _Void itms n = othvoid := List.hd !patlst; UNKNOWN

let othstmt = ref Work

(* an example blocking converter *)
let rec nonblocking = function
| ASGN (false, "", (VRF (a, typrnga, []) :: VRF (c, typrngc, []) :: [])) ::
  ASGN (false, "",
               (ARITH (op,
		       (VRF (c', typrngc', []) ::
			    VRF (b, typrngb, []) :: [])) :: VRF (c'', typrngc'', []) :: [])) :: tl
when c=c' && c'=c'' && typrnga=typrngc && typrngc=typrngc' && typrngc'=typrngc'' ->

  ASGN (false, "",
               (ARITH (op,
		       (VRF (a, typrnga, []) ::
			    VRF (b, typrngb, []) :: [])) :: VRF (c, typrngc, []) :: [])) :: nonblocking tl
| ASGN (false, "", (VRF (a, typrnga, []) :: VRF (c, typrngc, []) :: [])) ::
  ASGN (false, "",
               (LOGIC (op,
		       (VRF (c', typrngc', []) ::
			    VRF (b, typrngb, []) :: [])) :: VRF (c'', typrngc'', []) :: [])) :: tl
when c=c' && c'=c'' && typrnga=typrngc && typrngc=typrngc' && typrngc'=typrngc'' ->

  ASGN (false, "",
               (LOGIC (op,
		       (VRF (a, typrnga, []) ::
			    VRF (b, typrngb, []) :: [])) :: VRF (c, typrngc, []) :: [])) :: nonblocking tl
| oth -> oth

let _Always itms (deplst, body) = match deplst with
| SNTRE items -> itms.alwys := ("", COMB, (SNTRE items :: nonblocking body)) :: !(itms.alwys)
| POSPOS (clk, rst) as edg -> itms.alwys := ("", edg, body) :: !(itms.alwys)
| POSEDGE (clk) as edg -> itms.alwys := ("", edg, body) :: !(itms.alwys)
| deplst -> othalwys := (deplst, body); failwith "_Always"

let rec parse_base base r =
  let len = String.length r in
  if len = 0 then 0 else base * parse_base base (String.sub r 0 (len-1)) + (match r.[0] with
 | '0'..'9' -> Char.code r.[len-1] - Char.code '0'
 | _ -> 0)

let rec parse_bin b = parse_base 2 b
let rec parse_oct o = parse_base 8 o
let rec parse_dec d = parse_base 10 d

let _Class itms = ()
let _Id itms _ = failwith "Id"
let _Alw itms _ = failwith "Alw"
let _Bin itms (s, w) = let rslt = parse_bin s in CNST (w, HEX rslt)
let _Oct itms (s, w) = let rslt = parse_oct s in CNST (w, HEX rslt)
let _Dec itms (s, w) = let rslt = parse_dec s in CNST (w, HEX rslt)
let _Seq itms = function
  | [] -> SCOPE ""
  | lst -> BGN(None, lst)
let _Hex itms (s, w) = CNST (w, try Scanf.sscanf s "%x" (fun n -> HEX n) with _ -> STRING s)
let _If_ itms (c, a, b) = IF ("", [c; a; b])
let _If itms (c, a) = IF ("", [c; a])
let _Mux2 itms (_, _, _) = failwith "Mux2"
let _Block itms (_, _) = failwith "Block"
let _Block_array itms t s r = ASGN(true, s, [])
let _Integer itms n = CNST(32, HEX n)
let _Selection itms (nam, lft, rght, _, _) = SEL ("", [nam; rght; lft])
let _Update itms (_, _, _, _, _) = failwith "Update"
let _Bitsel itms (a, b) = SEL ("", a :: b :: _Integer itms 1 :: [])
let _Dyadic itms (_, _, _) = failwith "Dyadic"

let _Case itms (exp, lst) = CS ("", List.filter (function UNKNOWN -> false | _ -> true) (List.map (function
    | UNKNOWN -> UNKNOWN
    | SCOPE "Place767" -> UNKNOWN
    | VRF _ as v -> v
    | CSITM _ as c -> c
    | oth -> othpat' := oth; failwith "tran case othpat'") (exp :: lst)))

let _Item itms (n, stmt) = CSITM("", n :: stmt :: [])
let _Signed itms _ = failwith "Signed"
let _Unsigned itms _ = failwith "Unsigned"
let _Conpp itms _ = failwith "Conpp"
let _Inppp itms _ = failwith "Inppp"
let _Inpspp itms _ = failwith "Inpspp"
let _Sigpp itms _ = failwith "Sigpp"
let _Sigspp itms _ = failwith "Sigspp"
let _Alwpp itms _ = failwith "Alwpp"
let _Regpp itms _ = failwith "Regpp"
let _Itmpp itms _ = failwith "Itmpp"
let _Wirepp itms _ = failwith "Wirepp"
let _Othpp itms _ = failwith "Othpp"

let othtypmap = ref Work
let _typmap = function
| Vpireg -> []
| Vpisigned -> [TYPSIGNED]
| oth -> othtypmap := oth; failwith "_typmap"

let _Type_spec_rng itms typrng = VRF ("", (BASDTYP, "wire", typrng, []), [])
let _Identrng typ itms nam typrng = VRF (nam, (BASDTYP, "wire", typrng, _typmap typ), [])
let _Ident itms s = VRF (s, (BASDTYP, "wire", TYPNONE, []), [])

let identyp = ref Work

let identypmap = function
| Vpialways -> "wire"
| Vpinet -> "wire"
| Vpireg -> "reg"
| oth -> identyp := oth; failwith "identypmap"

let vadd itms (nam, arg) = if not (List.mem_assoc nam !(itms.v)) then itms.v := (nam, arg) :: !(itms.v) else failwith ("vadd: duplicate "^nam)

let _Identyp itms nam = function
| Vpinet | Vpireg | Vpialways as typ -> if not (List.mem_assoc nam !(itms.io)) then
vadd itms (nam, ("", (BASDTYP, (identypmap typ), TYPNONE, []), (identypmap typ), (UNKDTYP, "", TYPNONE, []))) 
| oth -> identyp := oth; failwith "identyp"

let _Identyprng itms nam rng = function
| (Vpinet | Vpireg | Vpialways as typ) -> if not (List.mem_assoc nam !(itms.io)) then
vadd itms (nam, ("", (BASDTYP, (identypmap typ), rng, []), (identypmap typ), (UNKDTYP, "", TYPNONE, []))) 
| oth -> identyp := oth; failwith "identyp"

let _Port itms dir nam = itms.io := (nam, ("", (BASDTYP, "wire", TYPNONE, []), dir, "wire", [])) :: !(itms.io)
let _Portrng itms dir nam typrng = itms.io := (nam, ("", (BASDTYP, "wire", typrng, []), dir, "wire", [])) :: !(itms.io)
let _Porthigh itms dir nam = vadd itms (nam, ("", (BASDTYP, "wire", TYPNONE, []), "wire", (UNKDTYP, "", TYPNONE, []))) 

let othenum' = ref UNKNOWN
let othcase = ref Work

let _Enumvar itms nam = function
| BGN (None, VRF (typdef, _, _) :: lst) as typ -> othenum' := typ; List.iter (function
  | VRF ("", typ', []) -> vadd itms (nam, ("", typ', typdef, (UNKDTYP, "", TYPNONE, []))) 
  | oth -> ()) lst
| oth -> othenum' := oth; failwith "_Enumvar"

let _Porthighrng itms dir nam typrng =
    if not (List.mem_assoc nam !(itms.io)) then
 vadd itms (nam, ("", (BASDTYP, "wire", typrng, []), "wire", (UNKDTYP, "", TYPNONE, []))) 
let _Enum itms enum_t lst =
  if not (List.mem_assoc enum_t !(itms.cnst)) then itms.cnst := (enum_t,(false,(0,CNSTEXP(Aunknown, List.map (function
  | TUPLE5 (Enum_const, STRING enam, TUPLE2 (INT, Int n), Vpidecompile _, TUPLE2 (Vpisize, Int _)) -> ENUMVAL (n, enam)
  | _ -> STRING "unknown") lst)))) :: !(itms.cnst)
let _Param itms param = function
| TUPLE2 (UINT, Int n) -> if not (List.mem_assoc param !(itms.cnst)) then itms.cnst := (param, (false, (0,HEX n))) :: !(itms.cnst)
| oth -> othpat := oth; failwith "_Param"
let _Not itms _ = failwith "Not"
let _Lneg itms a = UNRY (Unegate, a :: [])
let _Aneg itms _ = failwith "Aneg"
let _Sneg itms _ = failwith "Sneg"
let _Mux itms (_, _) = failwith "Mux"
let _Add itms (a, b) = ARITH(Aadd(itms.mode), [a;b])
let _Sub itms (a, b) = ARITH(Asub, [a;b])
let _Mult itms (a, b) = ARITH(Amul, [a;b])
let _Mults itms (a, b) = ARITH(Amuls, [a;b])
let _Div itms (a, b) = ARITH(Adiv, [a;b])
let _Mod itms (a, b) = ARITH(Amod, [a;b])
let _Pow itms (a, b) = ARITH(Apow, [a;b])
let _And itms (a, b) = LOGIC(Land, [a;b])
let _Or itms (a, b) = LOGIC(Lor, [a;b])
let _Xor itms (a, b) = LOGIC(Lxor, [a;b])
let _Xnor itms (a, b) = UNRY(Unot, LOGIC(Lxor, [a;b]) :: [])
let _LogAnd itms (a, b) = LOGIC(Lredand, [a;b])
let _LogOr itms (a, b) = LOGIC(Lredor, [a;b])
let _Partsel itms (_, _, _) = failwith "Partsel"
let _Lt itms (a, b) = CMP(Clt, [a;b])
let _Le itms (a, b) = CMP(Clte, [a;b])
let _Eq itms (a, b) = CMP(Ceq, [a;b])
let _Ne itms (a, b) = CMP(Cneq, [a;b])
let _Ge itms (a, b) = CMP(Cgte, [a;b])
let _Gt itms (a, b) = CMP(Cgt, [a;b])
let _LshiftL itms (a, b) = LOGIC(Lshiftl, [a;b])
let _LshiftR itms (a, b) = LOGIC(Lshiftr, [a;b])
let _AshiftR itms (a, b) = LOGIC(Lshiftrs, [a;b])
let _Ternary itms (cond, a, b) = CND ("", [cond;a;b])
let _Gen_case itms cond lst = itms.gen := ("", lst) :: !(itms.gen); UNKNOWN
let _Posedge itms = function
| VRF (id, (BASDTYP, "wire", TYPNONE, []), []) -> POSEDGE id
| oth -> oth' := oth; failwith "_Posedge oth'"
let rec _cnstexpr = function
| CNST (w, lft) -> lft
| ARITH(arithop, args) -> CNSTEXP(arithop, List.map _cnstexpr args)
| VRF (param, (BASDTYP, "wire", TYPNONE, []), []) -> STRING param
| SYS ("", syscall, args) -> CNSTEXP(Aunknown, STRING syscall :: List.map _cnstexpr args)
| oth -> oth' := oth; failwith "_cnstexpr oth'"
let _Range itms (lft, rght) = TYPRNG(_cnstexpr lft, _cnstexpr rght)
let _Place itms (n, _, _) = SCOPE ("Place"^string_of_int n)
let _Array_var itms _ = failwith "Array_var"
let otharray = ref (UNKNOWN, UNKNOWN, 0, 0)
let _Array_net itms = function
| VRF (nam, (BASDTYP, "wire", TYPNONE, []), []),
  VRF ("", (BASDTYP, "wire", typrng, []), []),
  siz, typrng' ->
 vadd itms (nam, ("", (UNPACKADTYP, "",
         RECTYP (BASDTYP, "wire", typrng, []),
         [TYPRNG (SHEX 0, SHEX (siz-1))]),
		      "", (UNKDTYP, "", TYPNONE, []))) 
| a -> otharray := a; failwith "actual"
let _Edge itms = function
| [POSEDGE clk; POSEDGE rst] -> POSPOS (clk, rst)
| VRF _ :: _ as lst -> SNTRE lst
| [] -> SNTRE []
| oth -> othlst' := oth; failwith "_Edge oth'"
let _ArrayRange itms = function
| TYPRNG(lo,hi) as rng, (TYPRNG(lft,rght) as elm) -> VRF ("", (UNPACKADTYP, "wire", rng, [elm]), [])
| oth -> othrng2' := oth; failwith "_ArrayRange othrng2'"
let _TypespecRange itms = function
| TYPRNG(lft,rght) as rng -> VRF ("", (BASDTYP, "wire", rng, []), [])
| oth -> othrng' := oth; failwith "_TypespecRange othrng'"

let _Asgn itms = function
| (lft, rght) -> ASGN(true, "", [rght;lft])

let _Cont_assign itms = function
| (lft, rght) -> itms.ca := ("", rght , lft) :: !(itms.ca)

let _Block itms (a, b) = ASGN(false, "", [b;a])

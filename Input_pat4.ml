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
let topmods = ref []
let allmods = ref []
let othmap = ref ([],[])
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

let _Concat (a, b) = CAT ("", [a; b])
let rec concat = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> _Concat(hd, concat tl)
let rec concat_multi = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> _Concat(hd, concat tl)

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
let othstmtlst = ref []

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
let _Type_spec_rng itms typrng = VRF ("", (BASDTYP, "logic", typrng, []), [])
let _Identrng typ itms nam typrng = VRF (nam, (BASDTYP, "logic", typrng, []), [])
let _Ident itms s = VRF (s, (BASDTYP, "logic", TYPNONE, []), [])

let identyp = ref Work

let identypmap = function
| Vpialways -> "wire"
| Vpinet -> "wire"
| Vpireg -> "reg"
| oth -> identyp := oth; failwith "identypmap"

let _Identyp itms nam = function
| Vpinet | Vpireg | Vpialways as typ -> if not (List.mem_assoc nam !(itms.io)) then
itms.v := (nam, ("", (BASDTYP, (identypmap typ), TYPNONE, []), (identypmap typ), (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
| oth -> identyp := oth; failwith "identyp"

let _Identyprng itms nam rng = function
| (Vpinet | Vpireg | Vpialways as typ) -> if not (List.mem_assoc nam !(itms.io)) then
itms.v := (nam, ("", (BASDTYP, (identypmap typ), rng, []), (identypmap typ), (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
| oth -> identyp := oth; failwith "identyp"

let _Port itms dir nam = itms.io := (nam, ("", (BASDTYP, "logic", TYPNONE, []), dir, "logic", [])) :: !(itms.io)
let _Portrng itms dir nam typrng = itms.io := (nam, ("", (BASDTYP, "logic", typrng, []), dir, "logic", [])) :: !(itms.io)
let _Porthigh itms dir nam = itms.v := (nam, ("", (BASDTYP, "logic", TYPNONE, []), "logic", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)

let othenum' = ref UNKNOWN
let othcase = ref Work

let _Enumvar itms nam = function
| BGN (None, VRF (typdef, _, _) :: lst) as typ -> othenum' := typ; List.iter (function
  | VRF ("", typ', []) -> itms.v := (nam, ("", typ', typdef, (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
  | oth -> ()) lst
| oth -> othenum' := oth; failwith "_Enumvar"

let _Porthighrng itms dir nam typrng =
    if not (List.mem_assoc nam !(itms.io)) then
 itms.v := (nam, ("", (BASDTYP, "logic", typrng, []), "logic", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
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
let _Add itms (a, b) = ARITH(Aadd, [a;b])
let _Sub itms (a, b) = ARITH(Asub, [a;b])
let _Mult itms (a, b) = ARITH(Amul, [a;b])
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
| VRF (id, (BASDTYP, "logic", TYPNONE, []), []) -> POSEDGE id
| oth -> oth' := oth; failwith "_Posedge oth'"
let rec _cnstexpr = function
| CNST (w, lft) -> lft
| ARITH(arithop, args) -> CNSTEXP(arithop, List.map _cnstexpr args)
| VRF (param, (BASDTYP, "logic", TYPNONE, []), []) -> STRING param
| SYS ("", syscall, args) -> CNSTEXP(Aunknown, STRING syscall :: List.map _cnstexpr args)
| oth -> oth' := oth; failwith "_cnstexpr oth'"
let _Range itms (lft, rght) = TYPRNG(_cnstexpr lft, _cnstexpr rght)
let _Place itms (n, _, _) = SCOPE ("Place"^string_of_int n)
let _Array_var itms _ = failwith "Array_var"
let otharray = ref None
let _Array_net itms = function
| VRF (nam, (BASDTYP, "logic", TYPNONE, []), []),
  VRF ("", (BASDTYP, "logic", typrng, []), []),
  siz, typrng' ->
 itms.v := (nam, ("", (UNPACKADTYP, "",
         RECTYP (BASDTYP, "logic", typrng, []),
         [TYPRNG (SHEX 0, SHEX (siz-1))]),
		      "", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
| a -> otharray := Some a; failwith "actual"
let _Edge itms = function
| [POSEDGE clk; POSEDGE rst] -> POSPOS (clk, rst)
| VRF _ :: _ as lst -> SNTRE lst
| [] -> SNTRE []
| oth -> othlst' := oth; failwith "_Edge oth'"
let _ArrayRange itms = function
| TYPRNG(lo,hi) as rng, (TYPRNG(lft,rght) as elm) -> VRF ("", (UNPACKADTYP, "logic", rng, [elm]), [])
| oth -> othrng2' := oth; failwith "_ArrayRange othrng2'"
let _TypespecRange itms = function
| TYPRNG(lft,rght) as rng -> VRF ("", (BASDTYP, "logic", rng, []), [])
| oth -> othrng' := oth; failwith "_TypespecRange othrng'"

let _Asgn itms = function
| (lft, rght) -> ASGN(true, "", [rght;lft])

let _Cont_assign itms = function
| (lft, rght) -> itms.ca := ("", rght , lft) :: !(itms.ca)

let _Block itms (a, b) = ASGN(false, "", [b;a])

let rec top_pat' itms = function
| Work -> ()
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec,
	TLIST (STRING enum_t :: TUPLE2 (Vpiinstance, TLIST _) :: lst))) -> _Enum itms enum_t lst
|   TUPLE2 (Vpivariables,
    TUPLE10 (Array_var, TUPLE2 (Vpisize, Int 64),
      TUPLE2 (Vpitypespec,
        TUPLE3 (Ref_typespec,
          TLIST _,
          TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, Work)))),
      STRING mem,
      TLIST _,
      Vpirandtype, TUPLE2 (Vpivisibility, Int 1), Vpiarraytype,
      rng,
      TUPLE2
       (Vpireg,
        TUPLE4
         (Logic_var, Vpitypespec,
          TUPLE3
           (Ref_typespec,
            TLIST _,
            TUPLE2 (Vpiactual,
              TUPLE4 (Logic_typespec, LOC _, Logic_var,
                elem))),
          TLIST _)))) -> _Identyp itms mem Vpireg (* placeholder *)

|   TUPLE2 (Vpivariables,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _)) -> _Identyp itms s Vpireg
|   TUPLE2 (Vpivariables,
     TUPLE6 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))),
       STRING s, TLIST _,  TUPLE2 (Vpivisibility, Int _))) -> _Identyprng itms s (typrng itms rng) Vpireg
|   TUPLE2 (Vpivariables,
     TUPLE6 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _,  TUPLE2 (Vpivisibility, Int _))) -> _Identyp itms s Vpireg
|   TUPLE2 (Vpivariables,
     TUPLE5 (Enum_var,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, typ)))),
       STRING s, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> _Enumvar itms s (pat itms typ)
| TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) -> _Cont_assign itms (expr itms rhs, expr itms lhs)
| TUPLE2 (Vpitopmodule, Int n) -> ()
| TUPLE2 ((Uhdmtopmodules|Uhdmallmodules), TLIST rawlst) ->
        (match List.partition (function TUPLE2 ((Vpitypedef|Vpiparamassign|Vpivariables|Vpimodule|Vpiport|Vpiprocess|Vpitopmodule|Vpinet|Vpitop), _) | TUPLE3 ((Vpiparameter|Cont_assign), _,_ ) | TUPLE6 ((Parameter), _, _ , _, _ , _) -> true | _ -> false) rawlst with
          | types, Vpiparent :: TLIST [] :: (STRING topmod) :: body -> fresh (topmod) allmods types body
          | types, Vpiname :: (STRING topmod) :: body -> fresh (topmod) topmods types body
          | oth -> othmap := oth; failwith "map'")
| TUPLE4 (Gen_scope_array, STRING lbl, TLIST _, TUPLE2(Gen_scope, TLIST (TLIST _ :: lst))) -> List.iter (function
  | TUPLE2 (Vpiprocess, TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
    TUPLE3 (Event_control,
	TUPLE2 (Vpicondition, cond), TUPLE3(Begin, TLIST _, TLIST (Vpiparent::TLIST _::lst))))) ->
	    let _ =  _Always itms (expr itms cond, seqlst itms lst) in (); (* failwith "Event339" *)
  | TUPLE2 (Vpiprocess, p) -> oldpat := p; failwith "proc"
  | TUPLE2 (Vpinet, _) as vpi -> let _ = top_pat itms vpi in ()
  | TUPLE2 (Vpivariables, _) as vpi -> let _ = top_pat itms vpi in ()
  | oth -> oldpat := oth; failwith "gen_scope") lst
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, (Vpialways|Vpiassignstmt))),
		     (TUPLE5 (Assignment, _, _, _, _) as stmt)) ->
		     _Always itms (SNTRE [], seqtok itms stmt)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, (Vpialways|Vpiassignstmt))),
		     TUPLE3 (Begin, TLIST _, TLIST (Vpiparent :: TLIST [] :: lst))) ->
		     othstmtlst := lst;
		     _Always itms (SNTRE [], seqlst itms lst)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, (Vpialways|Vpiassignstmt))),
		     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond), TUPLE3 (Begin, TLIST _, TLIST (Vpiparent :: TLIST [] :: lst )))) ->
		     othstmtlst := lst;
		     _Always itms ((expr itms) cond, seqlst itms lst)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, (Vpialways|Vpiassignstmt))),
		     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond), stmt)) ->
		     othstmt := stmt;
		     _Always itms ((expr itms) cond, seqtok itms stmt)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, (Vpialways|Vpiassignstmt))),
       TUPLE2 (Event_control,
         TUPLE3 (Begin, TLIST _, TLIST lst))) -> _Always itms (_Edge itms [], seqlst itms lst)
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST []))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Identyp itms nam typ
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) ->
        let rng' = typrng itms rng in
        _Porthighrng itms typ nam rng'
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned)))),
       STRING nam, TLIST _, (Vpisigned as typ))) ->
        let rng' = typrng itms rng in
        _Porthighrng itms typ nam rng'
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Port itms (dirmap dir) s
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng))))) -> _Portrng itms (dirmap dir) s (typrng itms rng)
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned))))) -> _Portrng itms (dirmap dir) s (typrng itms rng)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST pth,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))))) -> _Porthighrng itms (dirmap dir) (fullpth pth) (typrng itms rng)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING _, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST pth,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Porthigh itms (dirmap dir) (fullpth pth)
|   TUPLE2 (Vpiprocess, proc) -> top_pat itms proc
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _)) -> _Enum itms s []
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng)))),
       STRING s, TLIST _)) -> _Identyp itms s Vpinet
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpinet|Vpireg|Vpialways as typ)))) -> _Identyp itms s typ
|   TUPLE2 (Vpinet,
     TUPLE3 (Logic_net, STRING s, TLIST _)) -> _Identyp itms s Vpinet
|   TUPLE2 (Vpinet,
    TUPLE6 (Array_net, TUPLE2 (Vpisize, Int siz), mem,
      TLIST _,
      rng,
      TUPLE2 (Vpinet,
        TUPLE4 (Logic_net,
          TUPLE2 (Vpitypespec,
            TUPLE3 (Ref_typespec,
              TLIST _,
              TUPLE2 (Vpiactual, actual))),
          TLIST _,
          TUPLE2 (Vpinettype, Vpireg))))) -> _Array_net itms (pat itms mem, pat itms actual, siz, typrng itms rng)
| TUPLE2 (Vpitop, Int n) -> ()
| TUPLE2 (Uhdmtoppackages,
     TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
       TUPLE2 (Vpiclassdefn,
         TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)))) -> ()
| TUPLE2 (Uhdmallpackages, TUPLE4 (Package, Builtin, STRING _, Builtin)) -> ()
| TUPLE2 (Uhdmallclasses,
     TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
       TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task))) -> ()
| TUPLE2 (Uhdmallclasses,
     TUPLE6 (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class itms
| TUPLE2 (Uhdmallclasses,
     TUPLE10 (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class itms
| TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> ()
| TUPLE2 (Vpimodule, TLIST (Vpiparent :: STRING inferior :: TLIST _ :: lst)) -> List.iter (function
  | Input.STRING s -> ()
  | oth -> top_pat itms oth) lst
| Vpideffile -> ()
| TUPLE2 (Vpideflineno, Int _) -> ()
| TUPLE2 (Vpiinstance, TLIST _) -> ()
| TUPLE6 (Parameter, _, _, _, _, _) as par -> let param, p, _ = parameter itms par in _Param itms param p
| TUPLE2 (Vpiparamassign, TLIST lst) -> ()
| TUPLE2 (Vpigenscopearray, arg) -> top_pat itms arg
| oth -> othpat := oth; failwith "top_pat"

and pat' itms = function
| TUPLE2 (Vpiinstance, TLIST _) -> UNKNOWN
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned)))),
       STRING nam, TLIST _, Vpisigned)) -> _Identrng Vpisigned itms nam (typrng itms rng)
|   TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> _Void itms     11
|   TUPLE6 (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> _Void     itms 41
|   TUPLE6 (Array_var,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST lst)))),
     STRING s, TLIST _, Vpiarraytype,
     TUPLE2 (Vpireg,
       TUPLE4 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, Logic_net, rng))),
         TLIST _))) -> _Array_var itms s
|   TUPLE5 (Enum_var,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _, TUPLE2 (Vpivisibility, Int _)) -> _Void    itms 107
|   TUPLE5 (Enum_const, STRING s, TUPLE2 (INT, Int _), Vpidecompile _,
     TUPLE2 (Vpisize, Int _)) -> _Ident itms s
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> _Void    itms 201
|   TUPLE4 (Logic_typespec, LOC _, (Logic_net|Logic_var), rng) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE4 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> _Void    itms 219
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void    itms 220
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void    itms 222
|   TUPLE4 (Input.If_else, cond, then_, else_) -> _If_ itms ((expr itms) cond, (pat itms) then_, (pat itms) else_)
|   TUPLE6 (Parameter, _, _, _, _, _) as par -> let param, p, _ = parameter itms par in _Param itms param p; UNKNOWN
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, s))) -> _Place itms (291, (pat itms) s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, s))) -> _Place itms (294, (pat itms) s, Void 0)
|   TUPLE2 (Array_typespec, Work) -> failwith "Array_typespec"
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST (Vpirange :: elem)))) -> _Void    itms 300
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST (TUPLE3(Vpirange,_,_) :: elem)))) -> _Void    itms 300
|   TUPLE3 (Ref_module, TUPLE3 (STRING _, STRING _, LOC _), TLIST lst) ->seq itms lst
|   TUPLE3 (Named_begin, (STRING _ | TLIST _), TLIST rawlst) -> let namedmods = ref [] in
        (match List.partition (function TUPLE2 ((Vpitypedef|Vpiparamassign|Vpivariables|TUPLE2(Always,_)), _) | TUPLE3 (Vpiparameter, _,_ ) -> true | _ -> false) rawlst with
          | types, Vpiparent :: STRING topnamed :: TLIST _ :: body -> fresh topnamed namedmods types body
          | types, Vpiparent :: STRING topnamed :: body -> fresh topnamed namedmods types body
          | oth -> othmap := oth; failwith "named_begin"); (match !namedmods with (top, (lst', uitms))::[] -> CONTAINER (uitms,SCOPE top) | _ -> UNKNOWN)
|   TUPLE3 (Logic_typespec, LOC _, Logic_net) -> _Place itms (308, Void 0, Void 0)
|   TUPLE3 (Logic_typespec, LOC _, rng) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond), then_) -> _If itms ((expr itms) cond, (pat itms) then_)
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> _Void    itms 321
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> _Void    itms 324
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    itms 327
|   TUPLE3 (Class_defn, Queue, Queue) -> _Void    itms 329
|   TUPLE3 (Class_defn, Array, Array) -> _Void    itms 330
|   TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class) -> _Void    itms 331
|   TUPLE3 (Class_defn, STRING _, STRING _) -> _Void    itms 332
|   TUPLE3 (Begin, _, TLIST lst) -> _seq itms lst
|   TUPLE3 (STRING _, STRING _, LOC _) -> _Void    itms 373
|   TUPLE2 (Weaklyreferenced, TLIST lst) -> seq itms lst      
|   TUPLE2 (Vpivisibility, Int _) -> _Void    itms 375
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> _Void    itms 398
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> _Void    itms 403
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))) -> _TypespecRange itms (typrng itms rng)
|   TUPLE2 (Vpisize, Int _) -> _Void    itms 418
|   TUPLE2 (Vpisigned, Int _) -> _Void    itms 419
|   TUPLE2 (Vpirightrange, rhs) -> (expr itms) rhs
|   TUPLE2 (Vpirhs, rhs) -> (expr itms) rhs
|   TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned) -> _Identrng Vpisigned itms "$logic_typespec" (typrng itms rng)
|   TUPLE5 (Logic_typespec, LOC _, Logic_net, rng, Vpisigned) -> _Identrng Vpisigned itms "$logic_typespec" (typrng itms rng)
|   TUPLE2 (Vpireg,
     TUPLE4 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, (Logic_net|Logic_var), rng))),
       TLIST pth)) -> _Identrng Vpireg itms (match List.hd pth with STRING s -> s | _ -> failwith "Vpitypespec") (typrng itms rng)
|   TUPLE2 (Vpioverriden, Int n) -> _Place itms (489, Integer n, Void 0)
|   TUPLE2 (Vpinettype, Vpireg) -> _Void    itms 490
|   TUPLE2 (Vpinettype, Vpinet) -> _Void    itms 491
|   TUPLE2 (Vpinettype, Vpialways) -> _Void    itms 492
|   TUPLE2 (Vpiname, Semaphore) -> _Void    itms 540
|   TUPLE2 (Vpiname, Process) -> _Void    itms 541
|   TUPLE2 (Vpiname, Mailbox) -> _Void    itms 542
|   TUPLE2 (Vpiname, STRING _) -> _Void    itms 543
|   TUPLE2 (Vpimethod, Task) -> _Void    itms 545
|   TUPLE2 (Vpimethod, Function) -> _Void    itms 546
|   TUPLE2 (Vpilowconn, conn) -> (expr itms) conn
|   TUPLE2 (Vpilhs, lhs) -> (expr itms) lhs
|   TUPLE2 (Vpileftrange, lft) -> expr itms lft
|   TUPLE2 (Vpiindex,
     TUPLE7 (Part_select, TUPLE2 (Vpiname, STRING _),
       TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
       TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
       TUPLE2 (Vpirightrange, _))) -> _Void    itms 557
|   TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst)) -> _Void    itms 562
|   TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _)) -> _Void    itms 578
|   TUPLE2 (Vpihighconn, _) -> _Void    itms 579
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST (TUPLE2 (Vpicondition, _) as c :: lst))) -> let x = List.map (function
  | TUPLE2 (Vpicaseitem,
      TUPLE3 (Case_item,
       (TUPLE5 (Constant, _, _, _, _) | TUPLE4 (Ref_obj, _, _, _ ) as cexp),
		     stmt)) -> _Item itms (expr itms cexp, pat itms stmt)
  | TUPLE2 (Vpicaseitem,
      TUPLE2 (Case_item,
		     stmt)) -> _Item itms (CNST(0, STRING "default"), pat itms stmt)
  | oth -> othcase := oth; failwith "othcase") lst in
_Gen_case itms (expr itms c) x
|   TUPLE2 (Vpifullname, TLIST _) -> _Void    itms 584
|   TUPLE2 (Vpielaborated, Int _) -> _Void    itms 596
|   TUPLE2 (Vpidirection, Vpioutput) -> _Void    itms 597
|   TUPLE2 (Vpidirection, Vpiinput) -> _Void    itms 598
|   TUPLE2 (Vpidefname, STRING _) -> _Void    itms 599
|   TUPLE2 (Vpiconstantselect, Int _) -> _Void    itms 601
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)) -> _Void    itms 604
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)) -> _Void    itms 605
|   TUPLE2 (Vpiclassdefn,
     TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)) -> _Void    itms 608
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)) -> _Void    itms 609
|   TUPLE2 (Vpicasetype, Int n) -> _Void itms n
|   TUPLE2 (Vpiblocking, Int _) -> _Void    itms 666
|   TUPLE2 (Vpialwaystype, Vpialways) -> _Void    itms 668
|   TUPLE2 (Vpiactual,
     TUPLE5 (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
       TUPLE2 (Vpisize, Int _))) -> _Void    itms 711
|   TUPLE2 (Vpiactual,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> _Void    itms 721
|   TUPLE2 (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Void    itms 724
|   TUPLE2 (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Void    itms 728
|   TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)) -> _Void    itms 735
|   TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)) -> _Void    itms 736
|   TUPLE2 (Vpiactual, TLIST lst) ->seq itms lst
|   TUPLE2 (UINT, Int n) -> _Integer itms n
|   TUPLE2 (Ref_typespec,
     TUPLE2 (Vpiactual,
       TUPLE3 (Logic_typespec, LOC _, rng))) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE2 (Ref_obj, STRING s) -> _Ident itms s
|   TUPLE2 (Vpiport, Port) -> _Place itms (452, Void 0, Void 0)
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING s, TUPLE2 (Vpihighconn, _))) -> _Place itms (462, Ident s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, s))) -> _Place itms (486, (expr itms) s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING s)) -> _Place itms (487, Ident s, Void 0)
|   TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> _Void    itms 787
|   TUPLE2 (Port, STRING _) -> _Void    itms 788
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int n)) -> _Integer itms n
|   TUPLE2 (INT, Int _) -> _Void    itms 791
|   TUPLE2 (Enum_typespec, _) -> _Void    itms 793
|   TUPLE2 (Case_stmt, TLIST (Vpiparent :: TUPLE2 (Vpicasetype, Int 1) :: TUPLE2 (Vpicondition,cond) :: lst)) ->
_Case itms (expr itms cond, List.map (function
    | TUPLE3 (Case_item,
       (TUPLE5 (Constant, _, _, _, _) | TUPLE4 (Ref_obj, _, _, _ ) as cexp),
		     stmt) -> _Item itms (expr itms cexp, pat itms stmt)
    | TUPLE2 (Case_item,
		     stmt) -> _Item itms (CNST(0, STRING "default"), pat itms stmt)
    | oth -> othcase := oth; failwith "othcase") lst)
|   TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _)) -> _Void    itms 796
|   TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _)) -> _Void    itms 797
|   TUPLE2 (Array_typespec, TLIST ((TUPLE3(Vpirange,_,_) as rng) :: (TUPLE2 (Vpielemtypespec, _) as elem) :: [])) ->
      _ArrayRange itms (typrng itms rng, typrng itms elem)
|   TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)) -> _Void    itms 800
|   TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
     TUPLE2 (Vpiclassdefn,
       TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _))) -> _Void    itms 824
|   TUPLE10
    (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> _Void    itms 830
|   TUPLE2 (Vpitypespec,
    TUPLE3 (Ref_typespec, TLIST _,
      TUPLE2 (Vpiactual, TLIST []))) -> _Place itms (681, Void 0, Void 0)
|   TLIST lst -> _seq itms lst
|   Constant -> _Place itms (765, Void 0, Void 0)
| Vpiparent -> _Place itms (    767, Void 0, Void 0)
| Vpirange -> _Place itms (    768, Void 0, Void 0)
| Vpiname -> _Place itms (    770, Void 0, Void 0)
| Vpitypespec -> _Place itms (754, Void 0, Void 0)
|   STRING s -> _Ident itms s
|   Class_typespec -> _Place itms (654, Void 0, Void 0)
|   Int_typespec -> _Place itms (    834, Void 0, Void 0)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) -> _Asgn itms ((expr itms) lhs, (expr itms) rhs)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs,
    TUPLE6(Array_var, TUPLE2(Vpitypespec, t), STRING s, TLIST _, Vpiarraytype, (TUPLE2(Vpireg, _) as r)))) ->
          _Block_array itms (pat itms t) s (pat itms r)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs, lhs)) -> failwith "blocking"
|   TUPLE5 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->
    _Block itms ((expr itms) lhs, (expr itms rhs))
|   TUPLE5 (Assignment, op, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) ->
    _Block itms ((expr itms) lhs, asgntyp itms ((expr itms) lhs) ((expr itms) rhs) op)
|   TUPLE2 (Int_typespec, _) -> _Place itms (789, Void 0, Void 0)
|   oth -> othpat := oth; failwith "pat"

and pat itms x = oldpat' := !oldpat; oldpat := x; patlst := x :: !patlst; let p = pat' itms x in patlst := List.tl !patlst; p

and top_pat itms x = oldpat' := !oldpat; oldpat := x; patlst := x :: !patlst; let p = top_pat' itms x in patlst := List.tl !patlst; p

and asgntyp itms lhs rhs = function
| Vpiaddop -> _Add itms (lhs, rhs)
| Vpisubop -> _Sub itms (lhs, rhs)
| Vpimultop -> _Mult itms (lhs, rhs)
| Vpidivop -> _Div itms (lhs, rhs)
| Vpimodop -> _Mod itms (lhs, rhs)
| Vpipowerop -> _Pow itms (lhs, rhs)
| Vpilshiftop -> _LshiftL itms (lhs, rhs)
| Vpiarithlshiftop -> _LshiftL itms (lhs, rhs)
| Vpirshiftop -> _LshiftR itms (lhs, rhs)
| Vpiarithrshiftop -> _AshiftR itms (lhs, rhs)
| Vpilogandop -> _LogAnd itms (lhs, rhs)
| Vpilogorop -> _LogOr itms (lhs, rhs)
| Vpibitandop -> _And itms (lhs, rhs)
| Vpibitorop -> _Or itms (lhs, rhs)
| Vpibitxorop -> _Xor itms (lhs, rhs)
| Vpibitxnorop -> _Xnor itms (lhs, rhs)
| Vpieqop -> _Eq itms (lhs, rhs)
| Vpineqop -> _Ne itms (lhs, rhs)
| Vpigeop -> _Ge itms (lhs, rhs)
| Vpiltop -> _Lt itms (lhs, rhs)
| Vpileop -> _Le itms (lhs, rhs)
| Vpigtop -> _Gt itms (lhs, rhs)
| oth -> othpat := oth; failwith "asgntyp"

and (seqlst:itms->token list->rw list) = fun itms lst -> List.filter (function (UNKNOWN|SCOPE "Place767"|SCOPE "") -> false | _ -> true) (List.map (pat itms) lst)

and (seq:itms->token list->rw) = fun itms lst -> _Seq itms (seqlst itms lst)
and _seq itms lst = _Seq itms (seqlst itms lst)
and (_pat:itms->token->rw) = fun itms x ->(pat itms) x

and (seqtok:itms->token->rw list) = fun itms t -> match seqlst itms [t] with (BGN (None,lst))::[] -> lst | hd::[] -> [hd] | oth -> oth

and expr itms = function
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST [])),
         STRING _, TLIST _))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))),
         STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpialways|Vpinet as typ))))) -> _Identrng typ itms s (typrng itms rng)
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned)))),
         STRING s, TLIST _, (Vpisigned as typ)))) -> _Identrng typ itms s (typrng itms rng)
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE3 (Logic_net,
         STRING _, TLIST _))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TLIST []))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways))))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
         TUPLE2 (Vpisize, Int _)))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
         STRING _, TLIST _))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))),
         STRING _, TLIST _))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE6 (Logic_var, Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TLIST [])),
         STRING _, TLIST _, TUPLE2 (Vpivisibility, Int 1)))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE6 (Logic_var, Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng))),
         STRING _, TLIST _, TUPLE2 (Vpivisibility, Int 1)))) -> _Ident itms s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways))))) -> _Ident itms s
|   TUPLE3 (Ref_obj, STRING s, TLIST _) -> _Ident itms s
|   TUPLE7 (Part_select, TUPLE2 (Vpiname, STRING part),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, lft),
     TUPLE2 (Vpirightrange, rght)) -> _Selection itms (_Ident itms part,  (expr itms lft),  (expr itms rght), 0, 0)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w),
     TUPLE2 (UINT, Int n), Vpiuintconst) -> _Dec itms (string_of_int n, w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), TUPLE2 (INT, Int n),
     Vpiintconst) -> _Integer itms n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN b, Vpibinaryconst) -> _Bin itms (b,w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), OCT o, Vpioctconst) -> _Oct itms (o,w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), DEC d, Vpidecconst) -> _Dec itms (d,w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), HEXS h, Vpihexconst) -> _Hex itms (h,w)
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), DEC d, TUPLE2(Vpitypespec, _), Vpidecconst) -> _Dec itms (d,w)
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n),
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> _Integer itms n
|   TUPLE4 (Vpiconditionop, cond, lft, rght) -> _Ternary itms (expr itms cond, expr itms lft, expr itms rght)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (UINT, Int n), Vpiuintconst))) -> _Bitsel itms (_Ident itms s, _Integer itms n)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex, ix)) -> _Bitsel itms (_Ident itms s, (expr itms) ix)
|   TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), lft, rght) -> asgntyp itms (expr itms lft) (expr itms rght) op
|   TUPLE2 (Vpieventorop, TLIST lst) -> _Edge itms (List.map (expr itms) lst)
|   TUPLE3 (Vpieventorop, a, b) -> _Edge itms ((expr itms) a :: (expr itms) b :: [])
|   TUPLE2 (Vpibitnegop, a) -> _Lneg itms ((expr itms) a)
|   TUPLE2 (Vpiplusop, a) -> expr itms a
|   TUPLE2 (Vpiminusop, a) -> UNRY(Unegate, expr itms a :: [])
|   TUPLE2 (Vpinotop, a) -> UNRY(Unot, expr itms a :: [])
|   TUPLE2 ((Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop as op), a) -> LOGIC(_Unary op, expr itms a :: [])
|   TUPLE2 (Vpiconcatop, TLIST lst) -> concat (List.map (expr itms) lst)
|   TUPLE2 (Vpicondition, c) -> expr itms c
|   TUPLE2 (Vpiposedgeop, p) -> _Posedge itms (expr itms p)
|   TUPLE3 (Sys_func_call,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE4 (Logic_net, STRING s, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> _Ident itms s
|   TUPLE3 (Sys_func_call, exp, STRING syscall) -> SYS("", syscall, expr itms exp :: [])
|   TUPLE6 (Parameter, _, _, _, _, _) as par -> let param, _, n = parameter itms par in _Integer itms n
|   TUPLE2 (Ref_obj, STRING s) -> _Ident itms s
|   TUPLE2 (Vpimulticoncatop, TLIST lst) -> concat_multi (List.map (expr itms) lst)
|   oth -> othpat := oth; failwith "expr"

and typrng itms = function
|   Logic_net -> TYPNONE
|   Logic_var -> TYPNONE
|   TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght)) -> _Range itms (expr itms lft, expr itms rght)
|   TUPLE2 (Vpielemtypespec,
     TUPLE2 (Ref_typespec,
       TUPLE2 (Vpiactual,
         TUPLE3 (Logic_typespec, LOC _, rng)))) -> typrng itms rng
|   TUPLE2 (Vpielemtypespec,
    TUPLE2 (Ref_typespec,
      TUPLE2 (Vpiactual,
        TUPLE4 (Logic_typespec, LOC _, (Logic_net|Logic_var), rng)))) -> typrng itms rng
|   oth -> othpat := oth; failwith "typrng"

and dirmap = function
| Vpiinput -> Dinput
| Vpioutput -> Doutput
| oth -> othpat := oth; failwith "dirmap"

and parameter itms = function
|   TUPLE6 (Parameter,
      (TUPLE2 (UINT, Int n) as p), Vpitypespec,
      TUPLE3 (Ref_typespec,
        TLIST _,
        TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, Work))),
      STRING param,
      TLIST _) -> param, p, n
| oth -> othpat := oth; failwith "parameter othpat"

and fresh topmod dest types body =
      let uitms = empty_itms [] in
      let types' = List.rev types in
      crntp := types;
      List.iter (top_pat uitms) types';
      let body' = List.rev body in
      crntp := body;
      let _ = List.map (pat uitms) body' in
      dest := (topmod, (types' @ body', uitms)) :: !dest      

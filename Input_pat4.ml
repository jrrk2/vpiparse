open Input
open Input_types
open Input_dump
open Dump_types

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

let _Concat (_, a, b) = CAT ("", [a; b])
let rec concat op = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> _Concat(op, hd, concat op tl)

let _Void itms n = othvoid := List.hd !patlst; UNKNOWN

let othstmt = ref Work
let othstmtlst = ref []

let _Always itms (deplst, body) = match deplst with
| SNTRE items -> itms.alwys := ("", COMB, (SNTRE items :: body)) :: !(itms.alwys); UNKNOWN
| POSPOS (clk, rst) as edg -> itms.alwys := ("", edg, body) :: !(itms.alwys); UNKNOWN
| POSEDGE (clk) as edg -> itms.alwys := ("", edg, body) :: !(itms.alwys); UNKNOWN
| deplst -> othalwys := (deplst, body); failwith "_Always"

let rec parse_base base r =
  let len = String.length r in
  if len = 0 then 0 else base * parse_base base (String.sub r 0 (len-1)) + (match r.[0] with
 | '0'..'9' -> Char.code r.[len-1] - Char.code '0'
 | _ -> 0)

let rec parse_bin b = parse_base 2 b
let rec parse_oct o = parse_base 8 o
let rec parse_dec d = parse_base 10 d

let _Class itms = SCOPE "Class"
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
let _Selection itms (nam, lft, rght, _, _) = IRNG (nam, [rght; lft])
let _Update itms (_, _, _, _, _) = failwith "Update"
let _Bitsel itms (a, b) = SEL ("", a :: b :: _Integer itms 1 :: [])
let _Unary itms (_, _) = failwith "Unary"
let _Dyadic itms (_, _, _) = failwith "Dyadic"

let _Case itms (exp, lst) = CS ("", List.filter (function UNKNOWN -> false | _ -> true) (List.map (function
    | UNKNOWN -> UNKNOWN
    | SCOPE "Place767" -> UNKNOWN
    | VRF _ as v -> v
    | CSITM _ as c -> c
    | oth -> othpat' := oth; failwith "tran case othpat'") (exp :: lst)))

let _Item itms (_, n, stmt) = CSITM("", n :: stmt :: [])
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
itms.v := (nam, ("", (BASDTYP, (identypmap typ), TYPNONE, []), (identypmap typ), (UNKDTYP, "", TYPNONE, []))) :: !(itms.v); UNKNOWN
| oth -> identyp := oth; failwith "identyp"

let _Port itms dir nam = itms.io := (nam, ("", (BASDTYP, "logic", TYPNONE, []), dir, "logic", [])) :: !(itms.io); UNKNOWN
let _Portrng itms dir nam typrng = itms.io := (nam, ("", (BASDTYP, "logic", typrng, []), dir, "logic", [])) :: !(itms.io); UNKNOWN
let _Porthigh itms dir nam = itms.v := (nam, ("", (BASDTYP, "logic", TYPNONE, []), "logic", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v); UNKNOWN

let othenum' = ref UNKNOWN

let _Enumvar itms nam = function
| BGN (None, VRF (typdef, _, _) :: lst) as typ -> othenum' := typ; List.iter (function
  | VRF ("", typ', []) -> itms.v := (nam, ("", typ', typdef, (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
  | oth -> ()) lst; UNKNOWN
| oth -> othenum' := oth; failwith "_Enumvar"

let _Porthighrng itms dir nam typrng =
    if not (List.mem_assoc nam !(itms.io)) then
 itms.v := (nam, ("", (BASDTYP, "logic", typrng, []), "logic", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v); UNKNOWN
let _Enum itms enum_t lst =
  if not (List.mem_assoc enum_t !(itms.cnst)) then itms.cnst := (enum_t,(false,(0,CNSTEXP(Aunknown, List.map (function
  | TUPLE5 (Enum_const, STRING enam, TUPLE2 (INT, Int n), Vpidecompile _, TUPLE2 (Vpisize, Int _)) -> ENUMVAL (n, enam)
  | _ -> STRING "unknown") lst)))) :: !(itms.cnst)
let _Param itms param =
  List.iter (function
    | Vpiparent -> ()
    | TUPLE2 (UINT, Int n) -> if not (List.mem_assoc param !(itms.cnst)) then itms.cnst := (param, (false, (0,HEX n))) :: !(itms.cnst)
    | Vpitypespec -> ()
    | TUPLE3 (Ref_typespec, TLIST _, _) -> ()
    | STRING s -> ()
    | TLIST _ -> ()
    | oth -> othpat := oth; failwith "_Param")
let _Not itms _ = failwith "Not"
let _Lneg itms a = UNRY (Unegate, a :: [])
let _Aneg itms _ = failwith "Aneg"
let _Sneg itms _ = failwith "Sneg"
let _Mux itms (_, _) = failwith "Mux"
let _Add itms (a, b) = ARITH(Aadd, [a;b])
let _Sub itms (a, b) = ARITH(Asub, [a;b])
let _Mult itms (a, b) = ARITH(Amul, [a;b])
let _Div itms (a, b) = ARITH(Aunknown, [a;b])
let _Mod itms (a, b) = ARITH(Aunknown, [a;b])
let _Pow itms (a, b) = ARITH(Aunknown, [a;b])
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
let _Gen_case itms lst = itms.gen := ("", lst) :: !(itms.gen); UNKNOWN
let _Posedge itms = function
| VRF (id, (Dump_types.BASDTYP, "logic", TYPNONE, []), []) -> POSEDGE id
| oth -> oth' := oth; failwith "_Posedge oth'"
let rec _cnstexpr = function
| CNST (w, lft) -> lft
| ARITH(arithop, args) -> CNSTEXP(arithop, List.map _cnstexpr args)
| VRF (param, (Dump_types.BASDTYP, "logic", TYPNONE, []), []) -> STRING param
| SYS ("", syscall, args) -> CNSTEXP(Aunknown, STRING syscall :: List.map _cnstexpr args)
| oth -> oth' := oth; failwith "_cnstexpr oth'"
let _Range itms (lft, rght) = TYPRNG(_cnstexpr lft, _cnstexpr rght)
let _Place itms (n, _, _) = SCOPE ("Place"^string_of_int n)
let _Array_var itms _ = failwith "Array_var"
let otharray = ref None
let _Array_net itms = function
| VRF (nam, (BASDTYP, "logic", TYPNONE, []), []),
  VRF ("", (BASDTYP, "logic", typrng, []), []),
  siz ->
 itms.v := (nam, ("", (UNPACKADTYP, "",
         RECTYP (BASDTYP, "logic", typrng, []),
         [TYPRNG (SHEX 0, SHEX (siz-1))]),
		      "", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v); UNKNOWN
| a -> otharray := Some a; failwith "actual"
let _Edge itms = function
| [POSEDGE clk; POSEDGE rst] -> POSPOS (clk, rst)
| VRF _ :: _ as lst -> SNTRE lst
| oth -> othlst' := oth; failwith "_Edge oth'"
let _ArrayRange itms = function
| TYPRNG(lo,hi) as rng, (TYPRNG(lft,rght) as elm) -> VRF ("", (UNPACKADTYP, "logic", rng, [elm]), [])
| oth -> othrng2' := oth; failwith "_ArrayRange othrng2'"
let _TypespecRange itms = function
| TYPRNG(lft,rght) as rng -> VRF ("", (Dump_types.BASDTYP, "logic", rng, []), [])
| oth -> othrng' := oth; failwith "_TypespecRange othrng'"

let _Asgn itms = function
| (lft, rght) -> ASGN(true, "", [rght;lft])

let _Cont_assign itms = function
| (lft, rght) -> itms.ca := ("", rght , lft) :: !(itms.ca); UNKNOWN

let _Block itms (a, b) = ASGN(false, "", [b;a])

let rec pat' itms = function
|   TUPLE2 (Vpiprocess, proc) -> pat itms proc
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) ->
        let rng' = typrng itms rng in
        let _ = _Porthighrng itms typ nam rng' in
        _Identrng typ itms nam rng'
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned)))),
       STRING nam, TLIST _, Vpisigned)) -> _Identrng Vpisigned itms nam (typrng itms rng)
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST []))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Identyp itms nam typ
|   TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> _Void itms     11
|   TUPLE7 (Part_select, TUPLE2 (Vpiname, STRING part),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, lft),
     TUPLE2 (Vpirightrange, rght)) -> _Selection itms (part,  (pat itms lft),  (pat itms rght), 0, 0)
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n),
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> _Integer itms n
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
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n), Vpiuintconst) -> _Integer itms n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), TUPLE2 (INT, Int n),
     Vpiintconst) -> _Integer itms n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN b, Vpibinaryconst) -> _Bin itms (b,w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), OCT o, Vpioctconst) -> _Oct itms (o,w)
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), HEXS h, Vpihexconst) -> _Hex itms (h,w)
|   TUPLE4 (Vpiconditionop, cond, lft, rght) -> _Ternary itms (pat itms cond, pat itms lft, pat itms rght)
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
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> _Void    itms 201
|   TUPLE4 (Logic_typespec, LOC _, (Logic_net|Logic_var), rng) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE4 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> _Void    itms 219
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void    itms 220
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void    itms 222
|   TUPLE4 (Input.If_else, cond, then_, else_) -> _If_ itms ((pat itms) cond, (pat itms) then_, (pat itms) else_)
|   TUPLE4 (Gen_scope_array, STRING lbl, TLIST _, TUPLE2(Gen_scope, TLIST (TLIST _ :: lst))) -> List.iter (function
  | TUPLE2 (Vpiprocess, TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
    TUPLE3 (Event_control,
	TUPLE2 (Vpicondition, cond), TUPLE3(Begin, TLIST _, TLIST (Vpiparent::TLIST _::lst))))) ->
	    let _ =  _Always itms (pat itms cond, seqlst itms lst) in (); (* failwith "Event339" *)
  | TUPLE2 (Vpiprocess, p) -> oldpat := p; failwith "proc"
  | TUPLE2 (Vpinet, _) as vpi -> let _ = pat itms vpi in ()
  | TUPLE2 (Vpivariables, _) as vpi -> let _ = pat itms vpi in ()
  | oth -> oldpat := oth; failwith "gen_scope") lst; UNKNOWN
|   TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> _Place itms (225, Void 0, Void 0)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (UINT, Int n), Vpiuintconst))) -> _Bitsel itms (_Ident itms s, _Integer itms n)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex, ix)) -> _Bitsel itms (_Ident itms s, (pat itms) ix)
|   TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop as op), lft, rght) -> asgntyp itms (pat itms lft) (pat itms rght) op
|   TUPLE3 (Vpiparameter, STRING param, TLIST lst) -> _Param itms param lst; UNKNOWN
|   TUPLE3 (Vpineqop, a, b) -> _Ne itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpilshiftop, a, b) -> _LshiftL itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpilogandop, a, b) -> _LogAnd itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpigeop, a, b) -> _Ge itms ((pat itms) a, (pat itms) b)
|   TUPLE2 (Vpieventorop, TLIST lst) -> _Edge itms (seqlst itms lst)
|   TUPLE3 (Vpieventorop, a, b) -> _Edge itms ((pat itms) a :: (pat itms) b :: [])
|   TUPLE3 (Vpieqop, a, b) -> _Eq itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpibitxorop, a, b) -> _Xor itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpibitorop, a, b) -> _Or itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Sys_func_call,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE4 (Logic_net, STRING s, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> _Ident itms s
|   TUPLE3 (Sys_func_call, exp, STRING syscall) -> SYS("", syscall, pat itms exp :: [])
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, s))) -> _Place itms (291, (pat itms) s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, s))) -> _Place itms (294, (pat itms) s, Void 0)
|   TUPLE2 (Array_typespec, Work) -> failwith "Array_typespec"
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST (Vpirange :: elem)))) -> _Void    itms 300
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST (TUPLE3(Vpirange,_,_) as rng :: elem)))) -> _Void    itms 300
|   TUPLE3 (Ref_obj, STRING s, TLIST _) -> _Ident itms s
|   TUPLE3 (Ref_module, TUPLE3 (STRING _, STRING _, LOC _), TLIST lst) ->seq itms lst
|   TUPLE3 (Named_begin, TLIST _, TLIST lst) -> _seq itms lst
|   TUPLE3 (Named_begin, STRING _, TLIST lst) -> _seq itms lst
|   TUPLE3 (Logic_typespec, LOC _, Logic_net) -> _Place itms (308, Void 0, Void 0)
|   TUPLE3 (Logic_typespec, LOC _, rng) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond), then_) -> _If itms ((pat itms) cond, (pat itms) then_)
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> _Void    itms 321
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> _Void    itms 324
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    itms 327
|   TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) -> _Cont_assign itms (pat itms rhs, pat itms lhs)
|   TUPLE3 (Class_defn, Queue, Queue) -> _Void    itms 329
|   TUPLE3 (Class_defn, Array, Array) -> _Void    itms 330
|   TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class) -> _Void    itms 331
|   TUPLE3 (Class_defn, STRING _, STRING _) -> _Void    itms 332
|   TUPLE3 (Case_item,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst),
     TUPLE3 (Named_begin, STRING _, TLIST _)) -> _Void    itms 338
|   TUPLE3 (Case_item,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE4 (Assignment, _, _, _)) -> _Void    itms 344
|   TUPLE3 (Case_item,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    itms 350
|   TUPLE3 (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE4 (Assignment, _, _, _)) -> _Void    itms 360
|   TUPLE3 (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    itms 370
|   TUPLE3 (Begin, _, TLIST lst) -> _seq itms lst
|   TUPLE3 (STRING _, STRING _, LOC _) -> _Void    itms 373
|   TUPLE2 (Weaklyreferenced, TLIST lst) -> seq itms lst      
|   TUPLE2 (Vpivisibility, Int _) -> _Void    itms 375
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
          TLIST _)))) -> UNKNOWN

|   TUPLE2 (Vpivariables,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _)) -> _Ident itms s
|   TUPLE2 (Vpivariables,
     TUPLE6 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))),
       STRING s, TLIST _,  TUPLE2 (Vpivisibility, Int _))) -> _Ident itms s
|   TUPLE2 (Vpivariables,
     TUPLE6 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _,  TUPLE2 (Vpivisibility, Int _))) -> _Ident itms s
|   TUPLE2 (Vpivariables,
     TUPLE5 (Enum_var,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, typ)))),
       STRING s, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> _Enumvar itms s (pat itms typ)
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> _Void    itms 398
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> _Void    itms 403
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC _, rng)))) -> _TypespecRange itms (typrng itms rng)
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec,
	TLIST (STRING enum_t :: TUPLE2 (Vpiinstance, TLIST _) :: lst))) -> _Enum itms enum_t lst; UNKNOWN
|   TUPLE2 (Vpitopmodule, Int n) -> _Place itms (416, Integer n, Void 0)
|   TUPLE2 (Vpitop, Int n) -> _Place itms (417, Integer n, Void 0)
|   TUPLE2 (Vpisize, Int _) -> _Void    itms 418
|   TUPLE2 (Vpisigned, Int _) -> _Void    itms 419
|   TUPLE2 (Vpirightrange, rhs) -> (_pat itms) rhs
|   TUPLE2 (Vpirhs, rhs) -> (_pat itms) rhs
|   TUPLE4 (Logic_typespec, LOC _, rng, Vpisigned) -> _Identrng Vpisigned itms "$logic_typespec" (typrng itms rng)
|   TUPLE5 (Logic_typespec, LOC _, Logic_net, rng, Vpisigned) -> _Identrng Vpisigned itms "$logic_typespec" (typrng itms rng)
|   TUPLE2 (Vpireg,
     TUPLE4 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, (Logic_net|Logic_var), rng))),
       TLIST pth)) -> _Identrng Vpireg itms (match List.hd pth with STRING s -> s | _ -> failwith "Vpitypespec") (typrng itms rng)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
		     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond), TUPLE3 (Begin, TLIST _, TLIST (Vpiparent :: TLIST [] :: lst )))) ->
		     othstmtlst := lst;
		     _Always itms ((pat itms) cond, seqlst itms lst)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
		     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond), stmt)) ->
		     othstmt := stmt;
		     _Always itms ((pat itms) cond, seqtok itms stmt)
|     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE2 (Event_control,
         TUPLE3 (Begin, TLIST _, TLIST lst))) -> _Always itms (_Edge itms [], seqlst itms lst)
|   TUPLE2 (Vpiposedgeop, p) -> _Posedge itms (pat itms p)
|   TUPLE2 (Vpiparamassign, TLIST lst) -> _Place itms (488, seq itms lst, Void 0)
|   TUPLE2 (Vpioverriden, Int n) -> _Place itms (489, Integer n, Void 0)
|   TUPLE2 (Vpinettype, Vpireg) -> _Void    itms 490
|   TUPLE2 (Vpinettype, Vpinet) -> _Void    itms 491
|   TUPLE2 (Vpinettype, Vpialways) -> _Void    itms 492
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _)) -> _Enum itms s []; UNKNOWN
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng)))),
       STRING s, TLIST _)) -> _Ident itms s
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpinet|Vpireg|Vpialways)))) -> _Ident itms s
|   TUPLE2 (Vpinet,
     TUPLE3 (Logic_net, STRING s, TLIST _)) -> _Ident itms s
|   TUPLE2 (Vpinet,
    TUPLE6 (Array_net, TUPLE2 (Vpisize, Int siz), mem,
      TLIST _,
      Vpirange,
      TUPLE2 (Vpinet,
        TUPLE4 (Logic_net,
          TUPLE2 (Vpitypespec,
            TUPLE3 (Ref_typespec,
              TLIST _,
              TUPLE2 (Vpiactual, actual))),
          TLIST _,
          TUPLE2 (Vpinettype, Vpireg))))) -> _Array_net itms (pat itms mem, pat itms actual, siz)
|   TUPLE2 (Vpiname, Semaphore) -> _Void    itms 540
|   TUPLE2 (Vpiname, Process) -> _Void    itms 541
|   TUPLE2 (Vpiname, Mailbox) -> _Void    itms 542
|   TUPLE2 (Vpiname, STRING _) -> _Void    itms 543
|   TUPLE2 (Vpimodule, TLIST lst) -> _seq itms lst
|   TUPLE2 (Vpimethod, Task) -> _Void    itms 545
|   TUPLE2 (Vpimethod, Function) -> _Void    itms 546
|   TUPLE2 (Vpilowconn, conn) -> (_pat itms) conn
|   TUPLE2 (Vpilhs, lhs) -> (_pat itms) lhs
|   TUPLE2 (Vpileftrange, lft) -> _pat itms lft
|   TUPLE2 (Vpiinstance, TLIST lst) -> _seq itms lst
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
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST lst)) -> _Gen_case itms (List.map (pat itms) lst)
|   TUPLE2 (Vpigenscopearray, arg) -> pat itms arg
|   TUPLE2 (Vpifullname, TLIST _) -> _Void    itms 584
|   TUPLE2 (Vpielaborated, Int _) -> _Void    itms 596
|   TUPLE2 (Vpidirection, Vpioutput) -> _Void    itms 597
|   TUPLE2 (Vpidirection, Vpiinput) -> _Void    itms 598
|   TUPLE2 (Vpidefname, STRING _) -> _Void    itms 599
|   TUPLE2 (Vpideflineno, Int _) -> _Place itms (600, Void 0, Void 0)
|   TUPLE2 (Vpiconstantselect, Int _) -> _Void    itms 601
|   TUPLE2 (Vpicondition, c) -> (_pat itms) c
|   TUPLE2 (Vpiconcatop as op, TLIST lst) -> concat op (seqlst itms lst)
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)) -> _Void    itms 604
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)) -> _Void    itms 605
|   TUPLE2 (Vpiclassdefn,
     TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)) -> _Void    itms 608
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)) -> _Void    itms 609
|   TUPLE2 (Vpicasetype, Int n) -> _Void itms n
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w),
         TUPLE2 (UINT, Int n), Vpiuintconst),
       stmt)) -> _Item itms (_Void itms 0, _Dec itms (string_of_int n,w), (pat itms) stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN s, Vpibinaryconst),
       stmt)) -> _Item itms (_Void itms 0, _Bin itms (s,w), (pat itms) stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5
            (Enum_const, STRING _, TUPLE2 (INT, Int n), Vpidecompile _,
             TUPLE2 (Vpisize, Int w)))),
       stmt)) -> _Item itms (_Void itms 0, _Dec itms (string_of_int n, w), (pat itms) stmt)
|   TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, stmt)) -> _Item itms (_Void itms 0, _Void itms 0, (pat itms) stmt)
|   TUPLE2 (Vpiblocking, Int _) -> _Void    itms 666
|   TUPLE2 (Vpibitnegop, a) -> _Lneg itms ((pat itms) a)
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
|   TUPLE2 (Uhdmtoppackages,
     TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
       TUPLE2 (Vpiclassdefn,
         TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)))) -> _Place itms (750, Void 0, Void 0)
|   TUPLE2 ((Uhdmtopmodules|Uhdmallmodules), TLIST rawlst) ->
    let fresh topmod dest lst =
      let uitms = empty_itms [] in
      let lst' = List.rev lst in
      crntp := lst;
      let _ = seq uitms lst' in
      dest := (topmod, (lst', uitms)) :: !dest in
        (match List.partition (function TUPLE2 ((Vpitypedef|Vpiparamassign|Vpivariables), _) | TUPLE3 (Vpiparameter, _,_ ) -> true | _ -> false) rawlst with
          | types, Vpiparent :: TLIST [] :: (STRING topmod) :: body -> fresh (topmod) allmods (types@body)
          | types, Vpiname :: (STRING topmod) :: body -> fresh (topmod) topmods (types@body)
          | oth -> othmap := oth; failwith "map'");
      UNKNOWN
|   TUPLE2 (Uhdmallpackages, TUPLE4 (Package, Builtin, STRING _, Builtin)) -> _Place itms (752, Void 0, Void 0)
|   TUPLE2 (Uhdmallclasses,
     TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
       TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task))) -> _Place itms (761, Void 0, Void 0)
|   TUPLE2 (Uhdmallclasses,
     TUPLE6 (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class itms
|    TUPLE2 (Uhdmallclasses,
     TUPLE10 (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class itms
|   TUPLE2 (UINT, Int n) -> _Integer itms n
|   TUPLE2 (Ref_typespec,
     TUPLE2 (Vpiactual,
       TUPLE3 (Logic_typespec, LOC _, rng))) -> _Type_spec_rng itms (typrng itms rng)
|   TUPLE2 (Ref_obj, STRING s) -> _Ident itms s
|   TUPLE2 (Vpiport, Port) -> _Place itms (452, Void 0, Void 0)
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
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING s, TUPLE2 (Vpihighconn, _))) -> _Place itms (462, Ident s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, s))) -> _Place itms (486, (pat itms) s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING s)) -> _Place itms (487, Ident s, Void 0)
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
|   TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> _Void    itms 787
|   TUPLE2 (Port, STRING _) -> _Void    itms 788
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int n)) -> _Integer itms n
|   TUPLE2 (INT, Int _) -> _Void    itms 791
|   TUPLE2 (Enum_typespec, _) -> _Void    itms 793
|   TUPLE2 (Case_stmt, TLIST lst) -> _Case itms (_Void itms 0, seqlst itms lst)
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
| Cont_assign -> _Place itms (    766, Void 0, Void 0)
| Vpiparent -> _Place itms (    767, Void 0, Void 0)
| Vpirange -> _Place itms (    768, Void 0, Void 0)
|   Work -> _Place itms (    769, Void 0, Void 0)
| Vpiname -> _Place itms (    770, Void 0, Void 0)
| Vpitypespec -> _Place itms (754, Void 0, Void 0)
| STRING s -> _Ident itms s
|   Class_typespec -> _Place itms (654, Void 0, Void 0)
|   Vpideffile -> _Place itms (655, Void 0, Void 0)
|   Int_typespec -> _Place itms (    834, Void 0, Void 0)
|   Parameter -> _Place itms (    835, Void 0, Void 0)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) -> _Asgn itms ((pat itms) lhs, (pat itms) rhs)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs,
    TUPLE6(Array_var, TUPLE2(Vpitypespec, t), STRING s, TLIST _, Vpiarraytype, (TUPLE2(Vpireg, _) as r)))) ->
          _Block_array itms (pat itms t) s (pat itms r)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs, lhs)) -> failwith "blocking"
|   TUPLE5 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->
    _Block itms ((pat itms) lhs, (pat itms rhs))
|   TUPLE5 (Assignment, op, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) ->
    _Block itms ((pat itms) lhs, asgntyp itms ((pat itms) lhs) ((pat itms) rhs) op)
|   TUPLE2 (Int_typespec, _) -> _Place itms (789, Void 0, Void 0)
|   oth -> othpat := oth; failwith "pat"

and pat itms x = oldpat' := !oldpat; oldpat := x; patlst := x :: !patlst; let p = pat' itms x in patlst := List.tl !patlst; p

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
| oth -> failwith "asgntyp"

and (seqlst:itms->token list->rw list) = fun itms lst -> List.filter (function (UNKNOWN|SCOPE "Place767"|SCOPE "") -> false | _ -> true) (List.map (pat itms) lst)

and (seq:itms->token list->rw) = fun itms lst -> _Seq itms (seqlst itms lst)
and _seq itms lst = _Seq itms (seqlst itms lst)
and (_pat:itms->token->rw) = fun itms x ->(pat itms) x

and (seqtok:itms->token->rw list) = fun itms t -> match seqlst itms [t] with (BGN (None,lst))::[] -> lst | hd::[] -> [hd] | oth -> oth

and typrng itms = function
|   Logic_net -> TYPNONE
|   Logic_var -> TYPNONE
|   TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght)) -> _Range itms (pat itms lft, pat itms rght)
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

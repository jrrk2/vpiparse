open Input
open Input_types
open Input_dump
open Dump_types

let crntp = ref [Work]
let othedg = ref (Void 0)
let topmods = ref []
let allmods = ref []
let othmap = ref ([],[])
let othasgn = ref None
let oth' = ref UNKNOWN
let othlst' = ref [UNKNOWN]
let othpat = ref Work
let othpat' = ref UNKNOWN
let othpat2 = ref (UNKNOWN, UNKNOWN)
let othalwys = ref (UNKNOWN, [UNKNOWN])
let marklst = ref []
let mark n = if not (List.mem n !marklst) then marklst := n :: !marklst
let dump () = List.iter (fun n -> print_endline (string_of_int n)) (List.sort_uniq compare !marklst)

let _Concat (_, a, b) = CAT ("", [a; b])
let rec concat op = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> _Concat(op, hd, concat op tl)

let _Void itms n = UNKNOWN
let _Always itms (deplst, body) = match deplst with
| SNTRE items -> itms.alwys := ("", COMB, (SNTRE items :: body)) :: !(itms.alwys); UNKNOWN
| POSPOS (clk, rst) as edg -> itms.alwys := ("", edg, body) :: !(itms.alwys); UNKNOWN
| deplst -> othalwys := (deplst, body); failwith "_Always"

let _Class itms = SCOPE "Class"
let _Id itms _ = failwith "Id"
let _Alw itms _ = failwith "Alw"
let _Bin itms (s, w) = CNST (w, STRING s)
let _Oct itms (s, w) = CNST (w, STRING s)
let _Dec itms (s, w) = CNST (w, STRING s)
let _Seq itms lst = BGN(None, lst)
let _Hex itms (s, w) = CNST (w, try Scanf.sscanf s "%x" (fun n -> HEX n) with _ -> STRING s)
let _If_ itms (c, a, b) = IF ("", [c; a; b])
let _If itms (c, a) = IF ("", [c; a])
let _Mux2 itms (_, _, _) = failwith "Mux2"
let _Block itms (_, _) = failwith "Block"
let _Integer itms n = CNST(32, HEX n)
let _Selection itms (nam, lft, rght, _, _) = IRNG ("", List.map (fun n -> _Integer itms n) [lft; rght])
let _Update itms (_, _, _, _, _) = failwith "Update"
let _Bitsel itms (a, b) = SEL ("", a :: b :: [])
let _Unary itms (_, _) = failwith "Unary"
let _Dyadic itms (_, _, _) = failwith "Dyadic"

let _Case itms (exp, lst) = CS ("", List.filter (function UNKNOWN -> false | _ -> true) (List.map (function
    | UNKNOWN -> UNKNOWN
    | oth -> othpat' := oth; failwith "tran case'") (exp :: lst)))

let _Item itms (_, n, stmt) = CSITM("", n :: stmt :: [])
let _Signed itms _ = failwith "Signed"
let _Unsigned itms _ = failwith "Unsigned"
let _Conpp itms _ = failwith "Conpp"
let _Inppp itms _ = failwith "Inppp"
let _Inpspp itms _ = failwith "Inpspp"
let _Sigpp itms _ = failwith "Sigpp"
let _Sigspp itms _ = failwith "Sigspp"
let _Alwpp itms _ = failwith "Alwpp"
let _Regpp itms (_, _, _, _) = failwith "Regpp"
let _Itmpp itms _ = failwith "Itmpp"
let _Wirepp itms _ = failwith "Wirepp"
let _Othpp itms _ = failwith "Othpp"
let _Identrng typ itms nam typrng = VRF (nam, (BASDTYP, "logic", typrng, []), [])
let _Ident itms s = VRF (s, (BASDTYP, "logic", TYPNONE, []), [])
let _Identyp typ itms s = VRF (s, (BASDTYP, "logic", TYPNONE, []), [])
let _Port itms dir nam = itms.io := (nam, ("", (BASDTYP, "logic", TYPNONE, []), dir, "logic", [])) :: !(itms.io); UNKNOWN
let _Portrng itms dir nam typrng = itms.io := (nam, ("", (BASDTYP, "logic", typrng, []), dir, "logic", [])) :: !(itms.io); UNKNOWN
let _Enum itms e = itms.v := (e, ("", (BASDTYP, "logic", TYPNONE, []), "logic", (UNKDTYP, "", TYPNONE, []))) :: !(itms.v); UNKNOWN
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
let _Posedge itms = function
| VRF (id, (Dump_types.BASDTYP, "logic", TYPNONE, []), []) -> POSEDGE id
| oth -> oth' := oth; failwith "_Posedge oth'"
let _Range itms = function
| (CNST (32, lft), CNST (32, rght)) -> TYPRNG(lft,rght)
| (lft, rght) -> othpat2 := (lft, rght); failwith "_Range"
let _Place itms (n, _, _) = SCOPE ("Place"^string_of_int n)
let _Array_var itms _ = failwith "Array_var"
let _Edge itms = function
| [POSEDGE clk; POSEDGE rst] -> POSPOS (clk, rst)
| lst when false -> SNTRE lst
| oth -> othlst' := oth; print_endline "_Edge oth'"; UNKNOWN

let _Asgn itms (a, b) = ASGN(false, "", [b;a])
let _Block itms (a, b) = ASGN(false, "", [b;a])
let _Assign itms = function
| Ident a, b -> ASGN(false, a, b :: [])
| oth -> othasgn := Some oth; failwith "_Assign"

let rec pat itms = function
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Identrng typ itms nam (typrng itms rng)
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST []))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Identyp typ itms nam
|   TUPLE9
    (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> _Void itms     11
|   TUPLE7
    (Part_select, TUPLE2 (Vpiname, STRING part),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, lft),
     TUPLE2 (Vpirightrange, rght)) -> (match (pat itms) lft, (pat itms) rght with
     | CNST (32, HEX lft'), CNST (32, HEX rght') -> _Selection itms (Ident part, lft', rght', 0, 0)
     | oth -> othpat2 := oth; failwith "pat2")

|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void   itms  22
|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void     itms 28
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n),
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> _Integer itms n
|   TUPLE6
    (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
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
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> _Void     itms 63
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> _Void     itms 69
|   TUPLE5 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     STRING _, TLIST _) -> _Void     itms 75
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)) -> _Void     itms 83
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void     itms 91
|   TUPLE5 (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void     itms 99
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
|   TUPLE4 (Vpiconditionop, cond, lft, rght) -> _Void    itms 123
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
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
         STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpialways|Vpinet as typ))))) -> _Identrng typ itms s (typrng itms rng)
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> _Void    itms 157
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
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways))))) -> _Ident itms s
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> _Void    itms 201
|   TUPLE4 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     TLIST _) -> _Void    itms 207
|   TUPLE4 (Logic_typespec, LOC (_, _, _, _), Logic_net,
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> _Void    itms 211
|   TUPLE4 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> _Void    itms 219
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void    itms 220
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void    itms 222
|   TUPLE4 (Input.If_else, cond, then_, else_) -> _If_ itms ((pat itms) cond, (pat itms) then_, (pat itms) else_)
|   TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope) -> _Void    itms 224
|   TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> _Place itms (225, Void 0, Void 0)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (UINT, Int n), Vpiuintconst))) -> _Bitsel itms (_Ident itms s, _Integer itms n)
|   TUPLE4 (Bit_select, STRING _, TLIST _,
     TUPLE2 (Vpiindex,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5 (Logic_net,
             TUPLE2 (Vpitypespec,
               TUPLE3 (Ref_typespec, TLIST _,
                 TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
             STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)))))) -> _Void    itms 258
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex, ix)) -> _Bitsel itms (_Ident itms s, (pat itms) ix)
|   TUPLE3 (Vpisubop, lft, rght) -> _Void    itms 263
|   TUPLE3 (Vpiparameter, STRING _, TLIST lst) -> _seq itms lst
|   TUPLE3 (Vpineqop, a, b) -> _Ne itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpilshiftop, a, b) -> _LshiftL itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpilogandop, a, b) -> _LogAnd itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpigeop, a, b) -> _Ge itms ((pat itms) a, (pat itms) b)
|   TUPLE2 (Vpieventorop, TLIST lst) -> _Edge itms (seqlst itms lst)
|   TUPLE3 (Vpieventorop, a, b) -> _Edge itms ((pat itms) a :: (pat itms) b :: [])
|   TUPLE3 (Vpieqop, a, b) -> _Eq itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpibitxorop, a, b) -> _Xor itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpibitorop, a, b) -> _Or itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Vpiaddop, a, b) -> _Add itms ((pat itms) a, (pat itms) b)
|   TUPLE3 (Sys_func_call,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE4 (Logic_net, STRING s, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> _Ident itms s
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpisubop, _, _), STRING _) -> _Void    itms 286
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpiaddop, _, _), STRING _) -> _Void    itms 287
|   TUPLE3 (Sys_func_call, TUPLE2 (Vpiconcatop, TLIST _), STRING s) -> _Ident itms s
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, s))) -> _Place itms (291, (pat itms) s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, s))) -> _Place itms (294, (pat itms) s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))) -> _Void    itms 297
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _)))) -> _Void    itms 300
|   TUPLE3 (Ref_obj, STRING s, TLIST _) -> _Ident itms s
|   TUPLE3 (Ref_module, TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)), TLIST lst) ->seq itms lst
|   TUPLE3 (Port, STRING _, TUPLE2 (Vpihighconn, _)) -> _Void    itms 305
|   TUPLE3 (Named_begin, TLIST _, TLIST lst) -> _seq itms lst
|   TUPLE3 (Named_begin, STRING _, TLIST lst) -> _seq itms lst
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _), Logic_net) -> _Place itms (308, Void 0, Void 0)
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _),
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> _Place itms (312, (pat itms) lft, (pat itms) rght)
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond), then_) -> _If itms ((pat itms) cond, (pat itms) then_)
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> _Void    itms 321
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> _Void    itms 324
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    itms 327
|   TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) -> _Void    itms 328
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
|   TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)) -> _Void    itms 373
|   TUPLE2 (Weaklyreferenced, TLIST lst) -> seq itms lst      
|   TUPLE2 (Vpivisibility, Int _) -> _Void    itms 375
|   TUPLE2 (Vpivariables,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _)) -> _Ident itms s
|   TUPLE2 (Vpivariables,
     TUPLE5 (Enum_var,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> _Ident itms s
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> _Void    itms 398
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> _Void    itms 403
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void    itms 408
|   TUPLE2 (Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))))) -> _Void    itms 413
|   TUPLE2 (Vpitypedef, Enum_typespec) -> _Place itms (414, Void 0, Void 0)
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST lst)) -> _Seq itms (seqlst itms lst)
|   TUPLE2 (Vpitopmodule, Int n) -> _Place itms (416, Integer n, Void 0)
|   TUPLE2 (Vpitop, Int n) -> _Place itms (417, Integer n, Void 0)
|   TUPLE2 (Vpisize, Int _) -> _Void    itms 418
|   TUPLE2 (Vpisigned, Int _) -> _Void    itms 419
|   TUPLE2 (Vpirightrange, rhs) -> (_pat itms) rhs
|   TUPLE2 (Vpirhs, rhs) -> (_pat itms) rhs
|   TUPLE2 (Vpireg,
     TUPLE4 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       TLIST _)) -> _Void    itms 429
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond), stmt))) -> _Always itms ((pat itms) cond, seqtok itms stmt)
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE2 (Event_control,
         TUPLE3 (Begin, TLIST _, TLIST lst)))) -> _Always itms (_Edge itms [], seqlst itms lst)
|   TUPLE2 (Vpiposedgeop, p) -> _Posedge itms (pat itms p)
|   TUPLE2 (Vpiport, Port) -> _Place itms (452, Void 0, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Ident itms s
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng))))) -> _Portrng itms (dirmap dir) s (typrng itms rng)
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING s, TUPLE2 (Vpihighconn, _))) -> _Place itms (462, Ident s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, s))) -> _Place itms (486, (pat itms) s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING s)) -> _Place itms (487, Ident s, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))))) -> _Portrng itms (dirmap dir) s (typrng itms rng)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput as dir)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Port itms (dirmap dir) s
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
       STRING s, TLIST _)) -> _Enum itms s
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
|   TUPLE2 (Vpiname, Semaphore) -> _Void    itms 540
|   TUPLE2 (Vpiname, Process) -> _Void    itms 541
|   TUPLE2 (Vpiname, Mailbox) -> _Void    itms 542
|   TUPLE2 (Vpiname, STRING _) -> _Void    itms 543
|   TUPLE2 (Vpimodule, TLIST lst) -> _seq itms lst
|   TUPLE2 (Vpimethod, Task) -> _Void    itms 545
|   TUPLE2 (Vpimethod, Function) -> _Void    itms 546
|   TUPLE2 (Vpilowconn, conn) -> (_pat itms) conn
|   TUPLE2 (Vpilhs, lhs) -> (_pat itms) lhs
|   TUPLE2 (Vpileftrange, _) -> _Void    itms 549
|   TUPLE2 (Vpiinstance, TLIST lst) -> _seq itms lst
|   TUPLE2 (Vpiindex,
     TUPLE7 (Part_select, TUPLE2 (Vpiname, STRING _),
       TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
       TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
       TUPLE2 (Vpirightrange, _))) -> _Void    itms 557
|   TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst)) -> _Void    itms 562
|   TUPLE2 (Vpiindex,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE5 (Logic_net,
           TUPLE2 (Vpitypespec,
             TUPLE3 (Ref_typespec, TLIST _,
               TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
           STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))))) -> _Void    itms 577
|   TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _)) -> _Void    itms 578
|   TUPLE2 (Vpihighconn, _) -> _Void    itms 579
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST lst)) -> _seq itms lst
|   TUPLE2 (Vpigenscopearray,
     TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope)) -> _Place itms (    583, Void 0, Void 0)
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
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n),
         TUPLE2 (UINT, Int w), Vpiuintconst),
       stmt)) -> _Item itms (_Void itms 0, _Dec itms (string_of_int n,w), (pat itms) stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n), BIN s, Vpibinaryconst),
       stmt)) -> _Item itms (_Void itms 0, _Bin itms (s,n), (pat itms) stmt)
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
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       STRING _, TLIST _)) -> _Void    itms 676
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))) -> _Void    itms 686
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Void    itms 696
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Void    itms 706
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
       TUPLE2 (Vpisize, Int _))) -> _Void    itms 711
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> _Void    itms 721
|   TUPLE2
    (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Void    itms 724
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Void    itms 728
|   TUPLE2
    (Vpiactual,
     TUPLE3
      (Logic_typespec, LOC (_, _, _, _),
       TUPLE3
        (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _)))) -> _Void    itms 734
|   TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)) -> _Void    itms 735
|   TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)) -> _Void    itms 736
|   TUPLE2 (Vpiactual, TLIST lst) ->seq itms lst
|   TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))) -> _Void    itms 738
|   TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))) -> _Void    itms 739
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
          | types, Vpiparent :: TLIST [] :: (STRING topmod) :: body -> fresh ("all_"^topmod) allmods (types@body)
          | types, Vpiname :: (STRING topmod) :: body -> fresh ("top_"^topmod) topmods (types@body)
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
       TUPLE3 (Logic_typespec, LOC (_, _, _, _),
         TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, _),
           TUPLE2 (Vpirightrange, _))))) -> _Void    itms 785
|   TUPLE2 (Ref_obj, STRING s) -> _Ident itms s
|   TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> _Void    itms 787
|   TUPLE2 (Port, STRING _) -> _Void    itms 788
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int n)) -> _Integer itms n
|   TUPLE2 (INT, Int _) -> _Void    itms 791
|   TUPLE2 (Enum_typespec, _) -> _Void    itms 793
|   TUPLE2 (Case_stmt, TLIST lst) -> _Case itms (_Void itms 0, seqlst itms lst)
|   TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _)) -> _Void    itms 796
|   TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _)) -> _Void    itms 797
|   TUPLE2 (Array_typespec, TLIST lst) ->seq itms lst
|   TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)) -> _Void    itms 800
|   TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE4 (Input.If_else, _, _, _))) -> _Void    itms 805
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE3 (Named_begin, TLIST _, TLIST _))) -> _Void    itms 810
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
      TUPLE2 (Vpiactual,
        TUPLE3 (Logic_typespec, LOC _, rng)))) -> (_pat itms) rng
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
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs, lhs)) -> failwith "blocking"
|   TUPLE5 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->
    _Block itms ((pat itms) lhs, (pat itms rhs))
|   TUPLE5 (Assignment, op, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) ->
    _Block itms ((pat itms) lhs, asgntyp itms ((pat itms) lhs) ((pat itms) rhs) op)
|   TUPLE2 (Int_typespec, _) -> _Place itms (789, Void 0, Void 0)
|   oth -> othpat := oth; failwith "pat"

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

and (seqlst:itms->token list->rw list) = fun itms lst -> List.filter (function UNKNOWN -> false | _ -> true) (List.map (pat itms) lst)

and (seq:itms->token list->rw) = fun itms lst -> _Seq itms (seqlst itms lst)
and _seq itms lst = _Seq itms (seqlst itms lst)
and (_pat:itms->token->rw) = fun itms x ->(pat itms) x

and (seqtok:itms->token->rw list) = fun itms t -> match seqlst itms [t] with (BGN (None,lst))::[] -> lst | hd::[] -> [hd] | oth -> oth

and typrng itms = function
|   TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght)) -> _Range itms (pat itms lft, pat itms rght)
|   TUPLE2 (Vpielemtypespec,
     TUPLE2 (Ref_typespec,
       TUPLE2 (Vpiactual,
         TUPLE3 (Logic_typespec, LOC (_, _, _, _),
           TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft),
			     TUPLE2 (Vpirightrange, rght)))))) -> _Range itms (pat itms lft, pat itms rght)
|   Logic_net -> TYPNONE			     
|   oth -> othpat := oth; failwith "typrng"

and dirmap = function
| Vpiinput -> Dinput
| Vpioutput -> Doutput
| oth -> othpat := oth; failwith "dirmap"

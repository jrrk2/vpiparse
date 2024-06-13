open Input
open Input_types

let othpat = ref Work
let othpat2 = ref (Void 0, Void 0)
let marklst = ref []
let mark n = if not (List.mem n !marklst) then marklst := n :: !marklst
let dump () = List.iter (fun n -> print_endline (string_of_int n)) (List.sort_uniq compare !marklst)

let _Always (edge, body) = Always(edge, body)
let _Add(lhs, rhs) = Add(lhs, rhs)
let _And(lhs, rhs) = And(lhs, rhs)
let _Array_var s = Array_var s
let _Asgn(lhs, rhs) = Asgn(lhs, rhs)
let _AshiftR(lhs, rhs) = AshiftR(lhs, rhs)
let _Bin(b,w) = Bin(b,w)
let _Bitsel(Ident s, Integer n) = Bitsel(Ident s, Integer n)
let _Bitsel(Ident s, ix) = Bitsel(Ident s, ix)
let _Case(Void 0, lst) = Case(Void 0, lst)
let _Class = Class
let _Div(lhs, rhs) = Div(lhs, rhs)
let _Edge(lst) = Edge(lst)
let _Enum s = Enum s
let _Eq (a, b) = Eq (a, b)
let _Ge(a, b) = Ge(a, b)
let _Ident nam = Ident nam
let _If_ (cond, then_, else_) = If_ (cond, then_, else_)
(*
 let _concat op (lst) = Concat op (lst)
 *)
let _Integer n = Integer n
let _Item(Void 0, Bin (s,n), stmt) = Item(Void 0, Bin (s,n), stmt)
let _Lneg (a) = Lneg (a)
let _LogAnd(lhs, rhs) = LogAnd(lhs, rhs)
let _LogAnd(a, b) = LogAnd(a, b)
let _LogOr(lhs, rhs) = LogOr(lhs, rhs)
let _LshiftL(lhs, rhs) = LshiftL(lhs, rhs)
let _LshiftL(a, b) = LshiftL(a, b)
let _LshiftR(lhs, rhs) = LshiftR(lhs, rhs)
let _Mod(lhs, rhs) = Mod(lhs, rhs)
let _Mult(lhs, rhs) = Mult(lhs, rhs)
let _Ne(a, b) = Ne(a, b)
let _Or (a, b) = Or (a, b)
let _Or(lhs, rhs) = Or(lhs, rhs)
let _Place ( n, a, b) = Place ( n, a, b)
let _Port s = Port s
let _Posedge (p) = Posedge (p)
let _Pow(lhs, rhs) = Pow(lhs, rhs)
let _Range(lft, rght) = Range(lft, rght)
let _Selection (part, lft', rght', 0, 0) = Selection (part, lft', rght', 0, 0)
let _Seq(lst) = Seq(lst)
let _Sub(lhs, rhs) = Sub(lhs, rhs)
let _Void n = Void n
let _Xnor(lhs, rhs) = Xnor(lhs, rhs)
let _Xor (a, b) = Xor (a, b)

let rec pat = function
|   TUPLE9
    (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> _Void     11
|   TUPLE7
    (Part_select, TUPLE2 (Vpiname, STRING part),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, lft),
     TUPLE2 (Vpirightrange, rght)) -> (match pat lft, pat rght with
     | Integer lft', Integer rght' -> _Selection (Ident part, lft', rght', 0, 0)
     | oth -> othpat2 := oth; failwith "pat2")

|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void     22
|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void     28
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n),
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> _Integer n
|   TUPLE6
    (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> _Void     41
|   TUPLE6 (Array_var,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST lst)))),
     STRING s, TLIST _, Vpiarraytype,
     TUPLE2 (Vpireg,
       TUPLE4 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, Logic_net, rng))),
         TLIST _))) -> _Array_var s
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> _Void     63
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> _Void     69
|   TUPLE5 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     STRING _, TLIST _) -> _Void     75
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)) -> _Void     83
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void     91
|   TUPLE5 (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void     99
|   TUPLE5 (Enum_var,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _, TUPLE2 (Vpivisibility, Int _)) -> _Void    107
|   TUPLE5 (Enum_const, STRING s, TUPLE2 (INT, Int _), Vpidecompile _,
     TUPLE2 (Vpisize, Int _)) -> _Ident s
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n), Vpiuintconst) -> _Integer n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), TUPLE2 (INT, Int n),
     Vpiintconst) -> _Integer n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN b, Vpibinaryconst) -> _Bin(b,w)
|   TUPLE5 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int _), TUPLE2 (Vpirhs, rhs),
     TUPLE2 (Vpilhs, lhs)) -> _Void    122; pat lhs; pat rhs
|   TUPLE4 (Vpiconditionop, cond, lft, rght) -> _Void    123; pat lft; pat rght
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST [])),
         STRING _, TLIST _))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
         STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpialways|Vpinet))))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Logic_net,
         TUPLE2
          (Vpitypespec,
           TUPLE3
            (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> _Void    157
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TLIST []))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways))))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
         TUPLE2 (Vpisize, Int _)))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
         STRING _, TLIST _))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> _Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)))) -> _Ident s
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> _Void    201
|   TUPLE4 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     TLIST _) -> _Void    207
|   TUPLE4 (Logic_typespec, LOC (_, _, _, _), Logic_net,
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> _Void    211; pat lft; pat rght
|   TUPLE4 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> _Void    219
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> _Void    220
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> _Void    222
    |   TUPLE4 (Input.If_else, cond, then_, else_) -> _If_ (pat cond, seqtok then_, seqtok else_)
|   TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope) -> _Void    224
|   TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> _Place(225, Void 0, Void 0)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (UINT, Int n), Vpiuintconst))) -> _Bitsel(Ident s, Integer n)
|   TUPLE4 (Bit_select, STRING _, TLIST _,
     TUPLE2 (Vpiindex,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5 (Logic_net,
             TUPLE2 (Vpitypespec,
               TUPLE3 (Ref_typespec, TLIST _,
                 TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
             STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)))))) -> _Void    258
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex, ix)) -> _Bitsel(Ident s, pat ix)
|   TUPLE3 (Vpisubop, lft, rght) -> _Void    263; pat lft; pat rght
|   TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght)) -> _Range(pat lft, pat rght)
|   TUPLE3 (Vpiparameter, STRING _, TLIST lst) -> _seq lst
|   TUPLE3 (Vpineqop, a, b) -> _Ne(pat a, pat b)
|   TUPLE3 (Vpilshiftop, a, b) -> _LshiftL(pat a, pat b)
|   TUPLE3 (Vpilogandop, a, b) -> _LogAnd(pat a, pat b)
|   TUPLE3 (Vpigeop, a, b) -> _Ge(pat a, pat b)
|   TUPLE2 (Vpieventorop, TLIST lst) -> _Edge(seqlst lst)
|   TUPLE3 (Vpieventorop, a, b) -> _Edge(pat a :: pat b :: [])
|   TUPLE3 (Vpieqop, a, b) -> _Eq (pat a, pat b)
|   TUPLE3 (Vpibitxorop, a, b) -> _Xor (pat a, pat b)
|   TUPLE3 (Vpibitorop, a, b) -> _Or (pat a, pat b)
|   TUPLE3 (Vpiaddop, a, b) -> _Add (pat a, pat b)
|   TUPLE3 (Sys_func_call,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE4
          (Logic_net, STRING s, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> _Ident s
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpisubop, _, _), STRING _) -> _Void    286
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpiaddop, _, _), STRING _) -> _Void    287
|   TUPLE3 (Sys_func_call, TUPLE2 (Vpiconcatop, TLIST _), STRING s) -> _Ident s
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, s))) -> _Place(291, pat s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, s))) -> _Place(    294, pat s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))) -> _Void    297
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _)))) -> _Void    300
|   TUPLE3 (Ref_obj, STRING s, TLIST _) -> _Ident s
|   TUPLE3 (Ref_module, TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)), TLIST lst) ->seq lst
|   TUPLE3 (Port, STRING _, TUPLE2 (Vpihighconn, _)) -> _Void    305
|   TUPLE3 (Named_begin, TLIST _, TLIST lst) -> _seq lst
|   TUPLE3 (Named_begin, STRING _, TLIST lst) -> _seq lst
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _), Logic_net) -> _Place(   308, Void 0, Void 0)
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _),
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> _Place(312, pat lft, pat rght)
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond), then_) -> _If_ (pat cond, seqtok then_, [])
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond),
     TUPLE3 (Begin, TLIST _, TLIST lst)) -> _If_ (pat cond, seqlst lst, [])
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> _Void    321
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> _Void    324
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    327
|   TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) -> _Void    328; pat lhs; pat rhs
|   TUPLE3 (Class_defn, Queue, Queue) -> _Void    329
|   TUPLE3 (Class_defn, Array, Array) -> _Void    330
|   TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class) -> _Void    331
|   TUPLE3 (Class_defn, STRING _, STRING _) -> _Void    332
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst),
     TUPLE3 (Named_begin, STRING _, TLIST _)) -> _Void    338
|   TUPLE3 (Case_item,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE4 (Assignment, _, _, _)) -> _Void    344
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    350
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE4 (Assignment, _, _, _)) -> _Void    360
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    370
|   TUPLE3 (Begin, _, TLIST lst) -> _seq lst
|   TUPLE3 (Begin, TLIST _, TLIST lst) -> _seq lst
|   TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)) -> _Void    373
|   TUPLE2 (Weaklyreferenced, TLIST lst) ->seq lst
|   TUPLE2 (Vpivisibility, Int _) -> _Void    375
|   TUPLE2 (Vpivariables,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _)) -> _Ident s
|   TUPLE2 (Vpivariables,
     TUPLE5 (Enum_var,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> _Ident s
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> _Void    398
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> _Void    403
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> _Void    408
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))))) -> _Void    413
|   TUPLE2 (Vpitypedef, Enum_typespec) -> _Place(    414, Void 0, Void 0)
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST lst)) -> _Seq(seqlst lst)
|   TUPLE2 (Vpitopmodule, Int n) -> _Place(   416, Integer n, Void 0)
|   TUPLE2 (Vpitop, Int n) -> _Place(   417, Integer n, Void 0)
|   TUPLE2 (Vpisize, Int _) -> _Void    418
|   TUPLE2 (Vpisigned, Int _) -> _Void    419
|   TUPLE2 (Vpirightrange, rhs) -> _pat rhs
|   TUPLE2 (Vpirhs, rhs) -> _pat rhs
|   TUPLE2 (Vpireg,
     TUPLE4 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       TLIST _)) -> _Void    429
|   TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
       TUPLE3 (Begin, TLIST _, TLIST lst))) -> _Always (pat cond, seqlst lst)
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE4 (Input.If_else, cond_, then_, else_)))) -> _Always (pat cond, [If_ (pat cond_, [pat then_], [pat else_])])
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE3 (Named_begin, TLIST _, TLIST lst)))) -> _Always (pat cond, seqlst lst)
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE3 (Begin, TLIST _, TLIST lst)))) -> _Always (pat cond, seqlst lst)
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE2 (Event_control,
         TUPLE3 (Begin, TLIST _, TLIST lst)))) -> _Always (Edge [], seqlst lst)
|   TUPLE2 (Vpiposedgeop, p) -> _Posedge (pat p)
|   TUPLE2 (Vpiport, Port) -> _Place(    452, Void 0, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Ident s
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng))))) -> _Port s
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING s, TUPLE2 (Vpihighconn, _))) -> _Port s
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, s))) -> _Place(    486, pat s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING s)) -> _Place(    487, Ident s, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))))) -> _Port s
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> _Port s
|   TUPLE2 (Vpiparamassign, TLIST lst) -> _Place(   488, seq lst, Void 0)
|   TUPLE2 (Vpioverriden, Int n) -> _Place(    489, Integer n, Void 0)
|   TUPLE2 (Vpinettype, Vpireg) -> _Void    490
|TUPLE2 (Vpinettype, Vpinet) -> _Void    491
|   TUPLE2 (Vpinettype, Vpialways) -> _Void    492
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Ident nam
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST []))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> _Ident nam
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _)) -> _Enum s
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Ident s
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Ident s
|   TUPLE2 (Vpiname, Semaphore) -> _Void    540
|   TUPLE2 (Vpiname, Process) -> _Void    541
|   TUPLE2 (Vpiname, Mailbox) -> _Void    542
|   TUPLE2 (Vpiname, STRING _) -> _Void    543
|   TUPLE2 (Vpimodule, TLIST lst) -> _seq lst
|   TUPLE2 (Vpimethod, Task) -> _Void    545
|   TUPLE2 (Vpimethod, Function) -> _Void    546
|   TUPLE2 (Vpilowconn, conn) -> _pat conn
|   TUPLE2 (Vpilhs, lhs) -> _pat lhs
|   TUPLE2 (Vpileftrange, _) -> _Void    549
|   TUPLE2 (Vpiinstance, TLIST lst) -> _seq lst
|   TUPLE2
    (Vpiindex,
     TUPLE7
      (Part_select, TUPLE2 (Vpiname, STRING _),
       TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
       TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
       TUPLE2 (Vpirightrange, _))) -> _Void    557
|   TUPLE2
    (Vpiindex,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst)) -> _Void    562
|   TUPLE2 (Vpiindex,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE5 (Logic_net,
           TUPLE2 (Vpitypespec,
             TUPLE3 (Ref_typespec, TLIST _,
               TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
           STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))))) -> _Void    577
|   TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _)) -> _Void    578
|   TUPLE2 (Vpihighconn, _) -> _Void    579
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST lst)) -> _seq lst
|   TUPLE2 (Vpigenscopearray,
     TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope)) -> _Place (    583, Void 0, Void 0)
|   TUPLE2 (Vpifullname, TLIST _) -> _Void    584
|   TUPLE2 (Vpielemtypespec,
     TUPLE2 (Ref_typespec,
       TUPLE2 (Vpiactual,
         TUPLE3 (Logic_typespec, LOC (_, _, _, _),
           TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft),
             TUPLE2 (Vpirightrange, rght)))))) -> _Range(pat lft, pat rght)
|   TUPLE2 (Vpielaborated, Int _) -> _Void    596
|   TUPLE2 (Vpidirection, Vpioutput) -> _Void    597
|   TUPLE2 (Vpidirection, Vpiinput) -> _Void    598
|   TUPLE2 (Vpidefname, STRING _) -> _Void    599
|   TUPLE2 (Vpideflineno, Int _) -> _Place(    600, Void 0, Void 0)
|   TUPLE2 (Vpiconstantselect, Int _) -> _Void    601
|   TUPLE2 (Vpicondition, c) -> _pat c
|   TUPLE2 (Vpiconcatop as op, TLIST lst) -> Input_pp.concat op (seqlst lst)
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)) -> _Void    604
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)) -> _Void    605
|   TUPLE2 (Vpiclassdefn,
     TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)) -> _Void    608
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)) -> _Void    609
|   TUPLE2 (Vpicasetype, Int n) -> _Void n
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n),
         TUPLE2 (UINT, Int w), Vpiuintconst),
       stmt)) -> _Item(Void 0, Dec (string_of_int n,w), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n), BIN s, Vpibinaryconst),
       stmt)) -> _Item(Void 0, Bin (s,n), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _, Vpibinaryconst),
       TUPLE3 (Begin, TLIST _, TLIST _))) -> _Void    634
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5
            (Enum_const, STRING _, TUPLE2 (INT, Int n), Vpidecompile _,
             TUPLE2 (Vpisize, Int w)))),
       stmt)) -> _Item(Void 0, Dec (string_of_int n, w), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5 (Enum_const, STRING s, TUPLE2 (INT, Int _), Vpidecompile _,
             TUPLE2 (Vpisize, Int _)))),
       TUPLE3 (Begin, TLIST _, TLIST lst))) -> _Item(Void 0, Ident s, seq lst)
|   TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, stmt)) -> _Item(Void 0, Void 0, pat stmt)
|   TUPLE2 (Vpiblocking, Int _) -> _Void    666
|   TUPLE2 (Vpibitnegop, a) -> _Lneg (pat a)
|   TUPLE2 (Vpialwaystype, Vpialways) -> _Void    668
|   TUPLE2 (Vpiactual,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       STRING _, TLIST _)) -> _Void    676
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))) -> _Void    686
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Void    696
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Void    706
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
       TUPLE2 (Vpisize, Int _))) -> _Void    711
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> _Void    721
|   TUPLE2
    (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> _Void    724
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> _Void    728
|   TUPLE2
    (Vpiactual,
     TUPLE3
      (Logic_typespec, LOC (_, _, _, _),
       TUPLE3
        (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _)))) -> _Void    734
|   TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)) -> _Void    735
|   TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)) -> _Void    736
|   TUPLE2 (Vpiactual, TLIST lst) ->seq lst
|   TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))) -> _Void    738
|   TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))) -> _Void    739
|   TUPLE2 (Uhdmtoppackages,
     TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
       TUPLE2 (Vpiclassdefn,
         TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)))) -> _Place(    750, Void 0, Void 0)
|   TUPLE2 (Uhdmtopmodules, TLIST lst) ->seq lst
|   TUPLE2 (Uhdmallpackages, TUPLE4 (Package, Builtin, STRING _, Builtin)) -> _Place(    752, Void 0, Void 0)
|   TUPLE2 (Uhdmallmodules, TLIST lst) ->seq lst
|   TUPLE2 (Uhdmallclasses,
     TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
       TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task))) -> _Place(    761, Void 0, Void 0)
|   TUPLE2 (Uhdmallclasses,
     TUPLE6 (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class
|   TUPLE2 (Uhdmallclasses,
     TUPLE10 (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> _Class
|   TUPLE2 (UINT, Int n) -> _Integer n
|   TUPLE2 (Ref_typespec,
     TUPLE2 (Vpiactual,
       TUPLE3 (Logic_typespec, LOC (_, _, _, _),
         TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, _),
           TUPLE2 (Vpirightrange, _))))) -> _Void    785
|   TUPLE2 (Ref_obj, STRING s) -> _Ident s
|   TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> _Void    787
|   TUPLE2 (Port, STRING _) -> _Void    788
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int n)) -> _Integer n
|   TUPLE2 (INT, Int _) -> _Void    791
|   TUPLE2 (Gen_case, TLIST _) -> _Void    792
|   TUPLE2 (Enum_typespec, _) -> _Void    793
|   TUPLE2 (Enum_typespec, TLIST _) -> _Void    794
|   TUPLE2 (Case_stmt, TLIST lst) -> _Case(Void 0, seqlst lst)
|   TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _)) -> _Void    796
|   TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _)) -> _Void    797
|   TUPLE2 (Case_item, TUPLE3 (Begin, TLIST _, TLIST _)) -> _Void    798
|   TUPLE2 (Array_typespec, TLIST lst) ->seq lst
|   TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)) -> _Void    800
|   TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE4 (Input.If_else, _, _, _))) -> _Void    805
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE3 (Named_begin, TLIST _, TLIST _))) -> _Void    810
|   TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
     TUPLE2 (Vpiclassdefn,
       TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _))) -> _Void    824
|   TUPLE10
    (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> _Void    830
|   TUPLE2 (Vpitypespec,
    TUPLE3 (Ref_typespec, TLIST _,
      TUPLE2 (Vpiactual,
        TUPLE3 (Logic_typespec, LOC _, rng)))) -> _pat rng
|   TUPLE2 (Vpitypespec,
    TUPLE3 (Ref_typespec, TLIST _,
      TUPLE2 (Vpiactual, TLIST []))) -> _Place( 681, Void 0, Void 0)
|   TLIST lst -> _seq lst
|   Constant -> _Place(    765, Void 0, Void 0)
| Cont_assign -> _Place (    766, Void 0, Void 0)
| Vpiparent -> _Place (    767, Void 0, Void 0)
| Vpirange -> _Place (    768, Void 0, Void 0)
|   Work -> _Place (    769, Void 0, Void 0)
| Vpiname -> _Place (    770, Void 0, Void 0)
| Vpitypespec -> _Place( 754, Void 0, Void 0)
| STRING s -> _Ident s
|   Class_typespec -> _Place( 654, Void 0, Void 0)
|   Vpideffile -> _Place( 655, Void 0, Void 0)
|   Int_typespec -> _Place (    834, Void 0, Void 0)
|   Parameter -> _Place (    835, Void 0, Void 0)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) -> _Asgn(pat lhs, pat rhs)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs, lhs)) -> failwith "blocking"
|   TUPLE5 (Assignment, op, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) ->
    Asgn(pat lhs, asgntyp (pat lhs) (pat rhs) op)
|   TUPLE2 (Int_typespec, _) -> _Place(    789, Void 0, Void 0)
|   oth -> othpat := oth; failwith "pat"

and asgntyp lhs rhs = function
| Vpiaddop -> _Add(lhs, rhs)
| Vpisubop -> _Sub(lhs, rhs)
| Vpimultop -> _Mult(lhs, rhs)
| Vpidivop -> _Div(lhs, rhs)
| Vpimodop -> _Mod(lhs, rhs)
| Vpipowerop -> _Pow(lhs, rhs)
| Vpilshiftop -> _LshiftL(lhs, rhs)
| Vpiarithlshiftop -> _LshiftL(lhs, rhs)
| Vpirshiftop -> _LshiftR(lhs, rhs)
| Vpiarithrshiftop -> _AshiftR(lhs, rhs)
| Vpilogandop -> _LogAnd(lhs, rhs)
| Vpilogorop -> _LogOr(lhs, rhs)
| Vpibitandop -> _And(lhs, rhs)
| Vpibitorop -> _Or(lhs, rhs)
| Vpibitxorop -> _Xor(lhs, rhs)
| Vpibitxnorop -> _Xnor(lhs, rhs)

and seqlst (lst:token list) = List.filter (function Place (767, _, _) -> false | _ -> true) (List.map pat lst)

and seq lst = Seq (seqlst lst)
and _seq lst = Seq (seqlst lst)
and _pat x = pat x

and seqtok (t:token) = match seqlst [t] with (Seq lst)::[] -> lst | hd::[] -> [hd] | oth -> oth

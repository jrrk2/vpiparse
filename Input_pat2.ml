open Input
open Input_types

let othpat = ref Work
let othpat2 = ref (Void 0, Void 0)
let marklst = ref []
let mark n = if not (List.mem n !marklst) then marklst := n :: !marklst
let dump () = List.iter (fun n -> print_endline (string_of_int n)) (List.sort_uniq compare !marklst)

let rec pat = function
|   TUPLE9
    (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> Void     11
|   TUPLE7
    (Part_select, TUPLE2 (Vpiname, STRING part),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, lft),
     TUPLE2 (Vpirightrange, rght)) -> (match pat lft, pat rght with
     | Integer lft', Integer rght' -> Selection (Ident part, lft', rght', 0, 0)
     | oth -> othpat2 := oth; failwith "pat2")

|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> Void     22
|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> Void     28
|   TUPLE6 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n),
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> Integer n
|   TUPLE6
    (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> Void     41
|   TUPLE6 (Array_var,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Array_typespec, TLIST lst)))),
     STRING s, TLIST _, Vpiarraytype,
     TUPLE2 (Vpireg,
       TUPLE4 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE4 (Logic_typespec, LOC _, Logic_net, rng))),
         TLIST _))) -> Array_var s
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> Void     63
|   TUPLE5 (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))) -> Void     69
|   TUPLE5 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     STRING _, TLIST _) -> Void     75
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)) -> Void     83
|   TUPLE5 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> Void     91
|   TUPLE5 (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> Void     99
|   TUPLE5 (Enum_var,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _, TUPLE2 (Vpivisibility, Int _)) -> Void    107
|   TUPLE5 (Enum_const, STRING s, TUPLE2 (INT, Int _), Vpidecompile _,
     TUPLE2 (Vpisize, Int _)) -> Ident s
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int n), Vpiuintconst) -> Integer n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), TUPLE2 (INT, Int n),
     Vpiintconst) -> Integer n
|   TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN b, Vpibinaryconst) -> Bin(b,w)
|   TUPLE5 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int _), TUPLE2 (Vpirhs, rhs),
     TUPLE2 (Vpilhs, lhs)) -> Void    122; pat lhs; pat rhs
|   TUPLE4 (Vpiconditionop, cond, lft, rght) -> Void    123; pat lft; pat rght
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_var, Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST [])),
         STRING _, TLIST _))) -> Ident s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
         STRING s, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpialways|Vpinet))))) -> Ident s
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
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> Void    157
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TLIST []))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways))))) -> Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE5 (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
         TUPLE2 (Vpisize, Int _)))) -> Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net,
         TUPLE2 (Vpitypespec,
           TUPLE3 (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
         STRING _, TLIST _))) -> Ident s
|   TUPLE4 (Ref_obj, STRING _, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> Ident s
|   TUPLE4 (Ref_obj, STRING s, TLIST _,
     TUPLE2 (Vpiactual,
       TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)))) -> Ident s
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> Void    201
|   TUPLE4 (Logic_var, Vpitypespec,
     TUPLE3 (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     TLIST _) -> Void    207
|   TUPLE4 (Logic_typespec, LOC (_, _, _, _), Logic_net,
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> Void    211; pat lft; pat rght
|   TUPLE4 (Logic_net,
     TUPLE2 (Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> Void    219
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> Void    220
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> Void    222
    |   TUPLE4 (Input.If_else, cond, then_, else_) -> If_ (pat cond, seqtok then_, seqtok else_)
|   TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope) -> Void    224
|   TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> Place(225, Void 0, Void 0)
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (UINT, Int n), Vpiuintconst))) -> Bitsel(Ident s, Integer n)
|   TUPLE4 (Bit_select, STRING _, TLIST _,
     TUPLE2 (Vpiindex,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5 (Logic_net,
             TUPLE2 (Vpitypespec,
               TUPLE3 (Ref_typespec, TLIST _,
                 TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
             STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)))))) -> Void    258
|   TUPLE4 (Bit_select, STRING s, TLIST _, TUPLE2 (Vpiindex, ix)) -> Bitsel(Ident s, pat ix)
|   TUPLE3 (Vpisubop, lft, rght) -> Void    263; pat lft; pat rght
|   TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght)) -> Range(pat lft, pat rght)
|   TUPLE3 (Vpiparameter, STRING _, TLIST lst) -> seq lst
|   TUPLE3 (Vpineqop, a, b) -> Ne(pat a, pat b)
|   TUPLE3 (Vpilshiftop, a, b) -> LshiftL(pat a, pat b)
|   TUPLE3 (Vpilogandop, a, b) -> LogAnd(pat a, pat b)
|   TUPLE3 (Vpigeop, a, b) -> Ge(pat a, pat b)
|   TUPLE2 (Vpieventorop, TLIST lst) -> Edge(seqlst lst)
|   TUPLE3 (Vpieventorop, a, b) -> Edge(pat a :: pat b :: [])
|   TUPLE3 (Vpieqop, a, b) -> Eq (pat a, pat b)
|   TUPLE3 (Vpibitxorop, a, b) -> Xor (pat a, pat b)
|   TUPLE3 (Vpibitorop, a, b) -> Or (pat a, pat b)
|   TUPLE3 (Vpiaddop, a, b) -> Add (pat a, pat b)
|   TUPLE3 (Sys_func_call,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE4
          (Logic_net, STRING s, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> Ident s
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpisubop, _, _), STRING _) -> Void    286
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpiaddop, _, _), STRING _) -> Void    287
|   TUPLE3 (Sys_func_call, TUPLE2 (Vpiconcatop, TLIST _), STRING s) -> Ident s
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, s))) -> Place(291, pat s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _, TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, s))) -> Place(    294, pat s, Void 0)
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))) -> Void    297
|   TUPLE3 (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _)))) -> Void    300
|   TUPLE3 (Ref_obj, STRING s, TLIST _) -> Ident s
|   TUPLE3 (Ref_module, TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)), TLIST lst) ->seq lst
|   TUPLE3 (Port, STRING _, TUPLE2 (Vpihighconn, _)) -> Void    305
|   TUPLE3 (Named_begin, TLIST _, TLIST lst) -> seq lst
|   TUPLE3 (Named_begin, STRING _, TLIST lst) -> seq lst
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _), Logic_net) -> Place(   308, Void 0, Void 0)
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _),
     TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft), TUPLE2 (Vpirightrange, rght))) -> Place(312, pat lft, pat rght)
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond), then_) -> If_ (pat cond, seqtok then_, [])
|   TUPLE3 (If_stmt, TUPLE2 (Vpicondition, cond),
     TUPLE3 (Begin, TLIST _, TLIST lst)) -> If_ (pat cond, seqlst lst, [])
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> Void    321
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> Void    324
|   TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> Void    327
|   TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) -> Void    328; pat lhs; pat rhs
|   TUPLE3 (Class_defn, Queue, Queue) -> Void    329
|   TUPLE3 (Class_defn, Array, Array) -> Void    330
|   TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class) -> Void    331
|   TUPLE3 (Class_defn, STRING _, STRING _) -> Void    332
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst),
     TUPLE3 (Named_begin, STRING _, TLIST _)) -> Void    338
|   TUPLE3 (Case_item,
     TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE4 (Assignment, _, _, _)) -> Void    344
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> Void    350
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE4 (Assignment, _, _, _)) -> Void    360
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> Void    370
|   TUPLE3 (Begin, _, TLIST lst) -> seq lst
|   TUPLE3 (Begin, TLIST _, TLIST lst) -> seq lst
|   TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)) -> Void    373
|   TUPLE2 (Weaklyreferenced, TLIST lst) ->seq lst
|   TUPLE2 (Vpivisibility, Int _) -> Void    375
|   TUPLE2 (Vpivariables,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])),
       STRING s, TLIST _)) -> Ident s
|   TUPLE2 (Vpivariables,
     TUPLE5 (Enum_var,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> Ident s
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> Void    398
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> Void    403
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> Void    408
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))))) -> Void    413
|   TUPLE2 (Vpitypedef, Enum_typespec) -> Place(    414, Void 0, Void 0)
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST lst)) -> Seq(seqlst lst)
|   TUPLE2 (Vpitopmodule, Int n) -> Place(   416, Integer n, Void 0)
|   TUPLE2 (Vpitop, Int n) -> Place(   417, Integer n, Void 0)
|   TUPLE2 (Vpisize, Int _) -> Void    418
|   TUPLE2 (Vpisigned, Int _) -> Void    419
|   TUPLE2 (Vpirightrange, rhs) -> pat rhs
|   TUPLE2 (Vpirhs, rhs) -> pat rhs
|   TUPLE2
    (Vpireg,
     TUPLE4
      (Logic_var, Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       TLIST _)) -> Void    429
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE4 (Input.If_else, cond_, then_, else_)))) -> Always (pat cond, [If_ (pat cond_, [pat then_], [pat else_])])
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE3 (Named_begin, TLIST _, TLIST lst)))) -> Always (pat cond, seqlst lst)
|   TUPLE2 (Vpiprocess,
     TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
         TUPLE3 (Begin, TLIST _, TLIST lst)))) -> Always (pat cond, seqlst lst)
|   TUPLE2 (Vpiposedgeop, p) -> Posedge (pat p)
|   TUPLE2 (Vpiport, Port) -> Place(    452, Void 0, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> Ident s
|   TUPLE2 (Vpiport,
     TUPLE5 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpilowconn, _),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng))))) -> Port s
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING s, TUPLE2 (Vpihighconn, _))) -> Port s
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, s))) -> Place(    486, pat s, Void 0)
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING s)) -> Place(    487, Ident s, Void 0)
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE3(Logic_typespec, LOC _, rng))))) -> Port s
|   TUPLE2 (Vpiport,
     TUPLE6 (Port, STRING s, TUPLE2 (Vpidirection, (Vpiinput|Vpioutput)),
       TUPLE2 (Vpihighconn, high),
       TUPLE2 (Vpilowconn, low),
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TLIST [])))) -> Port s
|   TUPLE2 (Vpiparamassign, TLIST lst) -> Place(   488, seq lst, Void 0)
|   TUPLE2 (Vpioverriden, Int n) -> Place(    489, Integer n, Void 0)
|   TUPLE2 (Vpinettype, Vpireg) -> Void    490
|TUPLE2 (Vpinettype, Vpinet) -> Void    491
|   TUPLE2 (Vpinettype, Vpialways) -> Void    492
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE3 (Logic_typespec, LOC (_, _, _, _), rng)))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> Ident nam
|   TUPLE2 (Vpinet,
     TUPLE5 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TLIST []))),
       STRING nam, TLIST _, TUPLE2 (Vpinettype, (Vpireg|Vpinet|Vpialways as typ)))) -> Ident nam
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net,
       TUPLE2 (Vpitypespec,
         TUPLE3 (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING s, TLIST _)) -> Enum s
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> Ident s
|   TUPLE2 (Vpinet,
     TUPLE4 (Logic_net, STRING s, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> Ident s
|   TUPLE2 (Vpiname, Semaphore) -> Void    540
|   TUPLE2 (Vpiname, Process) -> Void    541
|   TUPLE2 (Vpiname, Mailbox) -> Void    542
|   TUPLE2 (Vpiname, STRING _) -> Void    543
|   TUPLE2 (Vpimodule, TLIST lst) -> seq lst
|   TUPLE2 (Vpimethod, Task) -> Void    545
|   TUPLE2 (Vpimethod, Function) -> Void    546
|   TUPLE2 (Vpilowconn, conn) -> pat conn
|   TUPLE2 (Vpilhs, lhs) -> pat lhs
|   TUPLE2 (Vpileftrange, _) -> Void    549
|   TUPLE2 (Vpiinstance, TLIST lst) -> seq lst
|   TUPLE2
    (Vpiindex,
     TUPLE7
      (Part_select, TUPLE2 (Vpiname, STRING _),
       TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
       TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
       TUPLE2 (Vpirightrange, _))) -> Void    557
|   TUPLE2
    (Vpiindex,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst)) -> Void    562
|   TUPLE2 (Vpiindex,
     TUPLE4 (Ref_obj, STRING _, TLIST _,
       TUPLE2 (Vpiactual,
         TUPLE5 (Logic_net,
           TUPLE2 (Vpitypespec,
             TUPLE3 (Ref_typespec, TLIST _,
               TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
           STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))))) -> Void    577
|   TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _)) -> Void    578
|   TUPLE2 (Vpihighconn, _) -> Void    579
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST lst)) -> seq lst
|   TUPLE2 (Vpigenscopearray,
     TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope)) -> Place (    583, Void 0, Void 0)
|   TUPLE2 (Vpifullname, TLIST _) -> Void    584
|   TUPLE2 (Vpielemtypespec,
     TUPLE2 (Ref_typespec,
       TUPLE2 (Vpiactual,
         TUPLE3 (Logic_typespec, LOC (_, _, _, _),
           TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, lft),
             TUPLE2 (Vpirightrange, rght)))))) -> Range(pat lft, pat rght)
|   TUPLE2 (Vpielaborated, Int _) -> Void    596
|   TUPLE2 (Vpidirection, Vpioutput) -> Void    597
|   TUPLE2 (Vpidirection, Vpiinput) -> Void    598
|   TUPLE2 (Vpidefname, STRING _) -> Void    599
|   TUPLE2 (Vpideflineno, Int _) -> Place(    600, Void 0, Void 0)
|   TUPLE2 (Vpiconstantselect, Int _) -> Void    601
|   TUPLE2 (Vpicondition, c) -> pat c
|   TUPLE2 (Vpiconcatop as op, TLIST lst) -> Input_pp.concat op (seqlst lst)
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)) -> Void    604
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)) -> Void    605
|   TUPLE2 (Vpiclassdefn,
     TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)) -> Void    608
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)) -> Void    609
|   TUPLE2 (Vpicasetype, Int n) -> Void n
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n),
         TUPLE2 (UINT, Int w), Vpiuintconst),
       stmt)) -> Item(Void 0, Dec (string_of_int n,w), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int n), BIN s, Vpibinaryconst),
       stmt)) -> Item(Void 0, Bin (s,n), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _, Vpibinaryconst),
       TUPLE3 (Begin, TLIST _, TLIST _))) -> Void    634
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5
            (Enum_const, STRING _, TUPLE2 (INT, Int n), Vpidecompile _,
             TUPLE2 (Vpisize, Int w)))),
       stmt)) -> Item(Void 0, Dec (string_of_int n, w), pat stmt)
|   TUPLE2 (Vpicaseitem,
     TUPLE3 (Case_item,
       TUPLE4 (Ref_obj, STRING _, TLIST _,
         TUPLE2 (Vpiactual,
           TUPLE5 (Enum_const, STRING s, TUPLE2 (INT, Int _), Vpidecompile _,
             TUPLE2 (Vpisize, Int _)))),
       TUPLE3 (Begin, TLIST _, TLIST lst))) -> Item(Void 0, Ident s, seq lst)
|   TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, stmt)) -> Item(Void 0, Void 0, pat stmt)
|   TUPLE2 (Vpiblocking, Int _) -> Void    666
|   TUPLE2 (Vpibitnegop, a) -> Lneg (pat a)
|   TUPLE2 (Vpialwaystype, Vpialways) -> Void    668
|   TUPLE2 (Vpiactual,
     TUPLE5 (Logic_var, Vpitypespec,
       TUPLE3 (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       STRING _, TLIST _)) -> Void    676
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))) -> Void    686
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> Void    696
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> Void    706
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
       TUPLE2 (Vpisize, Int _))) -> Void    711
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> Void    721
|   TUPLE2
    (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> Void    724
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> Void    728
|   TUPLE2
    (Vpiactual,
     TUPLE3
      (Logic_typespec, LOC (_, _, _, _),
       TUPLE3
        (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _)))) -> Void    734
|   TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)) -> Void    735
|   TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)) -> Void    736
|   TUPLE2 (Vpiactual, TLIST lst) ->seq lst
|   TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))) -> Void    738
|   TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))) -> Void    739
|   TUPLE2 (Uhdmtoppackages,
     TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
       TUPLE2 (Vpiclassdefn,
         TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)))) -> Place(    750, Void 0, Void 0)
|   TUPLE2 (Uhdmtopmodules, TLIST lst) ->seq lst
|   TUPLE2 (Uhdmallpackages, TUPLE4 (Package, Builtin, STRING _, Builtin)) -> Place(    752, Void 0, Void 0)
|   TUPLE2 (Uhdmallmodules, TLIST lst) ->seq lst
|   TUPLE2 (Uhdmallclasses,
     TUPLE9 (Class_defn, TUPLE2 (Vpiname, Process),
       TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task))) -> Place(    761, Void 0, Void 0)
|   TUPLE2 (Uhdmallclasses,
     TUPLE6 (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> Class
|   TUPLE2 (Uhdmallclasses,
     TUPLE10 (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> Class
|   TUPLE2 (UINT, Int n) -> Integer n
|   TUPLE2 (Ref_typespec,
     TUPLE2 (Vpiactual,
       TUPLE3 (Logic_typespec, LOC (_, _, _, _),
         TUPLE3 (Vpirange, TUPLE2 (Vpileftrange, _),
           TUPLE2 (Vpirightrange, _))))) -> Void    785
|   TUPLE2 (Ref_obj, STRING s) -> Ident s
|   TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> Void    787
|   TUPLE2 (Port, STRING _) -> Void    788
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int n)) -> Integer n
|   TUPLE2 (INT, Int _) -> Void    791
|   TUPLE2 (Gen_case, TLIST _) -> Void    792
|   TUPLE2 (Enum_typespec, _) -> Void    793
|   TUPLE2 (Enum_typespec, TLIST _) -> Void    794
|   TUPLE2 (Case_stmt, TLIST lst) -> Case(Void 0, seqlst lst)
|   TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _)) -> Void    796
|   TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _)) -> Void    797
|   TUPLE2 (Case_item, TUPLE3 (Begin, TLIST _, TLIST _)) -> Void    798
|   TUPLE2 (Array_typespec, TLIST lst) ->seq lst
|   TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)) -> Void    800
|   TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3 (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE4 (Input.If_else, _, _, _))) -> Void    805
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE3 (Named_begin, TLIST _, TLIST _))) -> Void    810
|   TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3 (Event_control, TUPLE2 (Vpicondition, cond),
       TUPLE3 (Begin, TLIST _, TLIST lst))) -> Always (pat cond, seqlst lst)
|   TUPLE10 (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
     TUPLE2 (Vpiclassdefn,
       TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _))) -> Void    824
|   TUPLE10
    (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> Void    830
|   TUPLE2 (Vpitypespec,
    TUPLE3 (Ref_typespec, TLIST _,
      TUPLE2 (Vpiactual,
        TUPLE3 (Logic_typespec, LOC _, rng)))) -> pat rng
|   TUPLE2 (Vpitypespec,
    TUPLE3 (Ref_typespec, TLIST _,
      TUPLE2 (Vpiactual, TLIST []))) -> Place( 681, Void 0, Void 0)
|   TLIST lst -> seq lst
|   Constant -> Place(    765, Void 0, Void 0)
| Cont_assign -> Place (    766, Void 0, Void 0)
| Vpiparent -> Place (    767, Void 0, Void 0)
| Vpirange -> Place (    768, Void 0, Void 0)
|   Work -> Place (    769, Void 0, Void 0)
| Vpiname -> Place (    770, Void 0, Void 0)
| Vpitypespec -> Place( 754, Void 0, Void 0)
| STRING s -> Ident s
|   Class_typespec -> Place( 654, Void 0, Void 0)
|   Vpideffile -> Place( 655, Void 0, Void 0)
|   Int_typespec -> Place (    834, Void 0, Void 0)
|   Parameter -> Place (    835, Void 0, Void 0)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2(Vpirhs, rhs), TUPLE2(Vpilhs, lhs)) -> Asgn(pat lhs, pat rhs)
|   TUPLE4 (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int 1), TUPLE2(Vpilhs, lhs)) -> Place(645, pat lhs, Void 0)
|   TUPLE2 (Int_typespec, _) -> Place(    789, Void 0, Void 0)
|   oth -> othpat := oth; failwith "pat"

and seqlst (lst:token list) = List.filter (function Void 767 -> false | _ -> true) (List.map pat lst)

and seq lst = Seq (seqlst lst)

and seqtok (t:token) = match seqlst [t] with (Seq lst)::[] -> lst | hd::[] -> [hd] | oth -> oth

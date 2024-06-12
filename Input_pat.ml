open Input

let othpat = ref Work

let rec match_pat = function
|   TUPLE9
    (Class_defn, TUPLE2 (Vpiname, Process),
     TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task)) -> ()
|   TUPLE7
    (Part_select, TUPLE2 (Vpiname, STRING _),
     TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
     TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
     TUPLE2 (Vpirightrange, _)) -> ()
|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE6
    (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE6
    (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int _),
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))),
     Vpiuintconst) -> ()
|   TUPLE6
    (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> ()
|   TUPLE6
    (Array_var,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, Vpiarraytype,
     TUPLE2
      (Vpireg,
       TUPLE4
        (Logic_var, Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
         TLIST _))) -> ()
|   TUPLE5
    (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE5
    (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
     TUPLE2 (Vpilowconn, _),
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE5
    (Logic_var, Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     STRING _, TLIST _) -> ()
|   TUPLE5
    (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)) -> ()
|   TUPLE5
    (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> ()
|   TUPLE5
    (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
     STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> ()
|   TUPLE5
    (Enum_var,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _, TUPLE2 (Vpivisibility, Int _)) -> ()
|   TUPLE5
    (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
     TUPLE2 (Vpisize, Int _)) -> ()
|   TUPLE5
    (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
     TUPLE2 (UINT, Int _), Vpiuintconst) -> ()
|   TUPLE5
    (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), TUPLE2 (INT, Int _),
     Vpiintconst) -> ()
|   TUPLE5
    (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
     Vpibinaryconst) -> ()
|   TUPLE5
    (Assignment, Vpirhs, TUPLE2 (Vpiblocking, Int _), TUPLE2 (Vpirhs, _),
     TUPLE2 (Vpilhs, _)) -> ()
|   TUPLE4 (Vpiconditionop, _, _, _) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Logic_var, Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
         STRING _, TLIST _))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Logic_net,
         TUPLE2
          (Vpitypespec,
           TUPLE3
            (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Logic_net,
         TUPLE2
          (Vpitypespec,
           TUPLE3
            (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Logic_net,
         TUPLE2
          (Vpitypespec,
           TUPLE3
            (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
         STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE5
        (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
         TUPLE2 (Vpisize, Int _)))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE4
        (Logic_net,
         TUPLE2
          (Vpitypespec,
           TUPLE3
            (Ref_typespec, TLIST _,
             TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
         STRING _, TLIST _))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE4
        (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)))) -> ()
|   TUPLE4
    (Ref_obj, STRING _, TLIST _,
     TUPLE2
      (Vpiactual,
       TUPLE4
        (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)))) -> ()
|   TUPLE4 (Package, Builtin, STRING _, Builtin) -> ()
|   TUPLE4
    (Logic_var, Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
     TLIST _) -> ()
|   TUPLE4
    (Logic_typespec, LOC (_, _, _, _), Logic_net,
     TUPLE3
      (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _))) -> ()
|   TUPLE4
    (Logic_net,
     TUPLE2
      (Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
     STRING _, TLIST _) -> ()
|   TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet)) -> ()
|   TUPLE4
    (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways)) -> ()
|   TUPLE4 (Input.If_else, _, _, _) -> ()
|   TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope) -> ()
|   TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> ()
|   TUPLE4
    (Bit_select, STRING _, TLIST _,
     TUPLE2
      (Vpiindex,
       TUPLE7
        (Part_select, TUPLE2 (Vpiname, STRING _),
         TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
         TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
         TUPLE2 (Vpirightrange, _)))) -> ()
|   TUPLE4
    (Bit_select, STRING _, TLIST _,
     TUPLE2
      (Vpiindex,
       TUPLE5
        (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
         TUPLE2 (UINT, Int _), Vpiuintconst))) -> ()
|   TUPLE4
    (Bit_select, STRING _, TLIST _,
     TUPLE2
      (Vpiindex,
       TUPLE4
        (Ref_obj, STRING _, TLIST _,
         TUPLE2
          (Vpiactual,
           TUPLE5
            (Logic_net,
             TUPLE2
              (Vpitypespec,
               TUPLE3
                (Ref_typespec, TLIST _,
                 TUPLE2
                  (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
             STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg)))))) -> ()
|   TUPLE4
    (Bit_select, STRING _, TLIST _,
     TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _))) -> ()
|   TUPLE4 (Assignment, _, _, _) -> ()
|TUPLE3 (Vpisubop, _, _) -> ()
|   TUPLE3
    (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _)) -> ()
|   TUPLE3 (Vpiparameter, STRING _, TLIST _) -> ()
|   TUPLE3 (Vpineqop, _, _) -> ()
|TUPLE3 (Vpilshiftop, _, _) -> ()
|   TUPLE3 (Vpilogandop, _, _) -> ()
|TUPLE3 (Vpigeop, _, _) -> ()
|   TUPLE3 (Vpieventorop, _, _) -> ()
|TUPLE3 (Vpieqop, _, _) -> ()
|   TUPLE3 (Vpibitxorop, _, _) -> ()
|TUPLE3 (Vpibitorop, _, _) -> ()
|   TUPLE3 (Vpiaddop, _, _) -> ()
|   TUPLE3
    (Sys_func_call,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE4
          (Logic_net, STRING _, TLIST _,
           TUPLE2 (Vpinettype, Vpialways)))),
     STRING _) -> ()
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpisubop, _, _), STRING _) -> ()
|   TUPLE3 (Sys_func_call, TUPLE3 (Vpiaddop, _, _), STRING _) -> ()
|   TUPLE3 (Sys_func_call, TUPLE2 (Vpiconcatop, TLIST _), STRING _) -> ()
|   TUPLE3
    (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _))) -> ()
|   TUPLE3
    (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _))) -> ()
|   TUPLE3
    (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))) -> ()
|   TUPLE3
    (Ref_typespec, TLIST _,
     TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _)))) -> ()
|   TUPLE3 (Ref_obj, STRING _, TLIST _) -> ()
|   TUPLE3
    (Ref_module, TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)),
     TLIST _) -> ()
|   TUPLE3 (Port, STRING _, TUPLE2 (Vpihighconn, _)) -> ()
|   TUPLE3 (Named_begin, TLIST _, TLIST _) -> ()
|   TUPLE3 (Named_begin, STRING _, TLIST _) -> ()
|   TUPLE3 (Logic_typespec, LOC (_, _, _, _), Logic_net) -> ()
|   TUPLE3
    (Logic_typespec, LOC (_, _, _, _),
     TUPLE3
      (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _))) -> ()
|   TUPLE3
    (If_stmt, TUPLE2 (Vpicondition, _),
     TUPLE4 (Assignment, _, _, _)) -> ()
|   TUPLE3
    (If_stmt, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> ()
|   TUPLE3
    (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE4 (Input.If_else, _, _, _)) -> ()
|   TUPLE3
    (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Named_begin, TLIST _, TLIST _)) -> ()
|   TUPLE3
    (Event_control, TUPLE2 (Vpicondition, _),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> ()
|   TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, _), TUPLE2 (Vpilhs, _)) -> ()
|   TUPLE3 (Class_defn, Queue, Queue) -> ()
|TUPLE3 (Class_defn, Array, Array) -> ()
|   TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class) -> ()
|   TUPLE3 (Class_defn, STRING _, STRING _) -> ()
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst),
     TUPLE3 (Named_begin, STRING _, TLIST _)) -> ()
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE4 (Assignment, _, _, _)) -> ()
|   TUPLE3
    (Case_item,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
       Vpibinaryconst),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> ()
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE4 (Assignment, _, _, _)) -> ()
|   TUPLE3
    (Case_item,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
           TUPLE2 (Vpisize, Int _)))),
     TUPLE3 (Begin, TLIST _, TLIST _)) -> ()
|   TUPLE3 (Begin, _, TLIST _) -> ()
|   TUPLE3 (Begin, TLIST _, TLIST _) -> ()
|   TUPLE3 (STRING _, STRING _, LOC (_, _, _, _)) -> ()
|   TUPLE2 (Weaklyreferenced, TLIST lst) -> List.iter match_pat lst
|TUPLE2 (Vpivisibility, Int _) -> ()
|   TUPLE2
    (Vpivariables,
     TUPLE5
      (Logic_var, Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       STRING _, TLIST _)) -> ()
|   TUPLE2
    (Vpivariables,
     TUPLE5
      (Enum_var,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _, TUPLE2 (Vpivisibility, Int _))) -> ()
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)))) -> ()
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))) -> ()
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE2
    (Vpitypespec,
     TUPLE3
      (Ref_typespec, TLIST _,
       TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))))) -> ()
|   TUPLE2 (Vpitypedef, Enum_typespec) -> ()
|   TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)) -> ()
|   TUPLE2 (Vpitopmodule, Int _) -> ()
|TUPLE2 (Vpitop, Int _) -> ()
|   TUPLE2 (Vpisize, Int _) -> ()
|TUPLE2 (Vpisigned, Int _) -> ()
|   TUPLE2 (Vpirightrange, _) -> ()
|TUPLE2 (Vpirhs, _) -> ()
|   TUPLE2
    (Vpireg,
     TUPLE4
      (Logic_var, Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       TLIST _)) -> ()
|   TUPLE2
    (Vpiprocess,
     TUPLE2
      (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3
        (Event_control, TUPLE2 (Vpicondition, _),
         TUPLE4 (Input.If_else, _, _, _)))) -> ()
|   TUPLE2
    (Vpiprocess,
     TUPLE2
      (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3
        (Event_control, TUPLE2 (Vpicondition, _),
         TUPLE3 (Named_begin, TLIST _, TLIST _)))) -> ()
|   TUPLE2
    (Vpiprocess,
     TUPLE2
      (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
       TUPLE3
        (Event_control, TUPLE2 (Vpicondition, _),
         TUPLE3 (Begin, TLIST _, TLIST _)))) -> ()
|   TUPLE2 (Vpiposedgeop, _) -> ()
|TUPLE2 (Vpiport, Port) -> ()
|   TUPLE2
    (Vpiport,
     TUPLE6
      (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))))) -> ()
|   TUPLE2
    (Vpiport,
     TUPLE6
      (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
       TUPLE2 (Vpihighconn, _), TUPLE2 (Vpilowconn, _),
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))))) -> ()
|   TUPLE2
    (Vpiport,
     TUPLE5
      (Port, STRING _, TUPLE2 (Vpidirection, Vpioutput),
       TUPLE2 (Vpilowconn, _),
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))))) -> ()
|   TUPLE2
    (Vpiport,
     TUPLE5
      (Port, STRING _, TUPLE2 (Vpidirection, Vpiinput),
       TUPLE2 (Vpilowconn, _),
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))))) -> ()
|   TUPLE2 (Vpiport, TUPLE3 (Port, STRING _, TUPLE2 (Vpihighconn, _))) -> ()
|   TUPLE2 (Vpiport, TUPLE2 (Port, TUPLE2 (Vpihighconn, _))) -> ()
|   TUPLE2 (Vpiport, TUPLE2 (Port, STRING _)) -> ()
|   TUPLE2 (Vpiparamassign, TLIST _) -> ()
|TUPLE2 (Vpioverriden, Int _) -> ()
|   TUPLE2 (Vpinettype, Vpireg) -> ()
|TUPLE2 (Vpinettype, Vpinet) -> ()
|   TUPLE2 (Vpinettype, Vpialways) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> ()
|   TUPLE2
    (Vpinet,
     TUPLE4
      (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> ()
|   TUPLE2 (Vpiname, Semaphore) -> ()
|TUPLE2 (Vpiname, Process) -> ()
|   TUPLE2 (Vpiname, Mailbox) -> ()
|TUPLE2 (Vpiname, STRING _) -> ()
|   TUPLE2 (Vpimodule, TLIST _) -> ()
|TUPLE2 (Vpimethod, Task) -> ()
|   TUPLE2 (Vpimethod, Function) -> ()
|TUPLE2 (Vpilowconn, _) -> ()
|   TUPLE2 (Vpilhs, _) -> ()
|TUPLE2 (Vpileftrange, _) -> ()
|   TUPLE2 (Vpiinstance, TLIST _) -> ()
|   TUPLE2
    (Vpiindex,
     TUPLE7
      (Part_select, TUPLE2 (Vpiname, STRING _),
       TUPLE2 (Vpifullname, TLIST _), TUPLE2 (Vpidefname, STRING _),
       TUPLE2 (Vpiconstantselect, Int _), TUPLE2 (Vpileftrange, _),
       TUPLE2 (Vpirightrange, _))) -> ()
|   TUPLE2
    (Vpiindex,
     TUPLE5
      (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
       TUPLE2 (UINT, Int _), Vpiuintconst)) -> ()
|   TUPLE2
    (Vpiindex,
     TUPLE4
      (Ref_obj, STRING _, TLIST _,
       TUPLE2
        (Vpiactual,
         TUPLE5
          (Logic_net,
           TUPLE2
            (Vpitypespec,
             TUPLE3
              (Ref_typespec, TLIST _,
               TUPLE2
                (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
           STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))))) -> ()
|   TUPLE2 (Vpiindex, TUPLE3 (Ref_obj, STRING _, TLIST _)) -> ()
|   TUPLE2 (Vpihighconn, _) -> ()
|   TUPLE2 (Vpigenstmt, TUPLE2 (Gen_case, TLIST _)) -> ()
|   TUPLE2
    (Vpigenscopearray,
     TUPLE4 (Gen_scope_array, STRING _, TLIST _, Gen_scope)) -> ()
|   TUPLE2 (Vpifullname, TLIST _) -> ()
|   TUPLE2
    (Vpielemtypespec,
     TUPLE2
      (Ref_typespec,
       TUPLE2
        (Vpiactual,
         TUPLE3
          (Logic_typespec, LOC (_, _, _, _),
           TUPLE3
            (Vpirange, TUPLE2 (Vpileftrange, _),
             TUPLE2 (Vpirightrange, _)))))) -> ()
|   TUPLE2 (Vpielaborated, Int _) -> ()
|TUPLE2 (Vpidirection, Vpioutput) -> ()
|   TUPLE2 (Vpidirection, Vpiinput) -> ()
|TUPLE2 (Vpidefname, STRING _) -> ()
|   TUPLE2 (Vpideflineno, Int _) -> ()
|TUPLE2 (Vpiconstantselect, Int _) -> ()
|   TUPLE2 (Vpicondition, _) -> ()
|TUPLE2 (Vpiconcatop, TLIST _) -> ()
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)) -> ()
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)) -> ()
|   TUPLE2
    (Vpiclassdefn,
     TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)) -> ()
|   TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)) -> ()
|   TUPLE2 (Vpicasetype, Int _) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE3
      (Case_item,
       TUPLE5
        (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _),
         TUPLE2 (UINT, Int _), Vpiuintconst),
       TUPLE3 (Named_begin, STRING _, TLIST _))) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE3
      (Case_item,
       TUPLE5
        (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
         Vpibinaryconst),
       TUPLE4 (Assignment, _, _, _))) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE3
      (Case_item,
       TUPLE5
        (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int _), BIN _,
         Vpibinaryconst),
       TUPLE3 (Begin, TLIST _, TLIST _))) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE3
      (Case_item,
       TUPLE4
        (Ref_obj, STRING _, TLIST _,
         TUPLE2
          (Vpiactual,
           TUPLE5
            (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
             TUPLE2 (Vpisize, Int _)))),
       TUPLE4 (Assignment, _, _, _))) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE3
      (Case_item,
       TUPLE4
        (Ref_obj, STRING _, TLIST _,
         TUPLE2
          (Vpiactual,
           TUPLE5
            (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
             TUPLE2 (Vpisize, Int _)))),
       TUPLE3 (Begin, TLIST _, TLIST _))) -> ()
|   TUPLE2
    (Vpicaseitem, TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _))) -> ()
|   TUPLE2
    (Vpicaseitem, TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _))) -> ()
|   TUPLE2
    (Vpicaseitem,
     TUPLE2 (Case_item, TUPLE3 (Begin, TLIST _, TLIST _))) -> ()
|   TUPLE2 (Vpiblocking, Int _) -> ()
|TUPLE2 (Vpibitnegop, _) -> ()
|   TUPLE2 (Vpialwaystype, Vpialways) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_var, Vpitypespec,
       TUPLE3
        (Ref_typespec, TLIST _,
         TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _)))),
       STRING _, TLIST _)) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpireg))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))))),
       STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE5
      (Enum_const, STRING _, TUPLE2 (INT, Int _), Vpidecompile _,
       TUPLE2 (Vpisize, Int _))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net,
       TUPLE2
        (Vpitypespec,
         TUPLE3
          (Ref_typespec, TLIST _,
           TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)))),
       STRING _, TLIST _)) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE4 (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpinet))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE4
      (Logic_net, STRING _, TLIST _, TUPLE2 (Vpinettype, Vpialways))) -> ()
|   TUPLE2
    (Vpiactual,
     TUPLE3
      (Logic_typespec, LOC (_, _, _, _),
       TUPLE3
        (Vpirange, TUPLE2 (Vpileftrange, _), TUPLE2 (Vpirightrange, _)))) -> ()
|   TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, _)) -> ()
|   TUPLE2 (Vpiactual, TUPLE2 (Enum_typespec, _)) -> ()
|   TUPLE2 (Vpiactual, TLIST _) -> ()
|   TUPLE2 (Vpiactual, NOT_FOUND (Logic_typespec, LOC (_, _, _, _))) -> ()
|   TUPLE2 (Vpiactual, NOT_FOUND (Array_typespec, LOC (_, _, _, _))) -> ()
|   TUPLE2
    (Uhdmtoppackages,
     TUPLE10
      (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
       TUPLE2
        (Vpiclassdefn,
         TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
       TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)))) -> ()
|   TUPLE2 (Uhdmtopmodules, TLIST lst) -> List.iter match_pat lst
|   TUPLE2 (Uhdmallpackages, TUPLE4 (Package, Builtin, STRING _, Builtin)) -> ()
|   TUPLE2 (Uhdmallmodules, TLIST lst) -> List.iter match_pat lst
|   TUPLE2
    (Uhdmallclasses,
     TUPLE9
      (Class_defn, TUPLE2 (Vpiname, Process),
       TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST _)),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task))) -> ()
|   TUPLE2
    (Uhdmallclasses,
     TUPLE6
      (Class_defn, TUPLE2 (Vpiname, Semaphore), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Task), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> ()
|   TUPLE2
    (Uhdmallclasses,
     TUPLE10
      (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
       TUPLE2 (Vpimethod, Function))) -> ()
|   TUPLE2 (UINT, Int _) -> ()
|   TUPLE2
    (Ref_typespec,
     TUPLE2
      (Vpiactual,
       TUPLE3
        (Logic_typespec, LOC (_, _, _, _),
         TUPLE3
          (Vpirange, TUPLE2 (Vpileftrange, _),
           TUPLE2 (Vpirightrange, _))))) -> ()
|   TUPLE2 (Ref_obj, STRING _) -> ()
|TUPLE2 (Port, TUPLE2 (Vpihighconn, _)) -> ()
|   TUPLE2 (Port, STRING _) -> ()
|TUPLE2 (Int_typespec, _) -> ()
|   TUPLE2 (Int_typespec, TUPLE2 (Vpisigned, Int _)) -> ()
|TUPLE2 (INT, Int _) -> ()
|   TUPLE2 (Gen_case, TLIST _) -> ()
|TUPLE2 (Enum_typespec, _) -> ()
|   TUPLE2 (Enum_typespec, TLIST _) -> ()
|TUPLE2 (Case_stmt, TLIST _) -> ()
|   TUPLE2 (Case_item, TUPLE4 (Assignment, _, _, _)) -> ()
|   TUPLE2 (Case_item, TUPLE3 (Begin, _, TLIST _)) -> ()
|   TUPLE2 (Case_item, TUPLE3 (Begin, TLIST _, TLIST _)) -> ()
|   TUPLE2 (Array_typespec, TLIST _) -> ()
|   TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)) -> ()
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE4 (Input.If_else, _, _, _))) -> ()
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE3 (Named_begin, TLIST _, TLIST _))) -> ()
|   TUPLE2
    (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpialways)),
     TUPLE3
      (Event_control, TUPLE2 (Vpicondition, _),
       TUPLE3 (Begin, TLIST _, TLIST _))) -> ()
|   TUPLE10
    (Package, Builtin, STRING _, Builtin, TUPLE2 (Vpitop, Int _),
     TUPLE2
      (Vpiclassdefn,
       TUPLE3 (Class_defn, Any_sverilog_class, Any_sverilog_class)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Array, Array)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, Queue, Queue)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _)),
     TUPLE2 (Vpiclassdefn, TUPLE3 (Class_defn, STRING _, STRING _))) -> ()
|   TUPLE10
    (Class_defn, TUPLE2 (Vpiname, Mailbox), TUPLE2 (Vpimethod, Function),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function), TUPLE2 (Vpimethod, Task),
     TUPLE2 (Vpimethod, Function)) -> ()
|   TLIST lst -> List.iter match_pat lst
|   Cont_assign | Vpiparent -> ()
|   Work | Vpiname | STRING _ -> ()
|   Class_typespec | Int_typespec -> ()
|   NOT_FOUND (Logic_typespec, LOC _) -> ()
|   oth -> othpat := oth; failwith "match_pat"


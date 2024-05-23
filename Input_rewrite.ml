open Input_lex
open Input
open Vpi_types

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

let othrw = ref None

type pattr = {
  mutable nam: token;
  mutable dir: token;
}

let findport pattr = function
| Vpiparent -> ()
| TUPLE2 (Vpiinstance, STRING inst) -> ()
| STRING _ as port -> pattr.nam <- port
| TUPLE2 (Vpidirection, dir) -> pattr.dir <- dir
| TUPLE3 (Ref_typespec, STRING refport,
            TLIST
             [TUPLE2 (Vpiactual, Logic_typespec);
              TLIST pth; Vpiparent]) -> ()
| TUPLE2 ((Vpilowconn|Vpihighconn),
            TUPLE2 (Ref_obj,
              TLIST
               [TUPLE2 (Vpiactual, Logic_net);
               TLIST pth'; lowport; Vpiparent])) -> pattr.nam <- lowport
| TUPLE2 (Vpihighconn,
      TUPLE2 (Ref_obj,
        TLIST
        [TLIST pth; STRING _ as port; Vpiparent])) -> pattr.nam <- port
| TUPLE3
     (Ref_typespec, TLIST pth,
      TLIST
       [TUPLE2 (Vpiactual, Logic_typespec);
        TLIST pth'; Vpiparent]) -> ()	
| oth -> othrw := Some oth; failwith "findport"

let othlst = ref []

let rec rw = function
| (Vpideffile | Vpiparent | Vpiname | VpiNum _ | Always | Vpicontassign | Vpitaskfunc | Vpiparamassign | Vpiparameter | Vpirefmodule | STRING _ | TLIST _) as s -> s
| TUPLE2 ((Vpitopmodule|Vpitop|Vpiblocking|Vpicasetype as top), VpiNum "1") -> top
| TUPLE2 (Vpiactual, Logic_var) -> Logic_var
| TUPLE2 (Ref_var, TLIST [TLIST pth; r; Vpiparent]) -> rw r
| TUPLE2 (Ref_obj, TLIST [TUPLE2 (Vpiactual, (Logic_net|Part_select)); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 (Ref_obj, TLIST [STRING "$unsigned"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 ((Ref_module|Module_inst|Vpigenstmt|Begin|If_else|Assignment|Event_control|Always|Sys_func_call|If_stmt|Case_stmt|Initial|For_stmt as op), TLIST ilst) -> TUPLE2(op, TLIST ( List.map rw ilst ))
| TUPLE2 (Vpilhs, TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent])) -> rw s
| TUPLE2 (Vpioperand, op) -> rw op
| TUPLE2 (Vpiinstance, STRING _) as s -> s
| TUPLE2 (Vpideflineno, VpiNum _) -> Vpideflineno
| TUPLE2 ((Vpistmt|Vpicondition|Vpirhs|Vpilhs as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE2 (Operation, TLIST ilst) -> TLIST ( List.map rw ilst )			   
| TUPLE2 (Part_select, TLIST ilst) -> TLIST ( List.map rw ilst )
| TUPLE2 (Indexed_part_select, TLIST ilst) -> TLIST ( List.map rw ilst )
| TUPLE2 (Vpinet, TUPLE2 (Array_net, TLIST [Vpirange; TLIST pth; net; Vpisize; Vpiparent])) -> rw net
| TUPLE2 (Bit_select, TLIST ilst) -> TLIST ( List.map rw ilst )			   
| TUPLE2 (Vpiindex, op) -> TUPLE2(Vpiindex, rw op)
| TUPLE2 (Cont_assign, TLIST [TUPLE2(Vpistmt, _); lhs; rhs; Vpiparent]) -> TUPLE3 (Cont_assign, rw lhs, rw rhs)
| TUPLE2 ((Vpileftrange|Vpirightrange as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent]) -> rw s
| TUPLE2 (Constant, TLIST (Vpiconsttype :: _ :: TUPLE2 (Vpidecompile, n) :: _)) -> rw n
| TUPLE2 (Constant, TLIST (Vpiconsttype :: _ :: Vpisize :: TUPLE2 (Vpidecompile, n) :: _)) -> rw n
| TUPLE2 (Vpielsestmt, arg) -> rw arg
| TUPLE2 (Vpiprocess, Initial) -> Work
| TUPLE2 (Vpivariables, TUPLE2 (Int_var, TLIST
         [TUPLE2 (Vpivisibility, VpiNum "1"); TUPLE2 (Vpisigned, VpiNum "1");
          TLIST pth; nam; Vpitypespec; Vpiparent])) ->  TUPLE2 (Int_var, rw nam)
| TUPLE2 (Vpiprocess, arg) -> rw arg
| TUPLE2 (Vpigenscopearray, TUPLE2 (Gen_scope_array, TLIST
         [Gen_scope; TLIST pth; genblk; Vpiparent])) -> TUPLE2 (Gen_scope_array, genblk)
| TUPLE2 (Vpimodule, TUPLE2 (Module_inst, TLIST lst)) -> TUPLE2 (Module_inst, TLIST (List.map rw lst))
| TUPLE2 ((Vpiforinitstmt|Vpiforincstmt as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE3 (Always, TLIST lst, TUPLE2 (Vpialwaystype, VpiNum "1")) -> TUPLE2 (Always, TLIST (List.map rw lst))
| TUPLE2 (Vpiindex, TUPLE2 (Ref_obj, TLIST [TUPLE2 (Vpiactual, Logic_net); pth; select; Vpiparent])) -> TUPLE2(pth, select)
| TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, TLIST [STRING _ as s; Vpiparent])) -> s
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [Vpiparent])) -> Vpicaseitem
| TUPLE2 (Case_item, TLIST [Vpitask; STRING "empty_statement"; Vpiparent]) -> Vpitask
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [TUPLE2 (Vpiexpr, expr); Vpiparent])) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [TUPLE2 (Vpiexpr, expr); TUPLE2 (Vpiexpr, expr'); Vpiparent])) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Var_select, TLIST lst) -> (match List.map rw lst with
| [TUPLE2 (TLIST [STRING y'; STRING mem2d'''; STRING mem''], y);
   TUPLE2 (TLIST [STRING x'; STRING mem2d''; STRING mem'], x);
   Logic_var; TLIST [mem2d; STRING mem]; STRING mem2d';
   Vpiparent] -> TUPLE4(Var_select, mem2d, x, y)
| [TUPLE2 (TLIST [STRING y'; STRING mem2d'''; STRING mem''], y);
   TUPLE2 (TLIST [STRING x'; STRING mem2d''; STRING mem'], x);
   TLIST [mem2d; STRING mem]; STRING mem2d';
   Vpiparent] -> TUPLE4(Var_select, mem2d, x, y)
| oth -> othlst := oth; failwith "select")

| TUPLE2 (Vpistmt, TUPLE2 ((Begin as stmt), TLIST lst)) -> TUPLE2 (stmt, TLIST (List.map rw lst))
| TUPLE2 (Vpistmt, TUPLE2 (Event_control as stmt, TLIST (TUPLE2(Vpistmt,_) as s :: TUPLE2(Vpicondition, _) :: _))) ->
  TUPLE2 (stmt, rw s)
(* *)
 | TUPLE2 (Vpistmt, TUPLE2 (Assignment, TLIST [TUPLE2 (Vpilhs, lhs); TUPLE2 (Vpirhs, rhs); optyp; Vpiparent])) -> TUPLE2(rw lhs, rw rhs)
(* *)
| TUPLE2 (Vpioptype, s) -> s
| TUPLE2 (Vpivariables,
      TUPLE2 (Logic_var,
        TLIST attr)) -> Vpivariables
| TUPLE2 (Vpinet,
      TUPLE2 (Logic_net,
        TLIST attr)) -> Logic_net
| TUPLE2 (Vpiport,
      TUPLE2 (Port,
        TLIST plst)) ->
(*
          TUPLE2 (Vpidirection, dir); port;
Vpiparent])) ->
*)
let pattr = {nam=Work; dir=Work} in
List.iter (findport pattr) plst;
TUPLE2(pattr.dir, pattr.nam)
| oth -> othrw := Some oth; failwith "rw"

let parse arg =
  let ch = open_in arg in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  List.flatten (List.map (function
    | TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, VpiNum _), Vpiname) -> []
    | TUPLE2 ((Uhdmallpackages|Uhdmtoppackages|Weaklyreferenced), TLIST _) -> []
    | TUPLE2 (Uhdmallclasses, TUPLE2 (Class_defn, TLIST _)) -> []
    | TUPLE2((Uhdmtopmodules|Uhdmallmodules), TLIST itm) -> List.map rw itm
    | Work -> []
    | oth -> othrw := Some oth; failwith "map") p)

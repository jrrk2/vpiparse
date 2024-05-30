open Input_lex
open Input

let deflate token = 
  let q = Queue.create () in
  fun lexbuf -> 
    if not (Queue.is_empty q) then Queue.pop q else   
      match token lexbuf with 
        | [   ] -> EOF_TOKEN
        | [tok] -> tok
        | hd::t -> List.iter (fun tok -> Queue.add tok q) t ; hd 

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start (deflate token) lb
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
| TUPLE5 (Ref_typespec, TLIST pth, Vpiparent, TLIST pth', 
              TUPLE2 (Vpiactual, TUPLE2(Logic_typespec, Work))) -> ()
| TUPLE2 ((Vpilowconn|Vpihighconn),
            TUPLE4 (Ref_obj, lowport, TLIST pth, 
               TUPLE2 (Vpiactual, TUPLE2(Logic_net, TLIST pth')))) -> pattr.nam <- lowport
| TUPLE2 (Vpihighconn,
      TUPLE3 (Ref_obj, (STRING _ as port), TLIST pth)) -> pattr.nam <- port
| TUPLE3
     (Ref_typespec, TLIST pth,
      TLIST
       [TUPLE2 (Vpiactual, Logic_typespec);
        TLIST pth'; Vpiparent]) -> ()	
| oth -> othrw := Some oth; failwith "findport"

let rec rw = function
| TUPLE2 (Vpinet, arg) -> rw arg
| TUPLE3 (If_stmt, TUPLE2 (Vpicondition, expr), stmt) -> TUPLE3 (If_stmt, rw expr, rw stmt)
        
| (Constant | Initial | Vpideffile | Vpiparent | Vpiname | VpiNum _ | Always | Cont_assign | Vpitaskfunc | Vpiparamassign | Vpiparameter | Vpirefmodule | STRING _ | TLIST _) as s -> s
| TUPLE2 (Vpicontassign, Cont_assign) -> Work
| TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->  TUPLE3 (Cont_assign, rw lhs, rw rhs)
| TUPLE4 (Cont_assign, TUPLE2 (Vpinetdeclassign, VpiNum "1"), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->  TUPLE3 (Cont_assign, rw lhs, rw rhs)
| TUPLE5 (Assignment, Vpioptypeint 82, TUPLE2 (Vpiblocking, VpiNum "1"),
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE3(Assignment, rw lhs, rw rhs)
| TUPLE4 (Assignment, Vpioptypeint 82,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE3(Assignment, rw lhs, rw rhs)
| TUPLE3 (Assignment,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE3(Assignment, rw lhs, rw rhs)
| TUPLE2 ((Vpiposedgeop as op), op1) -> TUPLE2(op, rw op1)
| TUPLE2 ((Vpiconcatop as op), TLIST op1) -> TUPLE2(op, TLIST (List.map rw op1))
| TUPLE3 ((Vpiaddop|Vpieqop as op), op1, op2) -> TUPLE3(op, rw op1, rw op2)
| TUPLE4 (Vpiconditionop as op, op1, op2, op3) -> TUPLE4(op, rw op1, rw op2, rw op3)
| TUPLE2 ((Vpitopmodule|Vpitop|Vpiblocking|Vpicasetype as top), VpiNum "1") -> top
(*
| TUPLE2 (Sys_func_call,
      TUPLE4 (Ref_obj, obj1,
        TLIST pth,
        func)) -> TUPLE2(func, rw obj1)
	*)
| TUPLE2 (Sys_func_call, arg) -> TUPLE2 (Sys_func_call, rw arg)
| TUPLE2 (Vpiactual, Logic_var) -> Logic_var
| TUPLE4 (Ref_obj, nam,
      TLIST pth,
      func) ->  TUPLE2(func, rw nam)
| TUPLE4 (Ref_obj, nam,
      TLIST pth,
      TUPLE2 (Vpiactual,
        TUPLE2 (Logic_net,
          TLIST pth2))) -> TUPLE2(Logic_net, nam)
| TUPLE5 (Ref_obj, nam,
      TLIST pth,
      TUPLE2 (Vpiactual,
        TUPLE2 (Logic_net,
          TLIST pth2)), STRING ("$signed"|"$unsigned")) -> TUPLE2(Logic_net, nam)
| TUPLE3 (Ref_var, nam, TLIST pth) -> rw nam
| TUPLE3 (Ref_obj, nam, TLIST pth) -> TUPLE2(Logic_net, nam)
| TUPLE2 (Ref_obj, TLIST [TUPLE2 (Vpiactual, (Logic_net|Part_select)); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 (Ref_obj, TLIST [STRING "$unsigned"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 ((Ref_module|Module_inst|Vpigenstmt|Begin|Assignment|Event_control|Always as op), TLIST ilst) -> TUPLE2(op, TLIST ( List.map rw ilst ))
| TUPLE2 (Initial, arg) -> TUPLE2(Initial, rw arg)
| TUPLE4 (If_else, TUPLE2 (Vpicondition, cond), if_clause, else_clause) -> TUPLE4(If_else, rw cond, rw if_clause, rw else_clause)
| TUPLE3 (Begin, TLIST pth, TLIST lst) -> TUPLE2(TLIST pth, TLIST (List.map rw lst))
| TUPLE2 (Vpilhs, TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent])) -> rw s
| TUPLE2 (Vpioperand, op) -> rw op
| TUPLE2 (Vpiinstance, STRING _) as s -> s
| TUPLE2 (Vpideflineno, VpiNum _) -> Vpideflineno
| TUPLE2 ((Vpicondition|Vpirhs|Vpilhs as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE7 (Part_select, TUPLE2(Vpiname, netnam), TUPLE2(Vpifullname, TLIST pth),
		       TUPLE2 (Vpidefname, STRING netnam'),
		       TUPLE2 (Vpiconstantselect, VpiNum "1"),
      TUPLE2 (Vpileftrange, lftrng),
      TUPLE2 (Vpirightrange, rghtrng)) -> TUPLE4(Part_select, rw netnam, rw lftrng, rw rghtrng)
| TUPLE8 (Indexed_part_select, nam, TLIST pth, nam', _, _, TUPLE2 (Vpileftrange, lftexp), TUPLE2 (Vpirightrange, rghtexp)) ->
  TUPLE4 (Indexed_part_select, rw nam, rw lftexp, rw rghtexp)
| TUPLE2 (Vpiport, TUPLE5 (Port, port, TUPLE2 (Vpidirection, dir), loconn, rts)) -> TUPLE3(Port, port, dir)
| TUPLE2 (Vpiport, TUPLE6 (Port, port, TUPLE2 (Vpidirection, dir), TUPLE2(Vpihighconn, hiconn), TUPLE2(Vpilowconn, loconn), rts)) -> TUPLE3(Port, port, dir)
| TUPLE2 (Logic_net as typ, TLIST pth) -> TUPLE2(typ, List.hd (List.rev pth))
| (TUPLE4 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent,
            TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)))),
        netnam, TLIST pth3)) -> TUPLE2(net, rw netnam)
| TUPLE4 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1,
          Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Integer_typespec, Work)))),
      netnam, TLIST pth3) -> TUPLE2(net, rw netnam)
| (TUPLE5 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent,
            TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)))),
        netnam, TLIST pth3,
        TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, rw netnam)
| (TUPLE3 (Logic_net, TUPLE2 (Vpitypespec, TUPLE6
           (Ref_typespec,
            TLIST pth,
            Vpiparent,
            TLIST pth2,
            TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)),
            TLIST pth3)),
        TUPLE2 (Vpinettype, STRING "vpi_port_net_48"))) -> Vpinet
| (TUPLE3 (Logic_net, net, TLIST pth)) -> rw net
| (TUPLE4 (Logic_net as net, netnam, TLIST pth, TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, rw netnam)
| (TUPLE4 (Logic_net as net, Vpitypespec, netnam, TLIST pth)) -> TUPLE2(net, rw netnam)
| (TUPLE5 (Logic_net as net, Vpitypespec, netnam, TLIST pth, TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, rw netnam)
| (TUPLE5 (Array_net, TUPLE2 (Vpisize, siz), nam, TLIST pth, Vpirange)) -> TUPLE3(Array_net, siz, nam)
| (TUPLE2 (Logic_net, TLIST [TUPLE2 (Vpinettype, STRING ("vpiReg"|"vpiAlways")); Vpitypespec; Vpiparent])) -> Logic_net
| (TUPLE2 (Logic_net, TLIST [TUPLE2 (Vpinettype, STRING ("vpiReg"|"vpiAlways")); TLIST pth; net; Vpitypespec; Vpiparent])) -> rw net
| (TUPLE2 (Logic_net, TLIST [TUPLE2 (Vpinettype, STRING ("vpiReg"|"vpiAlways")); TLIST pth; net; Vpiparent])) -> rw net
| (TUPLE2 (Logic_net, TLIST [TLIST pth; net; Vpitypespec; Vpiparent])) -> rw net
| (TUPLE2 (Logic_net, TLIST [TLIST pth; net; Vpiparent])) -> rw net
| TUPLE4 (Bit_select, op1,
      TLIST pth,
      TUPLE2 (Vpiindex, arg1)) -> TUPLE2(rw op1, rw arg1)
| TUPLE2 (Vpiindex, op) -> TUPLE2(Vpiindex, rw op)
| TUPLE2 ((Vpileftrange|Vpirightrange as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent]) -> rw s
| TUPLE5 (Constant, TUPLE2 (Vpidecompile, n), _, _, _) -> rw n
| TUPLE4 (Constant, TUPLE2 (Vpidecompile, s), STRING_CONST c, Vpistringconst) -> rw s
| TUPLE6 (Constant, TUPLE2 (Vpidecompile, n),
      TUPLE2 (Vpisize, VpiNum "1"), TUPLE2 (UINT, VpiNum "0"),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, Work)))),
      Vpiuintconst) -> rw n
| TUPLE2 (Constant, TLIST (Vpiconsttype :: _ :: Vpisize :: TUPLE2 (Vpidecompile, n) :: _)) -> rw n
| TUPLE2 (Vpielsestmt, arg) -> rw arg
| TUPLE2 (Vpivariables,
      TUPLE6 (Int_var,
        TUPLE2 (Vpitypespec,
          TUPLE5 (Ref_typespec,
            TLIST pth,
            Vpiparent,
            TLIST pth2,
            TUPLE2 (Vpiactual, TUPLE2 (Integer_typespec, Work)))),
        nam,
        TLIST pth3,
        TUPLE2 (Vpisigned, VpiNum "1"), TUPLE2 (Vpivisibility, VpiNum "1"))) -> TUPLE2(Int_var, nam)
| TUPLE2 (Vpivariables, TUPLE2 (Int_var, TLIST
         [TUPLE2 (Vpivisibility, VpiNum "1"); TUPLE2 (Vpisigned, VpiNum "1");
          TLIST pth; nam; Vpitypespec; Vpiparent])) ->  TUPLE2 (Int_var, rw nam)
| TUPLE2 (Vpiprocess, arg) -> rw arg
| TUPLE2 (Vpigenscopearray, TUPLE4 (Gen_scope_array, blk, TLIST pth, Gen_scope)) -> TUPLE2 (Gen_scope_array, blk)
| TUPLE2 (Vpimodule, TUPLE2 (Module_inst, TLIST lst)) -> TUPLE2 (Module_inst, TLIST (List.map rw lst))
| TUPLE2 ((Vpiforinitstmt|Vpiforincstmt as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE3 (Always, TLIST lst, TUPLE2 (Vpialwaystype, Vpialways)) -> TUPLE2 (Always, TLIST (List.map rw lst))
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE3(Event_control, TUPLE2(Vpicondition, cond), stmt)) -> TUPLE3 (Always, rw cond, rw stmt)
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE2(Event_control, stmt)) -> TUPLE2 (Always, rw stmt)
| TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpiassignstmt)), asgn) -> TUPLE3(Always, Vpiassignstmt, rw asgn)
| TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, TLIST [STRING _ as s; Vpiparent])) -> s
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [Vpiparent])) -> Vpicaseitem
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [Vpiparent])) -> Vpicaseitem
| TUPLE3 (Case_item,  STRING "empty_statement", Vpitask) -> Vpitask
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [TUPLE2 (Vpiexpr, expr); Vpiparent])) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TUPLE2 (Vpiexpr, expr))) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Case_stmt, TLIST (Vpiparent :: lst)) -> TUPLE2(Case_stmt, TLIST (List.map (function
    | TUPLE2 (Vpicasetype, VpiNum "1") as t -> t
    | TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, a)) ->  rw a
    | TUPLE2 (Vpicondition, cond) -> TUPLE2(Vpicondition, rw cond)
    | TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TUPLE2 (Vpiexpr, expr))) -> TUPLE2(Case_item, rw expr)
    | TUPLE3 (Begin, TLIST pth, TLIST _) -> Begin
    | TUPLE2 (Vpicaseitem, Case_item) -> Case_item
    | TUPLE2 (Case_stmt, _) as c -> rw c
    | TUPLE4 (Assignment, _, _, _) as a -> rw a
    | TUPLE5 (Assignment, _, _, _, _) as a -> rw a
    | oth -> othrw := Some oth; failwith "case_stmt") lst) )
| TUPLE6 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt) ->
  TUPLE6 (For_stmt, TLIST [], rw init1, rw inc1, rw cond, rw stmt)
| TUPLE9 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt1, stmt2, stmt3, stmt4) ->
  TUPLE6 (For_stmt, TLIST [], rw init1, rw inc1, rw cond, TLIST [rw stmt1; rw stmt2; rw stmt3; stmt4])
| TUPLE2 (Vpigenstmt, arg) -> TUPLE2 (Vpigenstmt, rw arg)
| TUPLE2 (Vpiport,
      TUPLE2 (Port,
        TLIST plst)) ->
let pattr = {nam=Work; dir=Work} in
List.iter (findport pattr) plst;
TUPLE2(pattr.dir, pattr.nam)
| TUPLE2 (Vpitaskfunc, Task) -> Task
| oth -> othrw := Some oth; failwith "rw"

let p' = ref []

type refh = {
lft: token;
rght: token;
lfttyp: token;
rghttyp: token;
lftsiz: token;
rghtsiz: token;
}

let refh = Hashtbl.create 257

let hash_itm = function
| TUPLE3 (Logic_typespec,
         TUPLE2 (Logic_net, TLIST pth),
         TUPLE3 (Vpirange,
           TUPLE2 (Vpileftrange,
             TUPLE5 (Constant, TUPLE2 (Vpidecompile, lft), TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
           TUPLE2 (Vpirightrange,
		   TUPLE5 (Constant, TUPLE2 (Vpidecompile, rght), TUPLE2 (Vpisize, rghtsiz), _, rghttyp)))) ->
Hashtbl.add refh (List.hd (List.rev pth)) {lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz}
| _ -> ()

let parse arg =
  let ch = open_in arg in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  p' := p;
  List.flatten (List.map (function
    | TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, VpiNum _), Vpiname) -> []
    | TUPLE2 ((Uhdmallpackages|Uhdmtoppackages), _) -> []
    | TUPLE2 (Weaklyreferenced, TLIST lst) -> List.iter (hash_itm) lst; []
    | TUPLE2 ((Uhdmtopmodules|Uhdmallclasses), _) -> []
    | TUPLE2(Uhdmallmodules, TUPLE2(Module_inst, TLIST arg)) -> List.map rw arg
    | Work -> []
    | oth -> othrw := Some oth; failwith "map") p)

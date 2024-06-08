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
  topmod: string;
  mutable nam: token;
  mutable dir: token;
  mutable loc: token;
  mutable signed: bool;
}

type refh = {
line: int;
col: int;
endln: int;
endcol: int;
lft: string;
rght: string;
lfttyp: token;
rghttyp: token;
lftsiz: token;
rghtsiz: token;
signed: bool;
}

let (refh:refh array ref) = ref [||]

let pwid = let wcache = ref [] in fun nam -> fun signed' -> function LOC (line', col', endln', endcol') ->
    let found = ref (if List.mem_assoc nam !wcache then List.assoc nam !wcache else Width(0,0,false)) in
    Array.iteri (fun ix ->
		   fun ({lft; rght; signed; line; col; endln; endcol}) ->
		      let nxtln = if ix+1 >= Array.length !refh then 9999 else (!refh).(ix+1).line in
		      let nxtcol = if ix+1 >= Array.length !refh then 9999 else (!refh).(ix+1).col in
		      if false then print_endline (string_of_int ix^": "^string_of_int line^" "^string_of_int col^" "^string_of_int endln^" "^string_of_int endcol);

		      if (line' >= line || (line'=line && col' >= col)) && (endln' < nxtln || (endln'=nxtln && endcol' <= nxtcol)) then
		      begin
		      if false then print_endline (string_of_int ix);
		      let lft' = int_of_string lft in
		      let rght' = int_of_string rght in
	              found := Width (lft', rght', signed || signed');
		      wcache := (nam, !found) :: !wcache
		      end
		      ) !refh;
if false then print_endline ((match nam, !found with STRING s, Width(hi,lo,_) -> s^" "^string_of_int hi^":"^string_of_int lo | _ -> ""));
    !found
| _ -> failwith "pwid"

let rec event_collapse = function
| TUPLE3 (Vpieventorop, ev, ev') -> ev' :: event_collapse ev
| oth -> [oth]

let rec rw topmod = function
| TUPLE2 (Vpinet, arg) -> (rw topmod) arg
| TUPLE5 (Enum_const, (STRING _ as nam), TUPLE2 (INT, (Int _ as n)), Vpidecompile _, TUPLE2 (Vpisize, (Int _ as siz))) -> TUPLE3(nam, n, siz)
| TUPLE2 (Vpitypedef, TUPLE2 (Enum_typespec, TLIST (STRING nam_t :: TUPLE2 (Vpiinstance, TLIST pth) :: lst))) -> TUPLE2(Enum_typespec, TLIST (List.map (rw topmod) lst))
| TUPLE2 (Vpitypedef, Enum_typespec) -> Enum_typespec
| TUPLE2 (Vpitypespec, spec) -> TUPLE2 (Vpitypespec, (rw topmod) spec)
| TUPLE2 (Ref_typespec, TLIST (pth :: Vpiparent :: TLIST pth' :: actual :: [])) -> TUPLE3(Ref_typespec, pth, (rw topmod) actual)
| TUPLE3 (If_stmt, TUPLE2 (Vpicondition, expr), stmt) -> TUPLE3 (If_stmt, (rw topmod) expr, (rw topmod) stmt)
        
| (Constant | Initial | Vpideffile | Vpiparent | Vpiname | Int _ | Always | Cont_assign | Vpitaskfunc | Vpiparamassign | Vpiparameter | Vpirefmodule | STRING _ | TLIST _) as s -> s
| TUPLE2 (Vpicontassign, Cont_assign) -> Work
| TUPLE2 (Cont_assign, LOC _) as c -> c
| TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->
let rwlhs = (rw topmod) lhs in let dest = match rwlhs with
       | TUPLE4 (Ref_obj, STRING nam, _, _) -> print_endline ("Cont_assign_rw1: "^topmod^" "^nam); nam
       | TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, Width _))), STRING nam) -> print_endline ("Cont_assign_rw2: "^topmod^" "^nam); nam
       | TUPLE5 (Part_select, STRING nam, TUPLE3 (Vpiuintconst, Int n, Int w), TUPLE3 (Vpiuintconst, Int n', Int w'), Width _) -> print_endline ("Cont_assign_rw3: "^topmod^" "^nam); nam
       | TUPLE3 (Bit_select, STRING nam, TUPLE3 (Vpiuintconst, Int n, Int w)) -> print_endline ("Cont_assign_rw4: "^topmod^" "^nam); nam
       | oth -> othrw := Some oth; failwith "Cont_assign_rw"^topmod in
  TUPLE5 (Cont_assign, rwlhs, (rw topmod) rhs, STRING topmod, STRING dest)
| TUPLE4 (Cont_assign, TUPLE2 (Vpinetdeclassign, Int 1), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->  TUPLE3 (Cont_assign, (rw topmod) lhs, (rw topmod) rhs)
| TUPLE5 (Assignment, optype, TUPLE2 (Vpiblocking, Int 1),
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Vpiblocking, optype, (rw topmod) lhs, (rw topmod) rhs)
| TUPLE4 (Assignment, optype,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Assignment, optype, (rw topmod) lhs, (rw topmod) rhs)
| TUPLE3 (Assignment,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Assignment, Vpirhs, (rw topmod) lhs, (rw topmod) rhs)
| TUPLE4 (Assignment, optype, TUPLE2 (Vpiblocking, Int 1),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE3(Vpiblocking, optype, (rw topmod) lhs)
| TUPLE6 (Array_var, TUPLE2 (Vpitypespec, spec), mem, pth, Vpiarraytype, TUPLE2 (Vpireg, TUPLE4 (Logic_var, Vpitypespec, spec', pth'))) ->
  TUPLE6 (Array_var, (rw topmod) spec, mem, pth, (rw topmod) spec', pth')
| TUPLE2 ((Vpiposedgeop|Vpinegedgeop|Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), op1) -> TUPLE2(op, (rw topmod) op1)
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop as op), TLIST op1) -> TUPLE2(op, TLIST (List.map (rw topmod) op1))
| TUPLE3 (Vpieventorop, TUPLE3 (Vpieventorop, _, _), _) as ev -> TUPLE2 (Vpieventorop, TLIST (event_collapse ev))
| TUPLE3 ((Vpieventorop|Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), op1, op2) -> TUPLE3(op, (rw topmod) op1, (rw topmod) op2)
| TUPLE4 (Vpiconditionop as op, op1, op2, op3) -> TUPLE4(op, (rw topmod) op1, (rw topmod) op2, (rw topmod) op3)
| TUPLE2 ((Vpitopmodule|Vpitop|Vpiblocking|Vpicasetype as top), Int 1) -> top
| TUPLE2 (Sys_func_call, arg) -> TUPLE2 (Sys_func_call, (rw topmod) arg)
| TUPLE3 (Sys_func_call, arg, (STRING ("$unsigned"|"$signed") as op)) -> TUPLE3 (Sys_func_call, (rw topmod) arg, (rw topmod) op)
| TUPLE2 (Vpiactual, Logic_var) -> Logic_var
| TUPLE4 (Ref_obj, nam,
      TLIST pth,
      func) ->  TUPLE2((rw topmod) func, (rw topmod) nam)
| TUPLE2 (Vpiactual,
      TUPLE2 (Logic_typespec,
              loc)) -> TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, pwid (STRING "") false loc))
| TUPLE2 (Vpiactual,
      TUPLE2 (Logic_net,
              TUPLE2 (TLIST pth, loc))) -> TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, pwid (List.hd (List.rev pth)) false loc)))
| TUPLE5 (Ref_obj, nam,
      TLIST pth,
      TUPLE2 (Vpiactual,
        TUPLE2 (Logic_net,
          TLIST pth2)), STRING ("$signed"|"$unsigned")) -> TUPLE2(Logic_net, nam)
| TUPLE3 (Ref_var, nam, TLIST pth) -> (rw topmod) nam
| TUPLE3 (Ref_obj, nam, TLIST pth) -> TUPLE2(Logic_net, nam)
| TUPLE2 (Ref_obj, TLIST [TUPLE2 (Vpiactual, (Logic_net|Part_select)); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 (Ref_obj, TLIST [STRING "$unsigned"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; nam; Vpiparent]) -> TUPLE2(Logic_net, nam)
| TUPLE2 ((Ref_module|Module_inst|Vpigenstmt|Begin|Assignment|Event_control|Always as op), TLIST ilst) -> TUPLE2(op, TLIST ( List.map (rw topmod) ilst ))
| TUPLE2 (Initial, arg) -> TUPLE2(Initial, (rw topmod) arg)
| TUPLE4 (If_else, TUPLE2 (Vpicondition, cond), if_clause, else_clause) -> TUPLE4(If_else, (rw topmod) cond, (rw topmod) if_clause, (rw topmod) else_clause)
| TUPLE3 (Begin, TLIST pth, TLIST lst) -> TUPLE3(Begin, TLIST pth, TLIST (List.map (rw topmod) lst))
| TUPLE3 (Named_begin, pth, TLIST lst) -> TUPLE3(Named_begin, pth, TLIST (List.map (rw topmod) lst))
| TUPLE2 (Vpilhs, TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent])) -> (rw topmod) s
| TUPLE2 (Vpioperand, op) -> (rw topmod) op
| TUPLE2 (Vpiinstance, STRING _) as s -> s
| TUPLE2 (Vpiinstance, TLIST [STRING _ as s]) -> TUPLE2 (Vpiinstance, s)
| TUPLE2 (Vpideflineno, Int _) -> Vpideflineno
| TUPLE2 ((Vpicondition|Vpirhs|Vpilhs as op), arg) -> TUPLE2 (op, (rw topmod) arg)
| TUPLE2 (TUPLE7 (Part_select, TUPLE2(Vpiname, netnam), TUPLE2(Vpifullname, TLIST pth),
		       TUPLE2 (Vpidefname, STRING netnam'),
		       TUPLE2 (Vpiconstantselect, Int 1),
      TUPLE2 (Vpileftrange, lftrng),
      TUPLE2 (Vpirightrange, rghtrng)), loc) -> TUPLE5(Part_select, (rw topmod) netnam, (rw topmod) lftrng, (rw topmod) rghtrng, pwid netnam false loc)
| TUPLE8 (Indexed_part_select, nam, TLIST pth, nam', _, _, TUPLE2 (Vpileftrange, lftexp), TUPLE2 (Vpirightrange, rghtexp)) ->
  TUPLE4 (Indexed_part_select, (rw topmod) nam, (rw topmod) lftexp, (rw topmod) rghtexp)
| TUPLE2 (Vpiport, TUPLE5 (Port, port, TUPLE2 (Vpidirection, dir), loconn, rts)) -> TUPLE3(Port, port, dir)
| TUPLE2 (Vpiport, TUPLE6 (Port, port, TUPLE2 (Vpidirection, dir), TUPLE2(Vpihighconn, hiconn), TUPLE2(Vpilowconn, loconn), rts)) -> TUPLE3(Port, port, dir)
| TUPLE2 (Logic_net as typ, TLIST pth) -> TUPLE2(typ, List.hd (List.rev pth))
| TUPLE4 (Logic_net, spec, nam, TLIST pth) -> TUPLE4 (Logic_net, (rw topmod) spec, nam, TLIST pth)
| TUPLE5 (Logic_net, spec, nam, TLIST pth, TUPLE2 (Vpinettype, (Vpinet|Vpireg|Vpialways as typ))) -> TUPLE4 (typ, (rw topmod) spec, nam, TLIST pth)
    
| (TUPLE4 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent,
            TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)))),
        netnam, TLIST pth3)) -> TUPLE2(net, (rw topmod) netnam)
| TUPLE4 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1,
          Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Integer_typespec, Work)))),
      netnam, TLIST pth3) -> TUPLE2(net, (rw topmod) netnam)
| (TUPLE5 (Logic_net as net, TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent,
            TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)))),
        netnam, TLIST pth3,
        TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, (rw topmod) netnam)
| (TUPLE3 (Logic_net, TUPLE2 (Vpitypespec, TUPLE6
           (Ref_typespec,
            TLIST pth,
            Vpiparent,
            TLIST pth2,
            TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, Work)),
            TLIST pth3)),
        TUPLE2 (Vpinettype, STRING "vpi_port_net_48"))) -> Vpinet
| (TUPLE3 (Logic_net, net, TLIST pth)) -> (rw topmod) net
| (TUPLE4 (Logic_net as net, netnam, TLIST pth, TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, (rw topmod) netnam)
| (TUPLE4 (Logic_net as net, Vpitypespec, netnam, TLIST pth)) -> TUPLE2(net, (rw topmod) netnam)
| (TUPLE5 (Logic_net as net, Vpitypespec, netnam, TLIST pth, TUPLE2 (Vpinettype, typ))) -> TUPLE3(net, typ, (rw topmod) netnam)
| (TUPLE5 (Array_net, TUPLE2 (Vpisize, siz), nam, TLIST pth, Vpirange)) -> TUPLE3(Array_net, siz, nam)
| TUPLE4 (Bit_select, op1,
      TLIST pth,
      TUPLE2 (Vpiindex, arg1)) -> TUPLE3(Bit_select, (rw topmod) op1, (rw topmod) arg1)
| TUPLE2 (Vpiindex, op) -> TUPLE2(Vpiindex, (rw topmod) op)
| TUPLE2 ((Vpileftrange|Vpirightrange as op), arg) -> TUPLE2 (op, (rw topmod) arg)
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent]) -> (rw topmod) s
| TUPLE5 (Constant, Vpidecompile _,
      TUPLE2 (Vpisize, Int wid), TUPLE2 (UINT, Int uint), Vpiuintconst) -> TUPLE3 (Vpiuintconst, Int uint, Int wid)
| TUPLE5 (Constant, Vpidecompile _,
      TUPLE2 (Vpisize, Int wid), (BIN _|OCT _|DEC _|HEX _ as radix), (Vpibinaryconst|Vpioctconst|Vpidecconst|Vpihexconst as kind)) -> TUPLE3 (kind, radix, Int wid)
| TUPLE4 (Constant, Vpidecompile s, STRING_CONST c, Vpistringconst) -> TUPLE3 (Vpistringconst, STRING_CONST c, Int (8*String.length c))
| TUPLE5 (Constant, Vpidecompile s, TUPLE2 (Vpisize, Int wid), STRING_CONST c, Vpistringconst) -> TUPLE3 (Vpistringconst, STRING_CONST c, Int wid)
| TUPLE6 (Constant, Vpidecompile s,
      TUPLE2 (Vpisize, Int wid), (TUPLE2 (UINT, Int n)),
      TUPLE2 (Vpitypespec, spec),
      kind) -> TUPLE4 (kind, Int n, Int wid, (rw topmod) spec)
| TUPLE6 (Constant, Vpidecompile s,
      TUPLE2 (Vpisize, Int wid), (DEC n | HEX n | BIN n as base),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, (TLIST _|STRING _), Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 ((Int_typespec|Integer_typespec), Work)))),
      kind) -> TUPLE3 (kind, base, Int wid)
(*
| TUPLE5 (Constant, Vpidecompile, n, _, _, _) -> (rw topmod) n
| TUPLE6 (Constant, Vpidecompile n,
      TUPLE2 (Vpisize, Int 1), TUPLE2 (UINT, Int "0"),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, Work)))),
      Vpiuintconst) -> (rw topmod) n
| TUPLE2 (Constant, TLIST (Vpiconsttype :: _ :: Vpisize :: Vpidecompile n :: _)) -> (rw topmod) n
*)
| TUPLE2 (Vpielsestmt, arg) -> (rw topmod) arg
| TUPLE2 (Vpivariables, TUPLE5 (Logic_var, Vpitypespec, spec, nam, (TLIST _ as pth))) -> TUPLE4(Logic_var, (rw topmod) spec, (rw topmod) nam, pth)
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
        TUPLE2 (Vpisigned, Int 1), TUPLE2 (Vpivisibility, Int 1))) -> TUPLE2(Int_var, nam)
| TUPLE2 (Vpivariables,
      TUPLE5 (Enum_var,
        TUPLE2 (Vpitypespec,
          TUPLE2 (Ref_typespec, TLIST [
            TLIST pth;
            Vpiparent;
            TLIST pth2;
            TUPLE2 (Vpiactual, TUPLE2 ((Enum_typespec|Integer_typespec), Work))])),
        nam,
        TLIST pth3,
        TUPLE2 (Vpivisibility, Int 1))) -> TUPLE2(Enum_var, nam)
| TUPLE2 (Vpivariables, TUPLE2 (Int_var, TLIST
         [TUPLE2 (Vpivisibility, Int 1); TUPLE2 (Vpisigned, Int 1);
          TLIST pth; nam; Vpitypespec; Vpiparent])) ->  TUPLE2 (Int_var, (rw topmod) nam)
| TUPLE2 (Vpiprocess, arg) -> (rw topmod) arg
| TUPLE2 (Vpigenscopearray, TUPLE4 (Gen_scope_array, blk, TLIST pth, Gen_scope)) -> TUPLE2 (Gen_scope_array, blk)
| TUPLE2 (Vpimodule, TLIST ( Vpiparent::STRING inst::TLIST pth::lst)) ->
    print_endline ("Module instance: "^inst);
     TUPLE2 (Vpimodule, TLIST (List.map (rw topmod) lst))
| TUPLE3 (Ref_module, TUPLE3(nam1', nam2', loc), TLIST (Vpiparent::nam1::nam2::act::portlst)) -> TUPLE4 (Ref_module, nam1, nam2, TLIST (List.map (rw topmod) portlst))
| TUPLE2 ((Vpiforinitstmt|Vpiforincstmt as op), arg) -> TUPLE2 (op, (rw topmod) arg)
| TUPLE3 (Always, TLIST lst, TUPLE2 (Vpialwaystype, Vpialways)) -> TUPLE2 (Always, TLIST (List.map (rw topmod) lst))
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE3(Event_control, TUPLE2(Vpicondition, cond), stmt)) -> TUPLE3 (Always, (rw topmod) cond, (rw topmod) stmt)
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE2(Event_control, stmt)) -> TUPLE2 (Always, (rw topmod) stmt)
| TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpiassignstmt)), asgn) -> TUPLE3(Always, Vpiassignstmt, (rw topmod) asgn)
| TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, TLIST [STRING _ as s; Vpiparent])) -> s
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [Vpiparent])) -> Vpicaseitem
| TUPLE3 (Case_item,  STRING "empty_statement", Vpitask) -> Vpitask
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [TUPLE2 (Vpiexpr, expr); Vpiparent])) -> TUPLE2 (Vpicaseitem, (rw topmod) expr)
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, expr)) -> TUPLE2 (Vpicaseitem, (rw topmod) expr)
| TUPLE2 (Vpicaseitem, TUPLE3 (Case_item, exp, stmt)) -> TUPLE3(Case_item, (rw topmod) exp, (rw topmod) stmt)
| TUPLE2 (Case_stmt, TLIST (Vpiparent :: lst)) -> TUPLE2(Case_stmt, TLIST (List.map (function
    | TUPLE2 (Vpicasetype, Int 1) as t -> t
    | TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, a)) ->  (rw topmod) a
    | TUPLE2 (Vpicondition, cond) -> TUPLE2(Vpicondition, (rw topmod) cond)
    | TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, stmt)) -> TUPLE2(Case_item, (rw topmod) stmt)
    | TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, expr)) -> TUPLE2(Case_item, (rw topmod) expr)
    | TUPLE2 (Vpicaseitem, TUPLE3 (Case_item, expr, stmt)) -> TUPLE3(Case_item, (rw topmod) expr, (rw topmod) stmt)
    | TUPLE2 (Vpicaseitem, TUPLE4 (Case_item, expr, stmt, stmt')) -> TUPLE4(Case_item, (rw topmod) expr, (rw topmod) stmt, (rw topmod) stmt')
    | TUPLE3 (Begin, TLIST pth, TLIST lst) -> TUPLE3(Begin, TLIST pth, TLIST (List.map (rw topmod) lst))
    | TUPLE2 (Vpicaseitem, Case_item) -> Case_item
    | TUPLE2 (Case_stmt, _) as c -> (rw topmod) c
    | TUPLE4 (Assignment, _, _, _) as a -> (rw topmod) a
    | TUPLE5 (Assignment, _, _, _, _) as a -> (rw topmod) a
    | TUPLE2 (Vpiattribute, TUPLE2 (Attribute, (STRING ("full_case"|"parallel_case") as attr))) -> TUPLE2 (Attribute, attr)
    | oth -> othrw := Some oth; failwith "case_stmt") lst) )
| TUPLE6 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt) ->
  TUPLE6 (For_stmt, TLIST [], (rw topmod) init1, (rw topmod) inc1, (rw topmod) cond, (rw topmod) stmt)
| TUPLE9 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt1, stmt2, stmt3, stmt4) ->
  TUPLE6 (For_stmt, TLIST [], (rw topmod) init1, (rw topmod) inc1, (rw topmod) cond, TLIST [(rw topmod) stmt1; (rw topmod) stmt2; (rw topmod) stmt3; stmt4])
| TUPLE2 (Vpigenstmt, arg) -> TUPLE2 (Vpigenstmt, (rw topmod) arg)
| TUPLE2 (Vpiport, TUPLE3 (Port, loc, TLIST (Vpiparent :: []))) -> TUPLE2(Port, loc)
| TUPLE2 (Vpiport, TUPLE3 (Port, (LOC _ as loc), TLIST (Vpiparent :: TUPLE2(Vpihighconn, conn) :: []))) -> TUPLE3(Port, loc, (rw topmod) conn)
| TUPLE2 (Vpiport, TUPLE3 (Port, TUPLE2(nam, loc), TLIST (Vpiparent :: TUPLE2(Vpihighconn, conn) :: []))) -> TUPLE4(Port, nam, loc, (rw topmod) conn)
| TUPLE2 (Vpiport,
      TUPLE3 (Port, TUPLE2 (STRING nam, (LOC (line', col', endln', endcol') as loc)),
        TLIST plst)) ->
let pattr = {topmod; nam=Work; dir=Work; loc; signed=false} in
List.iter (findport pattr) plst;
let wid = pwid pattr.nam pattr.signed pattr.loc in
TUPLE3(pattr.dir, pattr.nam, wid)
| TUPLE2 (Vpitaskfunc, Task) -> Task
| TUPLE3 (Vpiparameter, STRING param, TLIST lst) -> let pattr = {topmod; nam=Work; dir=Work; loc=Work; signed=false} in
List.iter (findport pattr) lst; TUPLE2(Parameter, pattr.nam)
| TUPLE2 (Vpiparamassign, TLIST lst) -> let pattr = {topmod; nam=Work; dir=Work; loc=Work; signed=false} in
List.iter (findport pattr) lst; TUPLE2(Parameter, pattr.nam)
| TUPLE2 (Vpiactual, TUPLE2 ((Enum_const|Enum_typespec|Logic_var|Int_typespec as typ), Work)) -> TUPLE2(Vpiactual, typ)
| TUPLE2 (Gen_case, TLIST (TUPLE2(Vpicondition, cond) :: items)) -> TUPLE3(Gen_case, (rw topmod) cond, TLIST (List.map (rw topmod) items))
| TUPLE3 (Begin, Work, TLIST [Vpiparent]) -> Begin
| Vpinullop -> Vpinullop
| oth -> othrw := Some oth; failwith "rw"

and findport pattr = function
| TUPLE2 (UINT, Int n) -> ()
| Vpiparent -> ()
| Vpisigned -> pattr.signed <- true
| Vpitypespec -> ()
| Vpilocalparam -> ()
| TUPLE2 (Vpioverriden, Int 1) -> ()
| TUPLE2 (Vpilhs, Parameter) -> ()
| TUPLE2 (Vpiinstance, STRING inst) -> ()
| STRING _ as port -> pattr.nam <- port
| TUPLE2 (Vpidirection, dir) -> pattr.dir <- dir
| TUPLE2 (Ref_typespec, TLIST lst) -> List.iter (function
       | TLIST pth -> ()
       | Vpiparent -> ()
       | TUPLE2 (Vpiactual, TUPLE2 (Logic_typespec, loc)) -> pattr.loc <- loc
       | TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, Work)) -> ()
       | oth -> othrw := Some oth; failwith "findport318") lst
| TUPLE5 (Ref_typespec, (TLIST _|STRING _), Vpiparent, TLIST pth', 
              TUPLE2 (Vpiactual, TUPLE2((Logic_typespec|Int_typespec|Integer_typespec), (LOC _ as loc)))) -> pattr.loc <- loc
| TUPLE2 ((Vpilowconn|Vpihighconn),
            TUPLE4 (Ref_obj, lowport, TLIST pth, 
               TUPLE2 (Vpiactual, TUPLE2(Logic_net, TUPLE2(TLIST pth', (LOC _ as loc)))))) -> pattr.nam <- lowport; pattr.loc <- loc
| TUPLE2 (Vpihighconn, TUPLE5 (Constant, Vpidecompile _, TUPLE2 (Vpisize, Int w), BIN b, Vpibinaryconst)) -> ()
| TUPLE2 (Vpihighconn,
      TUPLE3 (Ref_obj, (STRING _ as port), TLIST pth)) -> pattr.nam <- port
| TUPLE2 (Vpihighconn, TUPLE2 (expr, loc)) -> pattr.loc <- loc; findport pattr expr
| TUPLE7 (Part_select, TUPLE2(Vpiname, netnam), TUPLE2(Vpifullname, TLIST pth),
		       TUPLE2 (Vpidefname, STRING netnam'),
		       TUPLE2 (Vpiconstantselect, Int 1),
      TUPLE2 (Vpileftrange, lftrng),
      TUPLE2 (Vpirightrange, rghtrng)) -> ()
| TUPLE2 (Vpihighconn, expr) -> (match (rw pattr.topmod) expr with
    | TUPLE3 (Vpieqop, TUPLE2 (Logic_net, net), exp') -> ()
    | Vpinullop -> ()
    |  oth -> othrw := Some oth; failwith "findport335")
| TUPLE3 (Ref_typespec, TLIST pth,
      TLIST
       [TUPLE2 (Vpiactual, Logic_typespec);
        TLIST pth'; Vpiparent]) -> ()
| TLIST pth -> pattr.nam <- List.hd (List.rev pth)
| HEX h -> ()
| BIN h -> ()
| TUPLE2 (Vpirhs, cnst) -> (match (rw pattr.topmod) cnst with
   | TUPLE3 ((Vpiuintconst|Vpihexconst|Vpibinaryconst), Int n, Int wid) -> ()
   | TUPLE4 ((Vpiuintconst|Vpihexconst|Vpibinaryconst), Int n, Int wid, TUPLE3 (Ref_typespec, (STRING _|TLIST _), TUPLE2 (Vpiactual, Int_typespec))) -> ()
   | TUPLE4 (Vpiconditionop, TUPLE2 (Logic_net, STRING cond),
      TUPLE3 (Vpiuintconst, Int then_, Int wid),
      TUPLE3 (Vpiuintconst, Int else_, Int wid')) -> ()
   | TUPLE3 ((Vpiaddop|Vpimultop|Vpilogorop), lhs, rhs) -> ()
   | TUPLE2 (Vpiconcatop, TLIST lst) -> ()
   | oth -> othrw := Some oth; failwith "Vpirhs")
| oth -> othrw := Some oth; failwith "findport"

let hash_itm = function
| TUPLE3 (Logic_typespec,
         LOC (line, col, endln, endcol),
         TUPLE2 (Logic_typespec,
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))))) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;signed=false;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, endln, endcol),
         TUPLE3 (Logic_typespec,
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))), Vpisigned)) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;signed=true;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, _, _),
         TUPLE3 (Logic_typespec,
	   TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC (_, _, endln, endcol))),
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))))) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;signed=false;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, _, _),
         TUPLE4 (Logic_typespec,
	   TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC (_, _, endln, endcol))),
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))), Vpisigned)) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;signed=true;line; col; endln; endcol})
| TUPLE3 (Logic_typespec, LOC (line, col, _, _),
      TUPLE2 (Logic_typespec,
        TUPLE2 (Logic_net,
          TUPLE2 (TLIST pth,
            LOC (_, _, endln, endcol))))) -> 
({lft="0";rght="0";lfttyp=Work;rghttyp=Work;lftsiz=Work;rghtsiz=Work;signed=false;line; col; endln; endcol})
| TUPLE3 (Logic_typespec, LOC (line, col, endln, endcol),
      TUPLE2 (Logic_typespec,
        TUPLE3 (Vpirange,
          TUPLE2 (Vpileftrange, lft),
          TUPLE2 (Vpirightrange, rght)))) ->
({lft="0";rght="0";lfttyp=lft;rghttyp=rght;lftsiz=Work;rghtsiz=Work;signed=false;line; col; endln; endcol})
| TUPLE2 (Logic_typespec, LOC (line, col, endln, endcol)) ->
({lft="0";rght="0";lfttyp=Work;rghttyp=Work;lftsiz=Work;rghtsiz=Work;signed=false;line; col; endln; endcol})
| Int_typespec ->
({lft="0";rght="0";lfttyp=Work;rghttyp=Work;lftsiz=Work;rghtsiz=Work;signed=false;line=9999; col=9999; endln=9999; endcol=9999})
| oth -> othrw := Some oth; failwith "hash_itm"

let compare_hash_itm {line; col; endln; endcol} {line=line'; col=col'; endln=endln'; endcol=endcol'} =
if line < line' then -1 else
if line > line' then 1 else
if col < col' then -1 else
if col > col' then 1 else
0

let othmap = ref ([],[])

let parse arg =
  let ch = open_in arg in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  let modlst = ref [] in
  let modlst' = ref [] in
  List.iter (function
    | TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> ()
    | TUPLE2 ((Uhdmallpackages|Uhdmtoppackages), _) -> ()
    | TUPLE2 (Weaklyreferenced, TLIST lst) ->
    let lst' = List.filter (function Class_typespec -> false | TUPLE2 (Int_typespec, _) -> false | TUPLE2 (Logic_typespec, LOC _) -> true | _ -> true) lst in
    refh := Array.of_list (List.sort_uniq compare_hash_itm (List.map hash_itm lst'))
    | TUPLE2 (Uhdmallclasses, _) -> ()
    | TUPLE2((Uhdmtopmodules|Uhdmallmodules), TLIST rawlst) ->
        (match List.partition (function TUPLE2 ((Vpitypedef|Vpiparamassign|Vpivariables), _) | TUPLE3 (Vpiparameter, _,_ ) -> true | _ -> false) rawlst with
          | types, Vpiparent :: TLIST [] :: (STRING topmod) :: body -> modlst := (topmod, List.map (rw topmod) types @ List.map (rw topmod) body) :: !modlst
          | types, Vpiname :: (STRING topmod) :: body -> modlst' := (topmod, List.map (rw topmod) types @ List.map (rw topmod) body) :: !modlst'
          | oth -> othmap := oth; failwith "map'")
    | Work -> ()
    | oth -> othrw := Some oth; failwith "map") p;
  p, !modlst, !modlst'

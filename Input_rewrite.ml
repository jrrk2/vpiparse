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
  mutable nam: token;
  mutable dir: token;
  mutable loc: token;
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
}

let (refh:refh array ref) = ref [||]

let pwid = let wcache = ref [] in fun nam -> function LOC (line', col', endln', endcol') ->
    let found = ref (if List.mem_assoc nam !wcache then List.assoc nam !wcache else Width(0,0)) in
    Array.iteri (fun ix ->
		   fun ({lft; rght; line; col; endln; endcol}) ->
		      let nxtln = if ix+1 >= Array.length !refh then 9999 else (!refh).(ix+1).line in
		      let nxtcol = if ix+1 >= Array.length !refh then 9999 else (!refh).(ix+1).col in
		      if false then print_endline (string_of_int ix^": "^string_of_int line^" "^string_of_int col^" "^string_of_int endln^" "^string_of_int endcol);

		      if (line' >= line || (line'=line && col' >= col)) && (endln' < nxtln || (endln'=nxtln && endcol' <= nxtcol)) then
		      begin
		      if false then print_endline (string_of_int ix);
		      let lft' = int_of_string lft in
		      let rght' = int_of_string rght in
	              found := Width (lft', rght');
		      wcache := (nam, !found) :: !wcache
		      end
		      ) !refh;
if false then print_endline ((match nam, !found with STRING s, Width(hi,lo) -> s^" "^string_of_int hi^":"^string_of_int lo | _ -> ""));
    !found
| _ -> failwith "pwid"

let rec rw = function
| TUPLE2 (Vpinet, arg) -> rw arg
| TUPLE3 (If_stmt, TUPLE2 (Vpicondition, expr), stmt) -> TUPLE3 (If_stmt, rw expr, rw stmt)
        
| (Constant | Initial | Vpideffile | Vpiparent | Vpiname | Int _ | Always | Cont_assign | Vpitaskfunc | Vpiparamassign | Vpiparameter | Vpirefmodule | STRING _ | TLIST _) as s -> s
| TUPLE2 (Vpicontassign, Cont_assign) -> Work
| TUPLE3 (Cont_assign, TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->  TUPLE3 (Cont_assign, rw lhs, rw rhs)
| TUPLE4 (Cont_assign, TUPLE2 (Vpinetdeclassign, Int 1), TUPLE2 (Vpirhs, rhs), TUPLE2 (Vpilhs, lhs)) ->  TUPLE3 (Cont_assign, rw lhs, rw rhs)
| TUPLE5 (Assignment, optype, TUPLE2 (Vpiblocking, Int 1),
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Vpiblocking, optype, rw lhs, rw rhs)
| TUPLE4 (Assignment, optype,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Assignment, optype, rw lhs, rw rhs)
| TUPLE3 (Assignment,
		  TUPLE2 (Vpirhs, rhs),
		  TUPLE2 (Vpilhs, lhs)) -> TUPLE4(Assignment, Vpirhs, rw lhs, rw rhs)
| TUPLE2 ((Vpiposedgeop|Vpinegedgeop|Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), op1) -> TUPLE2(op, rw op1)
| TUPLE2 ((Vpiconcatop|Vpimulticoncatop as op), TLIST op1) -> TUPLE2(op, TLIST (List.map rw op1))
| TUPLE3 ((Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op), op1, op2) -> TUPLE3(op, rw op1, rw op2)
| TUPLE4 (Vpiconditionop as op, op1, op2, op3) -> TUPLE4(op, rw op1, rw op2, rw op3)
| TUPLE2 ((Vpitopmodule|Vpitop|Vpiblocking|Vpicasetype as top), Int 1) -> top
| TUPLE2 (Sys_func_call, arg) -> TUPLE2 (Sys_func_call, rw arg)
| TUPLE3 (Sys_func_call, arg, (STRING ("$unsigned"|"$signed") as op)) -> TUPLE3 (Sys_func_call, rw arg, rw op)
| TUPLE2 (Vpiactual, Logic_var) -> Logic_var
| TUPLE4 (Ref_obj, nam,
      TLIST pth,
      func) ->  TUPLE2(rw func, rw nam)
| TUPLE2 (Vpiactual,
      TUPLE2 (Logic_net,
              TUPLE2 (TLIST pth, loc))) -> TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TUPLE2 (TLIST pth, pwid (List.hd (List.rev pth)) loc)))
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
| TUPLE3 (Begin, TLIST pth, TLIST lst) -> TUPLE3(Begin, TLIST pth, TLIST (List.map rw lst))
| TUPLE2 (Vpilhs, TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent])) -> rw s
| TUPLE2 (Vpioperand, op) -> rw op
| TUPLE2 (Vpiinstance, STRING _) as s -> s
| TUPLE2 (Vpideflineno, Int _) -> Vpideflineno
| TUPLE2 ((Vpicondition|Vpirhs|Vpilhs as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE2 (TUPLE7 (Part_select, TUPLE2(Vpiname, netnam), TUPLE2(Vpifullname, TLIST pth),
		       TUPLE2 (Vpidefname, STRING netnam'),
		       TUPLE2 (Vpiconstantselect, Int 1),
      TUPLE2 (Vpileftrange, lftrng),
      TUPLE2 (Vpirightrange, rghtrng)), loc) -> TUPLE5(Part_select, rw netnam, rw lftrng, rw rghtrng, pwid netnam loc)
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
| TUPLE4 (Bit_select, op1,
      TLIST pth,
      TUPLE2 (Vpiindex, arg1)) -> TUPLE3(Bit_select, rw op1, rw arg1)
| TUPLE2 (Vpiindex, op) -> TUPLE2(Vpiindex, rw op)
| TUPLE2 ((Vpileftrange|Vpirightrange as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [STRING "$signed"; TUPLE2 (Vpiactual, Logic_net); TLIST pth; op; Vpiparent]) -> op
| TUPLE2 (Ref_obj, TLIST [TLIST pth; s; Vpiparent]) -> rw s
| TUPLE5 (Constant, Vpidecompile _,
      TUPLE2 (Vpisize, Int wid), TUPLE2 (UINT, Int uint), Vpiuintconst) -> TUPLE3 (Vpiuintconst, Int uint, Int wid)
| TUPLE5 (Constant, Vpidecompile _,
      TUPLE2 (Vpisize, Int wid), (BIN _|OCT _|DEC _|HEX _ as radix), (Vpibinaryconst|Vpioctconst|Vpidecconst|Vpihexconst as kind)) -> TUPLE3 (kind, radix, Int wid)
| TUPLE4 (Constant, Vpidecompile s, STRING_CONST c, Vpistringconst) -> TUPLE3 (Vpistringconst, STRING_CONST c, Int (8*String.length c))
| TUPLE5 (Constant, Vpidecompile s, TUPLE2 (Vpisize, Int wid), STRING_CONST c, Vpistringconst) -> TUPLE3 (Vpistringconst, STRING_CONST c, Int wid)
| TUPLE6 (Constant, Vpidecompile s,
      TUPLE2 (Vpisize, Int wid), (TUPLE2 (UINT, Int n)),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, (TLIST _|STRING _), Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 ((Int_typespec|Integer_typespec), Work)))),
      kind) -> TUPLE3 (kind, Int n, Int wid)
| TUPLE6 (Constant, Vpidecompile s,
      TUPLE2 (Vpisize, Int wid), (DEC n | HEX n | BIN n as base),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, (TLIST _|STRING _), Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 ((Int_typespec|Integer_typespec), Work)))),
      kind) -> TUPLE3 (kind, base, Int wid)
(*
| TUPLE5 (Constant, Vpidecompile, n, _, _, _) -> rw n
| TUPLE6 (Constant, Vpidecompile n,
      TUPLE2 (Vpisize, Int 1), TUPLE2 (UINT, Int "0"),
      TUPLE2 (Vpitypespec, TUPLE5 (Ref_typespec, TLIST pth1, Vpiparent, TLIST pth2, TUPLE2 (Vpiactual, TUPLE2 (Int_typespec, Work)))),
      Vpiuintconst) -> rw n
| TUPLE2 (Constant, TLIST (Vpiconsttype :: _ :: Vpisize :: Vpidecompile n :: _)) -> rw n
*)
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
        TUPLE2 (Vpisigned, Int 1), TUPLE2 (Vpivisibility, Int 1))) -> TUPLE2(Int_var, nam)
| TUPLE2 (Vpivariables, TUPLE2 (Int_var, TLIST
         [TUPLE2 (Vpivisibility, Int 1); TUPLE2 (Vpisigned, Int 1);
          TLIST pth; nam; Vpitypespec; Vpiparent])) ->  TUPLE2 (Int_var, rw nam)
| TUPLE2 (Vpiprocess, arg) -> rw arg
| TUPLE2 (Vpigenscopearray, TUPLE4 (Gen_scope_array, blk, TLIST pth, Gen_scope)) -> TUPLE2 (Gen_scope_array, blk)
| TUPLE2 (Vpimodule, TUPLE2 (Module_inst, TLIST lst)) -> TUPLE2 (Module_inst, TLIST (List.map rw lst))
| TUPLE2 (Vpirefmodule, TUPLE2 (Ref_module, TLIST (Vpiparent::nam1::nam2::act::portlst))) -> TUPLE4 (Ref_module, nam1, nam2, TLIST (List.map rw portlst))
| TUPLE2 ((Vpiforinitstmt|Vpiforincstmt as op), arg) -> TUPLE2 (op, rw arg)
| TUPLE3 (Always, TLIST lst, TUPLE2 (Vpialwaystype, Vpialways)) -> TUPLE2 (Always, TLIST (List.map rw lst))
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE3(Event_control, TUPLE2(Vpicondition, cond), stmt)) -> TUPLE3 (Always, rw cond, rw stmt)
| TUPLE2 (TUPLE2(Always, TUPLE2 (Vpialwaystype, Vpialways)), TUPLE2(Event_control, stmt)) -> TUPLE2 (Always, rw stmt)
| TUPLE2 (TUPLE2 (Always, TUPLE2 (Vpialwaystype, Vpiassignstmt)), asgn) -> TUPLE3(Always, Vpiassignstmt, rw asgn)
| TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, TLIST [STRING _ as s; Vpiparent])) -> s
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [Vpiparent])) -> Vpicaseitem
| TUPLE3 (Case_item,  STRING "empty_statement", Vpitask) -> Vpitask
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, TLIST [TUPLE2 (Vpiexpr, expr); Vpiparent])) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, expr)) -> TUPLE2 (Vpicaseitem, rw expr)
| TUPLE2 (Case_stmt, TLIST (Vpiparent :: lst)) -> TUPLE2(Case_stmt, TLIST (List.map (function
    | TUPLE2 (Vpicasetype, Int 1) as t -> t
    | TUPLE2 (Vpicaseitem, TUPLE2 (Attribute, a)) ->  rw a
    | TUPLE2 (Vpicondition, cond) -> TUPLE2(Vpicondition, rw cond)
    | TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, stmt)) -> TUPLE2(Case_item, rw stmt)
    | TUPLE2 (Vpicaseitem, TUPLE2 (Case_item, expr)) -> TUPLE2(Case_item, rw expr)
    | TUPLE2 (Vpicaseitem, TUPLE3 (Case_item, expr, stmt)) -> TUPLE3(Case_item, rw expr, rw stmt)
    | TUPLE2 (Vpicaseitem, TUPLE4 (Case_item, expr, stmt, stmt')) -> TUPLE4(Case_item, rw expr, rw stmt, rw stmt')
    | TUPLE3 (Begin, TLIST pth, TLIST lst) -> TUPLE3(Begin, TLIST pth, TLIST (List.map rw lst))
    | TUPLE2 (Vpicaseitem, Case_item) -> Case_item
    | TUPLE2 (Case_stmt, _) as c -> rw c
    | TUPLE4 (Assignment, _, _, _) as a -> rw a
    | TUPLE5 (Assignment, _, _, _, _) as a -> rw a
    | TUPLE2 (Vpiattribute, TUPLE2 (Attribute, (STRING ("full_case"|"parallel_case") as attr))) -> TUPLE2 (Attribute, attr)
    | oth -> othrw := Some oth; failwith "case_stmt") lst) )
| TUPLE6 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt) ->
  TUPLE6 (For_stmt, TLIST [], rw init1, rw inc1, rw cond, rw stmt)
| TUPLE9 (For_stmt, TLIST [], TUPLE2 (Vpiforinitstmt, init1), TUPLE2 (Vpiforincstmt, inc1), TUPLE2 (Vpicondition, cond), stmt1, stmt2, stmt3, stmt4) ->
  TUPLE6 (For_stmt, TLIST [], rw init1, rw inc1, rw cond, TLIST [rw stmt1; rw stmt2; rw stmt3; stmt4])
| TUPLE2 (Vpigenstmt, arg) -> TUPLE2 (Vpigenstmt, rw arg)
| TUPLE2 (Vpiport,
      TUPLE3 (Port, (LOC (line', col', endln', endcol') as loc),
        TLIST plst)) ->
let pattr = {nam=Work; dir=Work; loc} in
List.iter (findport pattr) plst;
let wid = pwid pattr.nam pattr.loc in
TUPLE3(pattr.dir, pattr.nam, wid)
| TUPLE2 (Vpitaskfunc, Task) -> Task
| TUPLE3 (Vpiparameter, STRING param, TLIST lst) -> let pattr = {nam=Work; dir=Work; loc=Work} in
List.iter (findport pattr) lst; TUPLE2(Parameter, pattr.nam)
| TUPLE2 (Vpiparamassign, TLIST lst) -> let pattr = {nam=Work; dir=Work; loc=Work} in
List.iter (findport pattr) lst; TUPLE2(Parameter, pattr.nam)
| oth -> othrw := Some oth; failwith "rw"

and findport pattr = function
| TUPLE2 (UINT, Int n) -> ()
| Vpiparent -> ()
| Vpisigned -> ()
| Vpitypespec -> ()
| Vpilocalparam -> ()
| TUPLE2 (Vpilhs, Parameter) -> ()
| TUPLE2 (Vpiinstance, STRING inst) -> ()
| STRING _ as port -> pattr.nam <- port
| TUPLE2 (Vpidirection, dir) -> pattr.dir <- dir
| TUPLE5 (Ref_typespec, (TLIST _|STRING _), Vpiparent, TLIST pth', 
              TUPLE2 (Vpiactual, TUPLE2((Logic_typespec|Int_typespec|Integer_typespec), (LOC _ as loc)))) -> pattr.loc <- loc
| TUPLE2 ((Vpilowconn|Vpihighconn),
            TUPLE4 (Ref_obj, lowport, TLIST pth, 
               TUPLE2 (Vpiactual, TUPLE2(Logic_net, TUPLE2(TLIST pth', (LOC _ as loc)))))) -> pattr.nam <- lowport; pattr.loc <- loc
| TUPLE2 (Vpihighconn,
      TUPLE3 (Ref_obj, (STRING _ as port), TLIST pth)) -> pattr.nam <- port
| TUPLE3 (Ref_typespec, TLIST pth,
      TLIST
       [TUPLE2 (Vpiactual, Logic_typespec);
        TLIST pth'; Vpiparent]) -> ()
| TLIST pth -> pattr.nam <- List.hd (List.rev pth)
| HEX h -> ()
| BIN h -> ()
| TUPLE2 (Vpirhs, cnst) -> (match rw cnst with
   | TUPLE3 ((Vpiuintconst|Vpihexconst|Vpibinaryconst), Int n, Int wid) -> ()
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
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, endln, endcol),
         TUPLE3 (Logic_typespec,
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))), Vpisigned)) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, _, _),
         TUPLE3 (Logic_typespec,
	   TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC (_, _, endln, endcol))),
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))))) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;line; col; endln; endcol})
| TUPLE3 (Logic_typespec,
         LOC (line, col, _, _),
         TUPLE4 (Logic_typespec,
	   TUPLE2 (Logic_net, TUPLE2(TLIST pth, LOC (_, _, endln, endcol))),
	   TUPLE3 (Vpirange,
	     TUPLE2 (Vpileftrange,
	       TUPLE5 (Constant, Vpidecompile lft, TUPLE2 (Vpisize, lftsiz), _, lfttyp)),
	     TUPLE2 (Vpirightrange,
		     TUPLE5 (Constant, Vpidecompile rght, TUPLE2 (Vpisize, rghtsiz), _, rghttyp))), Vpisigned)) ->
({lft;rght;lfttyp;rghttyp;lftsiz;rghtsiz;line; col; endln; endcol})
| TUPLE3 (Logic_typespec, LOC (line, col, _, _),
      TUPLE2 (Logic_typespec,
        TUPLE2 (Logic_net,
          TUPLE2 (TLIST pth,
            LOC (_, _, endln, endcol))))) -> 
({lft="0";rght="0";lfttyp=Work;rghttyp=Work;lftsiz=Work;rghtsiz=Work;line; col; endln; endcol})
| oth -> othrw := Some oth; failwith "hash_itm"

let compare_hash_itm {line; col; endln; endcol} {line=line'; col=col'; endln=endln'; endcol=endcol'} =
if line < line' then -1 else
if line > line' then 1 else
if col < col' then -1 else
if col > col' then 1 else
0

let parse arg =
  let ch = open_in arg in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  let modlst = ref [] in
  List.iter (function
    | TUPLE4 (DESIGN, STRING _, TUPLE2 (Vpielaborated, Int _), Vpiname) -> ()
    | TUPLE2 ((Uhdmallpackages|Uhdmtoppackages), _) -> ()
    | TUPLE2 (Weaklyreferenced, TLIST lst) ->
    let lst' = List.filter (function Class_typespec -> false | TUPLE2 (Int_typespec, _) -> false | TUPLE2 (Logic_typespec, LOC _) -> false | _ -> true) lst in
    refh := Array.of_list (List.sort_uniq compare_hash_itm (List.map hash_itm lst'))
    | TUPLE2 ((Uhdmtopmodules|Uhdmallclasses), _) -> ()
    | TUPLE2(Uhdmallmodules, TUPLE2(Module_inst, TLIST (TLIST [] :: STRING nam :: lst))) -> modlst := (nam, List.map rw lst) :: !modlst
    | Work -> ()
    | oth -> othrw := Some oth; failwith "map") p;
  p, !modlst

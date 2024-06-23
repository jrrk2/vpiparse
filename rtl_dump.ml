(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Printf
open Rtl_parser
open Input_dump
open Input_pat4
open Dump_types

let othrtl = ref ENDOFFILE
let othfail = ref ENDOFFILE
let uitms = ref []

let dir = function
| INPUT -> Dinput
| OUTPUT -> Doutput
| oth -> othfail := oth; failwith "dir"

let arithop = function
| PLUS -> Aadd
| MINUS -> Asub
| TIMES -> Amul
| oth -> othfail := oth; failwith "arithop"

let logicop = function
| AND -> Land
| OR -> Lor
| XOR -> Lxor
| oth -> othfail := oth; failwith "logicop"

let cmpop = function
| P_EQUAL -> Ceq
| LESS -> Clt
| oth -> othfail := oth; failwith "cmpop"

let rec expr itms = function
| IDSTR id -> _Ident itms id
| BINNUM c -> CNST (cexp c)
| INT n -> CNST (32, HEX n)
| DOUBLE(NOT,arg) -> UNRY(Unot, expr itms arg :: [])
| TRIPLE((AND|OR|XOR as op), lft, rght) -> LOGIC(logicop op, [expr itms lft;expr itms rght])
| TRIPLE((PLUS|MINUS|TIMES as op), lft, rght) -> ARITH(arithop op, [expr itms lft;expr itms rght])
| TRIPLE((P_EQUAL|LESS as op), lft, rght) -> CMP(cmpop op, [expr itms lft;expr itms rght])
| DOUBLE(CONCAT, TLIST lst) -> concat (List.map (expr itms) lst)
| QUADRUPLE (PARTSEL, id, INT hi, INT lo) -> SEL ("", [expr itms id; CNST (32, HEX lo); CNST (32, HEX (hi-lo+1))])
(*
 | QUADRUPLE (PARTSEL, id, hi, lo) -> _Selection itms (expr itms id, expr itms hi, expr itms lo, 0, 0)
*)
| QUADRUPLE (QUERY, cond, lft, rght) -> _Ternary itms (expr itms cond, expr itms lft, expr itms rght) 
| oth -> othfail := oth; failwith "expr"

(* *)
let ports = List.iter (function IDSTR id -> print_endline id | _ -> failwith "ports")
(* *)

let rec body itms = function
| QUADRUPLE(DLYASSIGNMENT, src, EMPTY, dest) -> _Asgn itms (expr itms src, expr itms dest)
| oth -> othfail := oth; failwith "body"
					
let rec dump itms = function
| TLIST lst -> List.iter (dump itms) lst
| DOUBLE(POSEDGE,arg) as pat -> othfail := pat; failwith "DOUBLE(POSEDGE,arg)"
| DOUBLE(ALWAYS, TLIST [DOUBLE (DOUBLE (AT, TLIST [DOUBLE (POSEDGE, IDSTR clk)]), TLIST [TLIST lst])]) ->
  _Always itms (POSEDGE clk, List.map (body itms) lst)
| DOUBLE(tok,arg) as pat -> othfail := pat; failwith "DOUBLE(tok,arg)"
| TRIPLE(EQUALS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(EQUALS,"
| TRIPLE(IF, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(IF,"
| TRIPLE(PLUS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(PLUS,"
| TRIPLE(LESS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(LESS,"
| TRIPLE(ASSIGNMENT, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(ASSIGNMENT,"
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, rhs)]) -> _Cont_assign itms (expr itms rhs, expr itms lhs)
| TRIPLE(tok, arg1, arg2) as pat ->  othfail := pat; failwith "TRIPLE(tok,"
| QUADRUPLE(QUERY, arg1, arg2, arg3) as pat ->  othfail := pat; failwith "QUADRUPLE(QUERY,"
| QUADRUPLE(PARTSEL, IDSTR id, hi, lo) as pat -> othfail := pat; failwith "QUADRUPLE(PARTSEL,"
| QUADRUPLE(EQUALS, arg1, arg2, arg3) as pat -> othfail := pat; failwith "QUADRUPLE(EQUALS,"
| QUADRUPLE(IF, arg1, arg2, arg3) as pat -> othfail := pat; failwith "QUADRUPLE(IF,"
| QUADRUPLE(WIRE, EMPTY, TRIPLE (EMPTY, EMPTY, EMPTY),
    TLIST [DOUBLE (IDSTR nam, EMPTY)]) -> _Identyp itms nam Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, RANGE (INT hi, INT lo), EMPTY),
    TLIST [DOUBLE (IDSTR nam, EMPTY)]) -> _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, RANGE (INT hi, INT lo),
    TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, EMPTY, EMPTY),
	  TLIST [TRIPLE (IDSTR nam, EMPTY, init)]) ->
		  _Identyp itms nam Vpinet; _Cont_assign itms (expr itms init, _Ident itms nam)
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, RANGE (INT hi, INT lo), EMPTY),
	  TLIST [TRIPLE (IDSTR nam, EMPTY, init)]) ->
		  _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet; _Cont_assign itms (expr itms init, _Ident itms nam)
| QUADRUPLE(tok, arg1, arg2, arg3) as pat -> othfail := pat; failwith "QUADRUPLE(tok,"
| QUINTUPLE(MODULE, arg1, arg2, TLIST arg3, arg4) -> let u = empty_itms [] in uitms := u :: !uitms; dump u arg2; ports arg3; dump u arg4
| QUINTUPLE((INPUT|OUTPUT as dir'), EMPTY, EMPTY, EMPTY,
        TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Port itms (dir dir') nam
| QUINTUPLE((INPUT|OUTPUT as dir'), EMPTY, EMPTY, RANGE (INT hi, INT lo),
        TLIST [TRIPLE (IDSTR nam, EMPTY, EMPTY)]) -> _Portrng itms (dir dir') nam (TYPRNG(HEX hi, HEX lo))
| QUINTUPLE(tok, arg1, arg2, arg3, arg4) as pat -> othfail := pat; failwith "QUINTUPLE(tok,"
| SEXTUPLE(tok, arg1, arg2, arg3, arg4, arg5) as pat -> othfail := pat; failwith "SEXTUPLE(tok,"
| SEPTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6) as pat -> othfail := pat; failwith "SEPTUPLE(tok,"
| RANGE(arg1,arg2) as pat -> othfail := pat; failwith "RANGE(arg1,arg2)"
| ALWAYS as pat -> othfail := pat; failwith "ALWAYS"
| ASCNUM c as pat -> othfail := pat; failwith "ASCNUM"
| ASSIGN as pat -> othfail := pat; failwith "ASSIGN"
| AT as pat -> othfail := pat; failwith "AT"
| BINNUM c as pat -> othfail := pat; failwith "BINNUM"
| BITSEL as pat -> othfail := pat; failwith "BITSEL"
| BUFIF lev as pat -> othfail := pat; failwith "BUFIF"
| D_ATTRIBUTE as pat -> othfail := pat; failwith "D_ATTRIBUTE"
| DECNUM c as pat -> othfail := pat; failwith "DECNUM"
| DOT as pat -> othfail := pat; failwith "DOT"
| EMPTY as pat -> othfail := pat; failwith "EMPTY"
| FLOATNUM flt as pat -> othfail := pat; failwith "FLOATNUM"
| HASH as pat -> othfail := pat; failwith "HASH"
| HEXNUM c as pat -> othfail := pat; failwith "HEXNUM"
| IDSTR str as pat -> othfail := pat; failwith "IDSTR"
| ILLEGAL c as pat -> othfail := pat; failwith "ILLEGAL"
| INOUT as pat -> othfail := pat; failwith "INOUT"
| INPUT as pat -> othfail := pat; failwith "INPUT"
| INTNUM c as pat -> othfail := pat; failwith "INTNUM"
| NEGEDGE as pat -> othfail := pat; failwith "NEGEDGE"
| OUTPUT as pat -> othfail := pat; failwith "OUTPUT"
| PARTSEL as pat -> othfail := pat; failwith "PARTSEL"
| PREPROC str as pat -> othfail := pat; failwith "PREPROC"
| REG as pat -> othfail := pat; failwith "REG"
| WEAK strength as pat -> othfail := pat; failwith "WEAK"
| WIDTHNUM(radix,sz,num) as pat -> othfail := pat; failwith "WIDTHNUM(radix,sz,num)"
| INT n as pat -> othfail := pat; failwith "INT"
| oth -> othfail := oth; failwith "dump"

let dump modnam rtl =
let chk = modnam^"_hardcaml.v" in
print_endline chk;
let fd = open_out chk in
output_string fd (Buffer.contents rtl);
close_out fd;
let lexbuf = Lexing.from_string (Buffer.contents rtl) in
let rslt = Rtl_parser.start Rtl_lexer.token lexbuf in
othrtl := rslt;
let u = empty_itms [] in
uitms := u :: [];
let _ = dump u rslt in
dump' "_callback" (modnam, ((), (List.hd !uitms)));
()

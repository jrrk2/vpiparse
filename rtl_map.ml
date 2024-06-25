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
open Input
open Input_types
open Input_dump
open Input_pat4
open Dump_types
open Rtl_parser
open File
open Formula
open Formula_rewrite
open File_rewrite
open Rtl_parser

let form = "!(((C1 | C2) & A) & (B1 | B2))";;
let rslt = Formula_rewrite.rewrite form;;
let othf = ref ("", "");;
let othmap = ref None
let othuniq = ref None
let othport = ref None
let othfilt = ref ENDOFFILE
let othchk = ref (XML [], XML [])
let sigcnt = ref 9999

let stem = "liberty/NCTU_75T";;
(* https://raw.githubusercontent.com/ieee-ceda-datc/RDF-2019/master/techlibs/NCTUcell_lib/NCTU_75T.lib *)
let rw, cellhash = File_rewrite.rewrite (stem^".lib");;
let _ = print_endline (string_of_int (Hashtbl.length cellhash));;
let cells = ref [];;
let othfail = ref UNKNOWN;;
Hashtbl.iter (fun k (iolst, formlst) ->
cells := (k,(iolst,List.map (fun (y,form) -> othf := k, form; y, try Formula_rewrite.rewrite form with _ -> failwith form) formlst)) :: !cells) cellhash;;
let cells = List.sort compare !cells;;

let filt1 = List.sort compare (List.filter (function (_,
  ([("Y", "output"); ("A", "input")],
    [("Y", DOUBLE (NOT, arg))])) -> true | _ -> false) cells);;

let filt' prop = function (_,
  (_,
    [(_, TRIPLE (IDSTR a, p, IDSTR b))
    ])) -> p=prop | _ -> false

let filtbuf = function (_,
  ((_,"output")::(_,"input")::[],
    [(_, IDSTR a)
    ])) -> true | _ -> false

let filtnot = function (_,
  ((_,"output")::(_,"input")::[],
    [(_, DOUBLE (NOT, IDSTR a))
    ])) -> true | _ -> false

let filtxor = function (_,
  (_,
    [(_, TRIPLE (TRIPLE (IDSTR a, P_AMPERSAND, DOUBLE (NOT, IDSTR b)), P_VBAR,
	      TRIPLE (DOUBLE (NOT, IDSTR a'), P_AMPERSAND, IDSTR b')))
    ])) when a=a' && b=b' -> true | _ -> false

let filtmap = function
| AND -> filt' P_AMPERSAND
| OR -> filt' P_VBAR
| XOR -> filtxor
| NOT -> filtnot
| oth -> othfilt := oth; failwith "filtmap"
    
let filtcells = function
| TRIPLE((AND|OR|XOR as op), lft, rght) ->
  let nlst = ref [] in
  let uniq = List.sort compare !nlst, List.sort_uniq compare (List.map (fun (nam,p) -> nlst := nam :: !nlst; p) (List.filter (filtmap op) cells)) in
  othuniq := Some uniq;
  uniq
| DOUBLE((NOT as op), rght) ->
  let nlst = ref [] in
  let uniq = List.sort compare !nlst, List.sort_uniq compare (List.map (fun (nam,p) -> nlst := nam :: !nlst; p) (List.filter (filtmap op) cells)) in
  othuniq := Some uniq;
  uniq
| BUF -> 
  let nlst = ref [] in
  let uniq = List.sort compare !nlst, List.sort_uniq compare (List.map (fun (nam,p) -> nlst := nam :: !nlst; p) (List.filter (filtbuf) cells)) in
  othuniq := Some uniq;
  uniq
| oth -> othfilt := oth; failwith "filtcells"

let opmap = function
| P_AMPERSAND -> "&"
| P_VBAR -> "|"
| XOR -> "^"
| NOT -> "!"
| oth -> othfail := oth; failwith "opmap"

let rec expr = function
| IDSTR id -> id
| INT n -> string_of_int n
| DOUBLE((NOT as op),arg) -> "("^opmap op^expr arg^")"
| TRIPLE(lft, (P_AMPERSAND|P_VBAR|XOR as op), rght) -> "("^expr lft^opmap op^expr rght^")"
| DOUBLE(CONCAT, TLIST lst) -> String.concat ", " (List.map expr lst)
| QUADRUPLE (PARTSEL, id, INT hi, INT lo) -> expr id^"["^string_of_int hi^":"^string_of_int lo^"]"
| QUADRUPLE (QUERY, cond, lft, rght) -> "("^expr cond^"?"^expr  lft^":"^expr  rght^")"
| oth -> othfail := oth; failwith "expr"

let v () = let fd = open_out (stem^".v") in
List.iter (fun (nam, (portlst, funclst)) ->
output_string fd ("module "^nam^" ("^String.concat ", " (List.map (fun (port,dir) -> dir^" "^port) portlst)^");\n"^
String.concat "\n" (List.map (fun (lhs, exp') -> "assign "^lhs^" = "^expr exp'^";") funclst)^"\nendmodule\n\n")) cells;
close_out fd

let othrtl = ref ENDOFFILE
let othfail = ref ENDOFFILE
let uitms = ref []

let dir = function
| INPUT -> Dinput
| OUTPUT -> Doutput
| oth -> othfail := oth; failwith "dir"

let arithop = function
| TIMES -> Amul
| PLUS -> Aadd
| MINUS -> Asub
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

let chk_reg itms reg fn =
  let wid' _ = function
    | Width(hi,lo,signed) -> for i = hi downto lo do fn hi lo i; done
    | _ -> failwith "chk_reg" in
  tran_search itms wid' wid' reg

let maplib' itms lst = function
| (VRF (nam, _, _) as op), (cellnam::_,(portlst,(funy,eqn)::_)::_) ->
let inports = List.filter (function (nam, "input") -> true | _ -> false) portlst in
let pnam ix = fst (try List.nth inports ix with _ -> othport := Some (inports, ix); failwith "othport") in
chk_reg itms nam (fun hi lo i ->
print_endline (string_of_int i);
itms.inst :=
     (cellnam^nam,
      ("", cellnam, List.mapi (fun ix -> function
	  | VRF (nam, _, []) as p ->
         PORT ("", pnam ix, Dinput, SEL("", p :: CNST(32, HEX i) :: CNST (32, SHEX 1) :: []) :: [] )
          | CNST (wid, HEX n) -> 
         PORT ("", pnam ix, Dinput, CNST (1, HEX (if ((1 lsl i) land n) > 0 then 1 else 0)) :: [])
          | SEL ("", _) as p -> 
         PORT ("", pnam ix, Dinput, p :: [])
          | oth -> othmap := Some oth; failwith "maplib'") lst @
         PORT ("", funy, Doutput, op :: []) :: [])) :: !(itms.inst)
)

let maplib itms lst cell =
decr sigcnt;
let nam = "_"^string_of_int !sigcnt in
let outp = VRF (nam, (BASDTYP, "logic", TYPNONE, []), []) in
_Identyp itms nam Vpinet;
maplib' itms lst (outp,cell);
outp

let rec expr itms = function
| IDSTR id -> _Ident itms id
| BINNUM c -> CNST (cexp c)
| INT n -> CNST (32, HEX n)
| DOUBLE(NOT,arg) -> UNRY(Unot, expr itms arg :: [])
| TRIPLE((AND|OR|XOR), lft, rght) as func -> maplib itms ([expr itms lft;expr itms rght]) (filtcells func)
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

let _chk_assign itms = function
| CNST (wid, HEX _) as lhs, (VRF (_, (BASDTYP, "logic", TYPNONE, []), []) as rhs) -> maplib' itms ([lhs]) (rhs, filtcells BUF)
| (VRF (_, _, []) as lhs), (VRF (_, _, []) as rhs) -> maplib' itms ([lhs]) (rhs, filtcells BUF)
| (SEL ("", _) as lhs), (VRF (_, _, []) as rhs) -> maplib' itms ([lhs]) (rhs, filtcells BUF)
| CAT ("", (VRF (lhs, (BASDTYP, "logic", TYPNONE, []), []) :: tl)), rhs -> print_endline "cat"
| (ARITH (Asub, VRF (arg1, _, []) :: VRF (arg2, _, []) :: []) as lhs), (VRF (_, _, []) as rhs) -> maplib' itms ([lhs]) (rhs, filtcells BUF)
| (rhs, lhs) -> othchk := (rhs, lhs); failwith "othchk"

let rec map itms = function
| TLIST lst -> List.iter (map itms) lst
| DOUBLE(POSEDGE,arg) as pat -> othfail := pat; failwith "DOUBLE(POSEDGE,arg)"
| DOUBLE(ALWAYS, TLIST [DOUBLE (DOUBLE (AT, TLIST [DOUBLE (POSEDGE, IDSTR clk)]), TLIST [TLIST lst])]) ->
  _Always itms (POSEDGE clk, List.map (body itms) lst)
| DOUBLE(tok,arg) as pat -> othfail := pat; failwith "DOUBLE(tok,arg)"
| TRIPLE(EQUALS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(EQUALS,"
| TRIPLE(IF, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(IF,"
| TRIPLE(PLUS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(PLUS,"
| TRIPLE(LESS, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(LESS,"
| TRIPLE(ASSIGNMENT, arg1, arg2) as pat -> othfail := pat; failwith "TRIPLE(ASSIGNMENT,"
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, (TRIPLE((AND|OR|XOR), lft, rght) as func))]) ->
maplib' itms ([expr itms lft;expr itms rght]) (expr itms lhs, filtcells func)
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, (DOUBLE((NOT), rght) as func))]) ->
maplib' itms ([expr itms rght]) (expr itms lhs, filtcells func)
| TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, lhs, rhs)]) -> _chk_assign itms (expr itms rhs, expr itms lhs)
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
		  _Identyp itms nam Vpinet; _chk_assign itms (expr itms init, _Ident itms nam)
| QUADRUPLE((WIRE|REG), EMPTY, TRIPLE (EMPTY, RANGE (INT hi, INT lo), EMPTY),
	  TLIST [TRIPLE (IDSTR nam, EMPTY, init)]) ->
		  _Identyprng itms nam (TYPRNG(HEX hi, HEX lo)) Vpinet; _chk_assign itms (expr itms init, _Ident itms nam)
| QUADRUPLE(tok, arg1, arg2, arg3) as pat -> othfail := pat; failwith "QUADRUPLE(tok,"
| QUINTUPLE(MODULE, arg1, arg2, TLIST arg3, arg4) -> let u = empty_itms [] in uitms := u :: !uitms; map u arg2; ports arg3; map u arg4
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

let map modnam rtl =
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
let _ = map u rslt in
dump' "_map" (modnam, ((), (List.hd !uitms)));
()

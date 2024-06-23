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

open Input_types
open Input
open Dump_types

let files = Hashtbl.create 255
let functable = Hashtbl.create 255
let hierarchy = Hashtbl.create 255
let modtokens = Hashtbl.create 255
let tasktable = Hashtbl.create 255

let csothitm = ref UNKNOWN
let exprothlst = ref []
let stmtothlst = ref []
let portothlst = ref []
let iothlst = ref []
let cellothlst = ref []
let memothlst = ref []
let mapothlst = ref []
let smpothlst = ref []
let optothlst = ref []
let xrflst = ref []
let selopt = ref None
let optopt = ref None
let typopt = ref None
let decopt = ref None
let cellopt = ref None
let arropt = ref []
let asciiopt = ref None
let optitmopt = ref None
let forlst = ref []
let widthlst = ref []

let matchcnt = ref 0
let empty_attr () = {anchor="a1";names=ref [];intf=ref [];instances=ref [];typetable=[||];modulexml=ref [];tmpvar=ref [];tmpasgn=ref []}

let constnet = function
| "1'h1" -> "supply1"
| "1'h0" -> "supply0"
| oth -> "wire"

let dirop = function
| "output" -> Doutput
| "input" -> Dinput
| "inout" -> Dinout
| "out" -> Doutput
| "in" -> Dinput
| oth -> failwith oth

let dumps s = "\""^s^"\""

let dumptyp = function
| UNKDTYP -> "UNKDTYP"
| PACKADTYP -> "PACKADTYP"
| UNPACKADTYP -> "UNPACKADTYP"
| CNSTDTYP -> "CNSTDTYP"
| BASDTYP -> "BASDTYP"
| STRDTYP -> "STRDTYP"
| UNIDTYP -> "UNIDTYP"
| REFDTYP -> "REFDTYP"
| ENUMDTYP -> "ENUMDTYP"
| MEMBDTYP -> "MEMBDTYP"
| PARMTDTYP -> "PARMTDTYP"
| IFCRFDTYP str -> "IFCRFDTYP "^dumps str
| TYPDF str -> "TYPDF "^dumps str

let unaryop = function
|"not" -> Unot
|"negate" -> Unegate
|"lognot" -> Ulognot
| _ -> Unknown

let extendop anchor w wm = function
|"extend" -> Uextend(w,wm)
|"extends" -> Uextends(anchor,w,wm)
| _ -> Unknown

let cmpop = function
|"eq" -> Ceq
|"neq" -> Cneq
|"gt" -> Cgt
|"gts" -> Cgts
|"gte" -> Cgte
|"gtes" -> Cgtes
|"eqwild" -> Ceqwild
|"neqwild" -> Cneqwild
|"ltes" -> Cltes
|"lte" -> Clte
|"lt" -> Clt
|"lts" -> Clts
| _ -> Cunknown

let logop = function
|"and" -> Land
|"redand" -> Lredand
|"or" -> Lor
|"redor" -> Lredor
|"xor" -> Lxor
|"xnor" -> Lxnor
|"redxor" -> Lredxor
|"redxnor" -> Lredxnor
|"shiftl" -> Lshiftl
|"shiftr" -> Lshiftr
|"shiftrs" -> Lshiftrs
| _ -> Lunknown

let arithop = function
|"add" -> Aadd
|"sub" -> Asub
|"mul" -> Amul
|"muls" -> Amuls
| _ -> Aunknown

let chkvif nam =
       let m = "__Viftop" in
       let lm = String.length m in
       let l = String.length nam in
       let vif = l > lm && String.sub nam (l-lm) lm = m in
       (vif, if vif then String.sub nam 0 (l-lm) else nam)

let expandbraket lo hi fn = Array.to_list (Array.init (hi-lo+1) (fun ix -> fn ("__BRA__"^string_of_int (ix+lo)^"__KET__")))
       
let dbg i ch h = if false then print_endline (string_of_int i^":"^String.make 1 ch^":"^string_of_int h)

let hex_to_bigint s = let rslt = ref 0L in String.iter (function
| '0'..'9' as ch -> rslt := Int64.add (Int64.of_int (int_of_char ch - int_of_char '0')) (Int64.mul 16L !rslt)
| 'a'..'f' as ch -> rslt := Int64.add (Int64.of_int (int_of_char ch - int_of_char 'a' + 10)) (Int64.mul 16L !rslt)
| ch -> failwith (String.make 1 ch)) s; !rslt

let bin_to_bigint s = let rslt = ref 0L in String.iter (function
| '0'..'1' as ch -> rslt := Int64.add (Int64.of_int (int_of_char ch - int_of_char '0')) (Int64.mul 2L !rslt)
| ch -> failwith (String.make 1 ch)) s; !rslt

let rec hex_of_bigint w n = Printf.sprintf "%Lx" n

let hex_to_ascii len str =
  let bytes = String.length str in
  if len mod 8 = 0 && len >= bytes*4 then
    begin
    let h = ref 0 and str' = Bytes.make (bytes/2) ' ' in
    let mychar_of_int x = if x >= 32 && x <= 127 && (len > 8 || x >= int_of_char 'a') then char_of_int x else failwith "ascii" in
    String.iteri (fun ix -> function
      | '0'..'9' as ch -> dbg ix ch !h;
          h := !h * 16 + (int_of_char ch - int_of_char '0');
          if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h;end
      | 'a'..'f' as ch -> dbg ix ch !h;
          h := !h * 16 + (int_of_char ch - int_of_char 'a' + 10);
          if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h; end
      | _ -> h := -1) str;
    str'
    end
  else invalid_arg str

let bin_to_ascii len str =
  let h = ref 0 in
  String.iteri (fun ix -> function
    | '0'..'1' as ch -> h := !h * 2 + (int_of_char ch - int_of_char '0');
    | _ -> h := -1) str;
  !h

let decode len str =
  decopt := Some (len,str);
  try STRING (Bytes.to_string (hex_to_ascii len str))
  with err -> let num = hex_to_bigint str in try HEX(Int64.to_int num) with err -> BIGINT num

let decodebin len str =
  decopt := Some (len,str);
  try HEX (bin_to_ascii len str)
  with err -> let num = bin_to_bigint str in try HEX(Int64.to_int num) with err -> BIGINT num

let cexp exp = match exp.[0] with
| '"' -> let n = String.length exp - 2 in let s = String.sub exp 1 n in (n, STRING s)
| _ ->
    try Scanf.sscanf exp "%d'h%x" (fun b n -> (b, HEX n)) with err ->
    try Scanf.sscanf exp "%d'h%s" (fun b s -> (b, decode b s)) with err ->
    try Scanf.sscanf exp "%d'b%s" (fun b s -> (b, decodebin b s)) with err ->
    try Scanf.sscanf exp "%d'sh%x" (fun b n -> (b, SHEX n)) with err ->
    try Scanf.sscanf exp "%d'bx" (fun b -> (b, BIN 'x')) with err ->
    try Scanf.sscanf exp "%d" (fun n -> (32, SHEX n)) with err ->
    try Scanf.sscanf exp "%f" (fun f -> (64, FLT f)) with err -> (-1,ERR exp)

let rec typmap = function
| [] -> TYPNONE
| [("signed", "true")] -> TYPNONE
| [("left", lft); ("right", rght)] -> TYPRNG(HEX(int_of_string lft), HEX(int_of_string rght))
| [("left", lft); ("right", rght); ("signed", "true")] -> TYPRNG(HEX(int_of_string lft), HEX(int_of_string rght))
| oth -> mapothlst := oth :: !mapothlst; failwith "mapothlst"

let fortailmatch ix' = function
| ASGN(dly, _, ARITH (Aadd, CNST inc :: (VRF _ as ix'') :: []) :: (VRF _ as ix''') :: []) :: tl -> (ix'=ix'') && (ix''=ix''')
| _ -> false

let forinc = function
| ASGN(dly,_, ARITH (Aadd, CNST inc :: (VRF _) :: []) :: (VRF _) :: []) :: tl -> (inc,List.rev tl)
| tl -> ((0,ERR ""),List.rev tl)

let fold1 fn = function
| [] -> failwith "should never occur, just to keep type system happy"
| (hd::tl) -> List.fold_left fn hd tl

let rec jump_opt origin = function
| JMPL (_, JMPL (orig, lst) :: []) :: [] -> jump_opt origin (JMPL (orig, lst) :: [])
| JMPL (orig, lst) :: [] -> JMPL (orig, lst)
| tl -> JMPL (origin, tl)

let xmlopt = ref None

let rec optitm' pass2 = function
| [] -> []
| XML _ as xml :: tl when pass2 -> xmlopt := Some xml; failwith "optitm'"
| XML xml :: tl -> optitm' pass2 (xml @ tl)
| IFC(o, nam', lst) :: tl -> IFC(o, nam', optitm' pass2 lst) :: optitm' pass2 tl
| PKG(o, nam, lst) :: tl -> PKG(o, nam, optitm' pass2 lst) :: optitm' pass2 tl
| MODUL(o, nam, lst, tmpvar) :: tl -> MODUL(o, nam, optitm' pass2 lst, tmpvar) :: optitm' pass2 tl
| INST _ as inst :: tl -> inst :: optitm' pass2 tl
| ALWYS(o, rw_lst) :: tl -> ALWYS(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| ASEL (exprlst) :: tl -> ASEL (optitm' pass2 exprlst) :: optitm' pass2 tl
| SNTRE lst :: tl -> SNTRE lst :: optitm' pass2 tl
| BGN(None, hd :: []) :: tl -> optitm' pass2 (hd :: tl)
| BGN(lbl, lst) :: tl -> BGN(lbl, optitm' pass2 (lst @ tl)) :: []
| CA(o,rw_lst) :: tl -> CA(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| CND (o, exprlst) :: tl -> CND (o, optitm' pass2 exprlst) :: optitm' pass2 tl
| CS(o,rw_lst) :: tl -> CS(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| CAT(o,rw_lst) :: tl -> CAT(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| CSITM(o,rw_lst) :: tl -> CSITM(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| IRNG (o, exprlst) :: tl -> IRNG (o, optitm' pass2 exprlst) :: optitm' pass2 tl
| JMPG(o,rw_lst) :: tl -> JMPG(o,optitm' pass2 rw_lst) :: optitm' pass2 tl
| WHL(rw_lst) :: tl -> WHL(optitm' pass2 rw_lst) :: optitm' pass2 tl
| FORSTMT(o,kind,cmpop,ix,strt,stop,inc,rw_lst) :: tl -> FORSTMT(o,kind,cmpop,ix,strt,stop,inc,optitm' pass2 rw_lst) :: optitm' pass2 tl
| TASKDEF(origin, nam, rw_lst) :: tl -> TASKDEF(origin, nam, optitm' pass2 rw_lst) :: optitm' pass2 tl
| TASKRF(origin, nam, rw_lst) :: tl -> TASKRF(origin, nam, optitm' pass2 rw_lst) :: optitm' pass2 tl
| ASGN _ as oth :: tl -> oth :: optitm' pass2 tl
| FNC (o, n, t, lst) :: tl -> FNC (o, n, t, optitm' pass2 lst) :: optitm' pass2 tl
| JMPL (o, lst) :: tl -> JMPL (o, optitm' pass2 lst) :: optitm' pass2 tl
| JMPBLK (o, lst) :: tl -> JMPBLK (o, optitm' pass2 lst) :: optitm' pass2 tl
| IF(origin, cnd :: then_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm' pass2 [then_stmt]) :: []) :: optitm' pass2 tl
| IF(origin, cnd :: then_stmt :: else_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm' pass2 [then_stmt]) :: BGN(None, optitm' pass2 [else_stmt]) :: []) :: optitm' pass2 tl
| (CNST _ | VAR _ | IVAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _ | UNRY _ | XRF _ | IO _ ) as oth :: tl -> oth :: optitm' pass2 tl
| (UNKNOWN|EITM (_, _, _, _, _)|TYP (_, _)|SFMT (_, _)|
TPLSRGS (_, _, _, _)|VPLSRGS (_, _, _)|PORT (_, _, _, _)|SNITM (_, _)|
ARITH (_, _)|FRF (_, _, _)|CPS (_, _)|REPL (_, _, _)|RNG _|INIT (_, _, _)|
IMP (_, _, _)|IMRF (_, _, _, _)|ARG _|FILS (_, _)|FIL (_, _)|
NTL _|CELLS (_, _)|CELL (_, _, _, _, _)|POSPOS (_, _)|POSNEG (_, _)|
NEGNEG (_, _)|POSEDGE _|NEGEDGE _|COMB|MODPORTFTR (_, _)|TYPETABLE _) as oth :: tl -> oth :: optitm' pass2 tl
| oth -> optitmopt := Some oth; failwith "optitmopt;;"

let while_opt origin lbl = function
| VAR (_, [ix''], _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly, _, CNST strt :: (VRF (ix''', _, []) as ix) :: []) ::
        JMPL (_, 
           (WHL
            (CMP (cmpop, CNST stop :: (VRF _ as ix') :: []) ::
             stmtlst)) :: []) :: [] when (ix=ix') && (ix''=ix''') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,kind,cmpop,ix,strt,stop,inc,stmts)
| VAR (_, [ix''], _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly,_, CNST strt :: (VRF (ix''', _, []) as ix) :: []) ::
           WHL
            (CMP (cmpop, CNST stop :: (VRF _ as ix') :: []) ::
             stmtlst) :: [] when (ix=ix') && (ix''=ix''') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,kind,cmpop,ix,strt,stop,inc,stmts)
| VAR _ :: ASGN(dly,_, a :: WHL (b :: stmtlst) :: []) :: [] as xlst' -> forlst := (a,b,stmtlst) :: !forlst; BGN (lbl, xlst')
| ASGN(dly,_, CNST strt :: (VRF _ as ix) :: []) ::
           WHL
            (CMP (cmpop, CNST stop :: (VRF _ as ix') :: []) ::
             stmtlst) :: [] when (ix=ix') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,"",cmpop,ix,strt,stop,inc,stmts)
| ASGN(dly,_, a :: WHL (b :: stmtlst) :: []) :: [] as xlst' -> forlst := (a,b,stmtlst) :: !forlst; BGN (lbl, xlst')
| oth -> BGN(lbl, oth)

let dlyenc = function
| "assign" -> false
| "assigndly" -> true
| _ -> failwith "dlyenc"

let typenc = function
| "packarraydtype" -> PACKADTYP
| "unpackarraydtype" -> UNPACKADTYP
| "constdtype" -> CNSTDTYP
| "basicdtype" -> BASDTYP
| "structdtype" -> STRDTYP
| "uniondtype" -> UNIDTYP
| "refdtype" -> REFDTYP
| "enumdtype" -> ENUMDTYP
| "memberdtype" -> MEMBDTYP
| "paramtypedtype" -> PARMTDTYP
| "voiddtype" -> UNKDTYP
| _ -> failwith "typenc"

let rec cell_hier = function
| CELL (_, nam, subnam, hier, rw_lst) ->
   let hier_lst = List.flatten (List.map cell_hier rw_lst) in
   if false then print_endline ("Cell: "^subnam);
   Hashtbl.replace hierarchy subnam hier_lst;
   (nam,subnam) :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

let dumpsized w = function
| BIN b -> string_of_int w^"'b"^String.make w b
| HEX n -> Printf.sprintf "%d'h%x" w n
| SHEX n -> Printf.sprintf "%d'sh%x" w n
| BIGINT n -> Printf.sprintf "%d'h%s" w (hex_of_bigint w n)
| STRING s -> "\""^String.escaped s^"\""
| FLT f -> string_of_float f
| ERR err -> ("NumberError:"^err)
| ENUMVAL (_, _) -> failwith "ENUMVAL"
| CNSTEXP (_, _) -> failwith "CSNTEXP"

let dumpu = function
| Unknown -> "Uunknown"
| Unot -> "Unot"
| Ulognot -> "Ulognot"
| Unegate -> "Unegate"
| Uunsigned -> "Uunsigned"
| Usigned -> "Usigned"
| Uextend(w,wm) -> "Uextend("^string_of_int w^", "^string_of_int wm^")"
| Uextends(anchor,w,wm) -> "Uextends("^anchor^", "^string_of_int w^", "^string_of_int wm^")"

let dumpcmp = function
| Cunknown -> "Cunknown"
| Ceq -> "Ceq"
| Cneq -> "Cneq"
| Cgt -> "Cgt"
| Cgts -> "Cgts"
| Cgte -> "Cgte"
| Cgtes -> "Cgtes"
| Ceqwild -> "Ceqwild"
| Cneqwild -> "Cneqwild"
| Cltes -> "Cltes"
| Clte -> "Clte"
| Clt -> "Clt"
| Clts -> "Clts"

let dumplog = function
| Lunknown -> "Lunknown"
| Land -> "Land"
| Lredand -> "Lredand"
| Lrednand -> "Lrednand"
| Lor -> "Lor"
| Lredor -> "Lredor"
| Lrednor -> "Lrednor"
| Lxor -> "Lxor"
| Lxnor -> "Lxnor"
| Lredxor -> "Lredxor"
| Lredxnor -> "Lredxnor"
| Lshiftl -> "Lshiftl"
| Lshiftr -> "Lshiftr"
| Lshiftrs -> "Lshiftrs"

let dumparith = function
| Aadd -> "Aadd"
| Asub -> "Asub"
| Amul -> "Amul"
| Amuls -> "Amuls"
| Adiv -> "Adiv"
| Adivs -> "Adivs"
| Amod -> "Amod"
| Amods -> "Amods"
| Apow -> "Apow"
| Apows -> "Apows"
| Aunknown -> "Aunknown"

let dumpstrlst lst = "["^String.concat ";\n\t" (List.map dumps lst)^"]"

let rec dumpdir = function
| Dinput -> "Dinput"
| Doutput -> "Doutput"
| Dinout -> "Dinout"
| Dvif s -> "Dvif "^dumps !s
| Dinam str -> "Dinam "^dumps str
| Dport(str1, int1, dirop, str2, str_lst) ->
     "Dport("^dumps str1 ^", "^ string_of_int int1 ^", "^ dumpdir dirop ^", "^ dumps str2 ^", "^ dumpstrlst str_lst^")"
| Dunknown -> "Dunknown"

let rec ascii_exp = function
| VRF (vrf, _, []) -> "_"^vrf^"_"
| CNST (_, BIN n) -> (String.make 1 n)
| CNST (_, HEX n) -> (string_of_int n)
| CNST (_, SHEX n) -> (string_of_int n)
| CNST (w, BIGINT n) -> hex_of_bigint w n
| CNST (_, STRING s) -> String.map (function '0'..'9' | 'a'..'f' | 'A'..'F' as ch -> ch | oth -> '_') s
| CNST (_, FLT f) -> (string_of_float f)
| CNST (_, ERR err) -> "ERR"
| UNRY ((Uextend _|Uextends _), exprlst) -> "Uext_"^ascii_lst exprlst
| UNRY (op, exprlst) -> "Unry_"^dumpu op^ascii_lst exprlst
| CMP (op, exprlst) -> dumpcmp op^ascii_lst exprlst
| LOGIC (op, exprlst) -> dumplog op^ascii_lst exprlst
| ARITH (op, exprlst) -> dumparith op^ascii_lst exprlst
| ASEL (exprlst) -> "Asel_"^ascii_lst exprlst
| CND (origin, exprlst) -> "Cnd_"^ascii_lst exprlst
| CPS (origin, exprlst) -> "Cps_"^ascii_lst exprlst
| CAT (origin, exprlst) -> "Cat_"^ascii_lst exprlst
| REPL (origin, tid, exprlst) -> "Repl_"^string_of_int tid^ascii_lst exprlst
| IRNG (origin, exprlst) -> "Irng_"^ascii_lst exprlst
| FRF (origin, fref, arglst) -> "Frf_"^fref^ascii_lst arglst
| SFMT (fmt, exprlst) -> "Sfmt_"^ascii_lst exprlst
| XRF(str1, str2, str3, str4, dirop) -> "Xrf_"^str1^"_"^str2^"_"^str3^"_"^str4^"_"^dumpdir dirop
| SEL (origin, exprlst) -> "Sel"^ascii_lst exprlst
| ARG exprlst -> "Arg_"^ascii_lst exprlst
| oth -> asciiopt := Some oth; failwith "asciiopt"
and ascii_lst exprlst = "_"^String.concat "_" (List.map ascii_exp exprlst)^"_"

let ascii_exp arg wid =
  let s = ascii_exp arg ^ string_of_int wid in
  let out = ref 2 and under = ref 0 in
  let s' = Bytes.make (String.length s + 2) '_' in
  String.iter (function '_' -> incr under | oth -> if !under > 0 then incr out; Bytes.set s' !out oth; incr out; under := 0) s;
  Bytes.sub s' 0 !out

let rec simplify_exp attr = function
| VRF (_, _, []) as vrf -> (vrf)
| CNST _ as cst -> (cst)
| UNRY (op, expr1 :: []) -> (UNRY (op, simplify_exp attr expr1 :: []))
| CMP (op, expr1 :: expr2 :: []) -> (CMP (op, simplify_exp attr expr1 :: simplify_exp attr expr2 :: []))
| LOGIC (op, exprlst) -> (LOGIC (op, List.map (simplify_exp attr) exprlst))
| ARITH (op, exprlst) -> (ARITH (op, List.map (simplify_exp attr) exprlst))
| ASEL (VRF _ as vrf :: expr1 :: []) -> (ASEL (vrf :: simplify_exp attr expr1 :: []))
| ASEL (ASEL _ as multi :: expr :: []) -> (ASEL (simplify_exp attr multi :: simplify_exp attr expr :: []))
| CND (origin, exprlst) -> (CND (origin, List.map (simplify_exp attr) exprlst))
| CPS (origin, exprlst) -> (CPS (origin, List.map (simplify_exp attr) exprlst))
| CAT (origin, exprlst) -> (CAT (origin, List.map (simplify_exp attr) exprlst) )
| REPL (origin, tid, arg :: (CNST _ as cst) :: []) -> (REPL (origin, tid, simplify_exp attr arg :: cst :: []))
| IRNG (origin, exprlst) -> (IRNG (origin, List.map (simplify_exp attr) exprlst))
| FRF (origin, fref, arglst) -> (FRF (origin, fref, List.map (simplify_exp attr) arglst))
| SFMT (fmt, exprlst) -> (SFMT (fmt, List.map (simplify_exp attr) exprlst))
| XRF _ as xrf -> (xrf)
| SEL (orig, expr1 :: expr2 :: expr3 :: []) ->
    SEL (orig, List.map (simplify_exp attr) (expr1 :: expr2 :: expr3 :: []))
| oth -> smpothlst := oth :: !smpothlst; oth

let simplify_exp' arg =
  let attr = empty_attr() in
  let rslt = simplify_exp attr arg in
  (!(attr.tmpvar),rslt)

let simplify_asgn dly' attr dst = function
| CND (origin, cnd :: lft :: rght :: []) -> 
            IF(origin, cnd :: ASGN(dly', origin, lft :: dst :: []) :: ASGN(dly', origin, rght :: dst :: []) :: []) :: []
| src ->
        attr.tmpasgn := [];
        let src' = simplify_exp attr src in
        let dst' = simplify_exp attr dst in
        let prep = List.map (fun (_,x) -> x) !(attr.tmpasgn) in
        let rslt = ASGN(dly', attr.anchor, src' :: dst' :: []) in
        prep@rslt :: []

let arithopv = function
| Aadd -> "+"
| Asub -> "-"
| Amul | Amuls -> "*"
| Adiv | Adivs -> "/"
| Amod | Amods -> "%"
| Apow | Apows -> "**"
| Aunknown -> "???"

let dumpi n = string_of_int n
let dumpcnst (w,n) = dumpsized w n
let rec dumpr = function
| HEX n -> string_of_int n
| SHEX n -> string_of_int n
| ERR _ -> "ERR"
| BIN b -> String.make 1 b
| STRING s -> s
| FLT f -> string_of_float f
| BIGINT i -> hex_of_bigint 64 i
| CNSTEXP (Aunknown, STRING fn::rght::[]) -> fn^" "^(dumpr rght)
| CNSTEXP (op, lft::rght::[]) -> (dumpr lft)^" "^arithopv op^" "^(dumpr rght)
| CNSTEXP _ -> failwith "CNSTEXP"
| ENUMVAL _ -> failwith "ENUMVAL"

let rec dumpmap = function
| TYPNONE -> "TYPNONE"
| SUBTYP int1 -> "SUBTYP "^string_of_int int1
| TYPRNG(int1,int2) -> "TYPRNG("^dumpr int1^", "^dumpr int2^")"
| TYPMEMBER (tab) -> "TYPMEMBER"^dumptab tab
| TYPENUM(str1, int1, (exp2,exp3)) -> "TYPENUM("^dumps str1^", "^string_of_int int1^", ("^string_of_int exp2^", "^dumpr exp3^"))"
| TYPDEF -> "TYPDEF"
| RECTYP tab -> "RECTYP"^dumptab tab

and dumptab (typenc, str1, map, typmaplst) = "("^dumptyp typenc^", "^dumps str1^", "^dumpmap map^", "^dumpmlst typmaplst^")"
and dumpmlst lst = "["^String.concat ";\n\t" (List.map dumpmap lst)^"]"
let dumptablst lst = "["^String.concat ";\n\t" (List.map dumptab lst)^"]"
let dumpb b = string_of_bool b

let oldsrc = ref ("",-1)

let unaryopv = function
| Unknown -> "???"
| Unot -> " ~ "
| Ulognot -> " ! "
| Unegate -> " - "
| Uunsigned -> "$unsigned"
| Usigned -> "$signed"
| Uextend(w,wm) -> "__extend_"^string_of_int wm^"_"^string_of_int w
| Uextends(anchor,w,wm) -> "__extends_"^string_of_int wm^"_"^string_of_int w

let cmpopv = function
| Cunknown -> "???"
| Ceq -> " == "
| Cneq -> " != "
| Cgt -> " > "
| Cgts -> " > "
| Cgte -> " >= "
| Cgtes -> " >= "
| Ceqwild -> " == "
| Cneqwild -> " != "
| Cltes -> " <= "
| Clte -> " <= "
| Clt -> " < "
| Clts -> " < "

let othlogop = ref Lunknown

let logopv = function
| Lunknown -> "???"
| Land -> " & "
| Lredand -> " & "
| Lrednand -> " ~& "
| Lor -> " | "
| Lredor -> " | "
| Lrednor -> " ~| "
| Lxor -> " ^ "
| Lxnor -> " ~^ "
| Lredxor -> " ^ "
| Lredxnor -> " ~^ "
| Lshiftl -> " << "
| Lshiftr -> " >> "
| Lshiftrs -> " >>> "

let rec cadd = function
| [] -> HEX 0
| ERR err :: tl -> ERR err
| HEX n :: [] -> HEX n
| SHEX n :: [] -> SHEX n
| FLT f :: [] -> FLT f
| FLT f :: _ -> ERR "addflt"
| STRING _ :: tl -> ERR "addstr"
| BIGINT n :: [] -> BIGINT n
| BIGINT n :: BIGINT m :: tl -> cadd (BIGINT (Int64.add n m) :: tl)
| (HEX n | SHEX n) :: BIGINT m :: tl -> cadd (BIGINT (Int64.add (Int64.of_int n) m) :: tl)
| BIGINT n :: (HEX m | SHEX m) :: tl -> cadd (BIGINT (Int64.add n (Int64.of_int m)) :: tl)
| BIN 'x' :: tl -> cadd (BIN 'x' :: tl)
| BIN _ :: tl -> cadd (BIN 'x' :: tl)
| HEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| HEX n :: SHEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: SHEX m :: tl -> cadd (SHEX (n+m) :: tl)
| (HEX _ | SHEX _ | BIGINT _) ::(ERR _|BIN _|STRING _|FLT _):: tl -> ERR "cadd"
| oth -> ERR "cadd"

let diropv = function
| Dinput -> "input"
| Doutput -> "output"
| Dinout -> "inout"
| Dvif _ -> "vif"
| Dinam str -> str
| Dport _ -> "ifport"
| Dunknown -> "inout"

let nltoken indent = ("\n"^if !indent > 0 then String.make (!indent*4) ' ' else "")

let othtok = ref INVALID

let tokencnv indent = function
| SP -> " "
| SEMI -> ";"
| COLON -> ":"
| COMMA -> ","
| LPAREN -> "("
| RPAREN -> ")"
| LBRACK -> "["
| RBRACK -> "]"
| LCURLY -> "{"
| RCURLY -> "}"
| LCOMMENT -> " /* "
| RCOMMENT -> " */ "
| LSHIFT -> "<<"
| RSHIFT -> ">>"
| AT -> "@"
| HASH -> "#"
| EQUALS -> "="
| PARAMETER -> "parameter"
| DOT -> "."
| PLUS -> "+"
| MINUS -> "-"
| STAR -> "*"
| POW -> "**"
| QUERY -> "?"
| QUOTE -> "'"
| DQUOTE -> "\""
| NL -> nltoken indent
| SRC (str1,int1) ->
    if (str1,int1) <> !oldsrc then
        begin
        oldsrc := (str1,int1);
        "\n/* "^str1^":"^string_of_int int1^" */"^nltoken indent
        end
    else ""
| DEFAULT -> "default"
| IDENT str -> str
| NUM (BIN n) -> (String.make 1 n)
| NUM (HEX n) -> (string_of_int n)
| NUM (SHEX n) -> (string_of_int n)
| NUM (BIGINT n) -> (Int64.to_string n)
| NUM (STRING s) -> ("\""^String.escaped s^"\"")
| NUM (FLT f) -> (string_of_float f)
| NUM (ERR err) -> ("NumberError:"^err)
| SIZED (w,n) -> (dumpsized w n)
| DIR str -> (diropv str)
| BEGIN None -> incr indent; "    begin"
| BEGIN (Some lbl) -> incr indent; ("    begin:"^lbl)
| END -> decr indent; "end"
| IFF -> "if"
| ELSE -> "else"
| ASSIGN -> "assign"
| ASSIGNMENT -> "="
| ASSIGNDLY -> "<="
| CASE -> incr indent; "case"
| ENDCASE -> decr indent; "endcase"
| CMPOP op -> (cmpopv op)
| WHILE -> "while"
| FOR -> "for"
| ALWAYS -> "always"
| POSEDGE -> "posedge"
| NEGEDGE -> "negedge"
| RETURN -> "return"
| LOGIC -> "logic"
| WIRE -> "wire"
| REG -> "reg"
| VSTRING -> "string"
| FUNCTION -> incr indent; "function"
| ENDFUNCTION -> decr indent; "endfunction"
| TASK -> incr indent; "task"
| ENDTASK -> decr indent; "endtask"
| MODULE -> incr indent; "module"
| ENDMODULE -> decr indent; "endmodule"
| INITIAL -> "initial"
| FINAL -> "final"
| INTERFACE -> incr indent; "interface"
| ENDINTERFACE -> decr indent; "endinterface"
| PACKAGE -> incr indent; "package"
| ENDPACKAGE -> decr indent; "endpackage"
| MODPORT -> "modport"
| INVALID -> failwith "invalid token"
| oth -> othtok := oth; failwith "unhandled token 606"

let tokendump fd = function
| SP -> output_string fd "SP\n"
| SEMI -> output_string fd "SEMI\n"
| COLON -> output_string fd "COLON\n"
| COMMA -> output_string fd "COMMA\n"
| LPAREN -> output_string fd "LPAREN\n"
| RPAREN -> output_string fd "RPAREN\n"
| LBRACK -> output_string fd "LBRACK\n"
| RBRACK -> output_string fd "RBRACK\n"
| LCURLY -> output_string fd "LCURLY\n"
| RCURLY -> output_string fd "RCURLY\n"
| LCOMMENT -> output_string fd "LCOMMENT\n"
| RCOMMENT -> output_string fd "RCOMMENT\n"
| LSHIFT -> output_string fd "LSHIFT\n"
| RSHIFT -> output_string fd "RSHIFT\n"
| AT -> output_string fd "AT\n"
| DOT -> output_string fd "DOT\n"
| PLUS -> output_string fd "PLUS\n"
| MINUS -> output_string fd "MINUS\n"
| STAR -> output_string fd "STAR\n"
| QUERY -> output_string fd "QUERY\n"
| QUOTE -> output_string fd "QUOTE\n"
| DQUOTE -> output_string fd "DQUOTE\n"
| NL -> output_string fd "NL\n"
| SRC _ -> output_string fd "SRC\n"
| DEFAULT -> output_string fd "DEFAULT\n"
| IDENT str -> output_string fd ("IDENT "^str^"\n")
| NUM n -> output_string fd ("NUM\n")
| SIZED (w,n) -> output_string fd "SIZED\n"
| DIR str -> output_string fd "DIR\n"
| BEGIN _ -> output_string fd "BEGIN\n"
| END -> output_string fd "END\n"
| IFF -> output_string fd "IFF\n"
| ELSE -> output_string fd "ELSE\n"
| ASSIGN -> output_string fd "ASSIGN\n"
| ASSIGNMENT -> output_string fd "ASSIGNMENT\n"
| ASSIGNDLY -> output_string fd "ASSIGNDLY\n"
| CASE -> output_string fd "CASE\n"
| CMPOP _ -> output_string fd "CMPOP\n"
| ENDCASE -> output_string fd "ENDCASE\n"
| WHILE -> output_string fd "WHILE\n"
| FOR -> output_string fd "FOR\n"
| ALWAYS -> output_string fd "ALWAYS\n"
| POSEDGE -> output_string fd "POSEDGE\n"
| NEGEDGE -> output_string fd "NEGEDGE\n"
| RETURN -> output_string fd "RETURN\n"
| LOGIC -> output_string fd "LOGIC\n"
| WIRE -> output_string fd "WIRE\n"
| REG -> output_string fd "REG\n"
| VSTRING -> output_string fd "VSTRING\n"
| FUNCTION -> output_string fd "FUNCTION\n"
| ENDFUNCTION -> output_string fd "ENDFUNCTION\n"
| TASK -> output_string fd "TASK\n"
| ENDTASK -> output_string fd "ENDTASK\n"
| MODULE -> output_string fd "MODULE\n"
| ENDMODULE -> output_string fd "ENDMODULE\n"
| INITIAL -> output_string fd "INITIAL\n"
| FINAL -> output_string fd "FINAL\n"
| INVALID -> output_string fd "INVALID\n"
| POW -> output_string fd "POW\n"
| INTERFACE -> output_string fd "INTERFACE\n"
| ENDINTERFACE -> output_string fd "ENDINTERFACE\n"
| PACKAGE -> output_string fd "PACKAGE\n"
| ENDPACKAGE -> output_string fd "ENDPACKAGE\n"
| MODPORT -> output_string fd "MODPORT\n"
| oth -> othtok := oth; failwith "unhandled token 673"

let tokenout fd indent tok = output_string fd (tokencnv indent tok)
let dumptok tok = let indent = ref 0 in tokencnv indent tok

let rec dumpitm = function
| VRF (id, _, []) -> "VRF (\""^id^"\", _, [])"
| UNKNOWN -> "UNKNOWN"
| XML (rw_lst) -> "XML("^dumplst rw_lst^")"
| EITM (str1, str2, str3, int2, rw_lst) -> "EITM("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumpi int2^", "^dumplst rw_lst^")"
| IO (str1, str2lst, typ2, dirop, str3, rw_lst) -> "IO("^dumps str1^", "^dumpstrlst str2lst^", "^dumptab typ2^", "^dumpdir dirop^", "^dumps str3^", "^dumplst rw_lst^")"
| VAR (str1, str2lst, typ2, str3) -> "VAR"^dumps str1^", "^dumpstrlst str2lst^", "^dumptab typ2^", "^dumps str3^")"
| IVAR (str1, str2, typ2, rw_lst, int3) -> "IVAR("^dumps str1^", "^dumps str2^", "^dumptab typ2^", "^dumplst rw_lst^", "^dumpi int3^")"
| CNST (int, cexp) -> "CNST("^dumpcnst (int, cexp)^")"
| VRF (str1, typ', rw_lst) -> "VRF("^dumps str1^", "^dumptab typ'^", "^dumplst rw_lst^")"
| TYP (idx, (typenc, str1, typmap, typ_lst)) -> "TYP("^dumptyp typenc^", "^dumps str1^", "^dumpmap typmap^", "^dumplst typ_lst^")"
| FNC (str1, str2, typ2, rw_lst) -> "FNC("^dumps str1^", "^dumps str2^", "^dumptab typ2^", "^dumplst rw_lst^")"
| TASKDEF (str1, str2, rw_lst) -> "TASK("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| TASKRF (str1, str2, rw_lst) -> "TASKRF("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| INST (str1, kind, str2lst, (str3, rw_lst)) -> "INST("^dumps str1^", "^dumptok kind^", "^dumpstrlst str2lst^"("^", "^dumps str3^", "^", "^dumplst rw_lst^"))"
| SFMT (str1, rw_lst) -> "SFMT("^dumps str1^", "^dumplst rw_lst^")"
| SYS (str1, str2, rw_lst) -> "SYS("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| TPLSRGS (str1, str2, int2, rw_lst) -> "TPLSRGS("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^")"
| VPLSRGS (str1, int2, rw_lst) -> "VPLSRGS("^dumps str1^", "^dumpi int2^", "^dumplst rw_lst^")"
| PORT (str1, str2, dirop, rw_lst) -> "PORT("^dumps str1^", "^dumps str2^", "^dumpdir dirop^", "^dumplst rw_lst^")"
| CA (str1, rw_lst) -> "CA("^dumps str1^", "^dumplst rw_lst^")"
| UNRY (unaryop, rw_lst) -> "UNRY("^dumpu unaryop^", "^dumplst rw_lst^")"
| SEL (str1, rw_lst) -> "SEL ("^dumps str1^", "^dumplst rw_lst^")"
| ASEL (rw_lst) -> "ASEL ("^dumplst rw_lst^")"
| SNITM (str1, rw_lst) -> "SNITM("^dumps str1^", "^dumplst rw_lst^")"
| ASGN (bool, str2, rw_lst) -> "ASGN("^dumpb bool^", "^dumps str2^", "^dumplst rw_lst^")"
| ARITH (arithop, rw_lst) -> "ARITH("^dumparith arithop^", "^dumplst rw_lst^")"
| LOGIC (logop, rw_lst) -> "LOGIC("^dumplog logop^", "^dumplst rw_lst^")"
| CMP (cmpop, rw_lst) -> "CMP("^dumpcmp cmpop^", "^dumplst rw_lst^")"
| FRF (str1, str2, rw_lst) -> "FRF("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| XRF (str1, str2, str3, str4, dirop) -> "XRF("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumps str4^", "^dumpdir dirop^")"
| PKG (str1, str2, rw_lst) -> "PKG("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| CAT (str1, rw_lst) -> "CAT("^dumps str1^", "^dumplst rw_lst^")"
| CPS (str1, rw_lst) -> "CPS("^dumps str1^", "^dumplst rw_lst^")"
| CND (str1, rw_lst) -> "CND("^dumps str1^", "^dumplst rw_lst^")"
| REPL (str1, int2, rw_lst) -> "REPL("^dumps str1^", "^dumpi int2^", "^dumplst rw_lst^")"
| MODUL (str1, str2, rw_lst, tmp) -> "MODUL("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^", ...)"
| BGN (None, rw_lst) -> "BGN(None,"^dumplst rw_lst^")"
| BGN (Some str1, rw_lst) -> "BGN(Some "^dumps str1^", "^dumplst rw_lst^")"
| RNG (rw_lst) -> "RNG("^dumplst rw_lst^")"
| ALWYS (str1, rw_lst) -> "ALWYS("^dumps str1^", "^dumplst rw_lst^")"
| SNTRE (rw_lst) -> "SNTRE("^dumplst rw_lst^")"
| IF (str1, rw_lst) -> "IF("^dumps str1^", "^dumplst rw_lst^")"
| INIT (str1, str2, rw_lst) -> "INIT("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IRNG (str1, rw_lst) -> "IRNG("^dumps str1^", "^dumplst rw_lst^")"
| IFC (str1, str2, rw_lst) -> "IFC("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IMP (str1, str2, rw_lst) -> "IMP("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IMRF (str1, str2, dir, rw_lst) -> "IMRF("^dumps str1^", "^dumps str2^", "^dumpdir dir^", "^dumplst rw_lst^")"
| JMPL (str1, rw_lst) -> "JMPL("^dumps str1^", "^dumplst rw_lst^")"
| JMPG (str1, rw_lst) -> "JMPG("^dumps str1^", "^dumplst rw_lst^")"
| JMPBLK (str1, rw_lst) -> "JMPBLK("^dumps str1^", "^dumplst rw_lst^")"
| CS (str1, rw_lst) -> "CS("^dumps str1^", "^dumplst rw_lst^")"
| CSITM (str1, rw_lst) -> "CSITM("^dumps str1^", "^dumplst rw_lst^")"
| WHL (rw_lst) -> "WHL("^dumplst rw_lst^")"
| FORSTMT (str1, str2, cmpop, VRF(str', typ', rw_lst'), (int1, cexp1), (int2, cexp2), (int3, cexp3), rw_lst) ->
    "FORSTMT("^dumps str1^", "^dumps str2^", "^dumpcmp cmpop^", "^"VRF("^dumps str'^", "^dumptab typ'^", "^dumplst rw_lst'^"), "^dumpcnst (int1, cexp1)^", "^dumpcnst (int2, cexp2)^", "^dumpcnst (int3, cexp3)^", "^dumplst rw_lst^")"
| FORSTMT _ -> "FORSTMT(...)"
| ARG (rw_lst) -> "ARG("^dumplst rw_lst^")"
| DSPLY (str1, str2, rw_lst) -> "DSPLY("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| FILS (str1, rw_lst) -> "FILS("^dumps str1^", "^dumplst rw_lst^")"
| FIL (str1, str2) -> "FIL"^dumps str1^", "^dumps str2^")"
| NTL (rw_lst) -> "NTL("^dumplst rw_lst^")"
| CELLS (rw_lst, attr) -> "CELLS("^dumplst rw_lst^", ...)"
| CELL (str1, str2, str3, str4, rw_lst) -> "CELL("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumps str4^", "^dumplst rw_lst^")" 
| POSPOS (str1, str2) -> "POSPOS"^dumps str1^", "^dumps str2^")"
| POSNEG (str1, str2) -> "POSNEG"^dumps str1^", "^dumps str2^")"
| NEGNEG (str1, str2) -> "NEGNEG"^dumps str1^", "^dumps str2^")"
| POSEDGE (str1) -> "POSEDGE"^dumps str1^")"
| NEGEDGE (str1) -> "NEGEDGE"^dumps str1^")"
| COMB -> "COMB"
| MODPORTFTR (str1, str2) -> "MODPORTFTR("^dumps str1^", "^dumps str2^")"
| TYPETABLE arr -> "" (* "[|"^String.concat ";\n\t" (Array.to_list (Array.mapi (fun idx (typenc, str1, typmap, typ_lst) -> string_of_int idx^": TYP("^dumptyp typenc^", "^dumps str1^", "^dumpmap typmap^", "^dumpmlst typ_lst^")") arr))^"|]" *)
| TIM _ -> "TIM"
| SCOPE tid -> "SCOPE "^tid
| ITM _ -> "ITM"
| CONTAINER (itms, top) -> "CONTAINER"
| CONSPACK (_, _) -> failwith "CONSPACK"
| CONSPACKMEMB (_, _) -> failwith "CONSPACKMEMB"

and dumplst lst = "["^String.concat ";\n\t" (List.map dumpitm lst)^"]"
and dumpcstlst lst = "["^String.concat ";\n\t" (List.map dumpcnst lst)^"]"

and dumpitms fd modul =
  Printf.fprintf fd "  {io =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,d,e,lst)) ->
    Printf.fprintf fd "(%s, (%s, %s, %s, %s, %s));\n" (dumps a) (dumps b) (dumptab c) (dumpdir d) (dumps e) (dumpcstlst lst)) !(modul.io);
  Printf.fprintf fd "     ]};\n";
  Printf.fprintf fd " v = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %s, %s, %s));\n" (dumps a) (dumps b) (dumptab c) d (dumptab e)) !(modul.v);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " iv = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %s, %s, %d));\n" (dumps a) b (dumptab c) (dumplst d) e) !(modul.iv);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " ir = {contents = [";
  List.iter (fun (a,b,c) -> Printf.fprintf fd "(%s, %s, %s)\n" a (dumps b) (dumptab c)) !(modul.ir);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " ca = {contents = [";
  List.iter (fun (a,b,c) -> Printf.fprintf fd "(%s, %s, %s)\n" (dumps a) (dumpitm b) (dumpitm c)) !(modul.ca);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " typ = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " alwys = {contents = [";
  List.iter (fun (a,b,lst) -> Printf.fprintf fd "(%s, %s, %s)\n" (dumps a) (dumpitm b) (dumplst lst)) !(modul.alwys);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " init = {contents = [";
  List.iter (fun (a,b,lst) ->
      Printf.fprintf fd "(%s, " (dumps a);
      tokenout fd (ref 0) b;
      Printf.fprintf fd ", %s)\n" (dumplst lst)) !(modul.init);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " func = {contents = [";
  List.iter (fun (a,(b,c,d,itms)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %s, %s\n" (dumps b) (dumptab c) (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.func);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " task = {contents = [";
  List.iter (fun (b,(c,d,itms)) ->
  Printf.fprintf fd "     (%s, %s, %s\n" (dumps b) (dumps c) (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.task);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " gen = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " imp = {contents = [";
  List.iter (fun (a,(b,lst)) ->
    Printf.fprintf fd "     (%s,%s, " (dumps a)  (dumps b);
    List.iter (fun (c,d) -> Printf.fprintf fd "     (%s, %s)\n" (dumps c) (dumpdir d)) lst;
  Printf.fprintf fd "         ));\n" ) !(modul.imp);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " inst =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,lst)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %s,\n" (dumps b) (dumps c);
  Printf.fprintf fd "       %s" (dumplst lst);
  Printf.fprintf fd "         ));\n" ) !(modul.inst);
  Printf.fprintf fd " cnst = {contents = [";
  Printf.fprintf fd "]};\n";
  if [] <> !(modul.needed) then
      begin
      Printf.fprintf fd " needed = {contents = ";
      let delim = ref '[' in
      List.iter (fun (a,b) ->
                     Printf.fprintf fd "%c(" !delim;
                     tokenout fd (ref 0) a;
                     Printf.fprintf fd ",%s)" (dumps b);
                     delim := ',') !(modul.needed);
                     Printf.fprintf fd "]};\n";
      end;
  Printf.fprintf fd "}\n";
  Printf.fprintf fd "  \n"

let cell_traverse fd attr indent (nam, subnam) =
    output_string fd (indent^"Searching hierarchy: "^nam^"\n");
    if List.mem_assoc subnam !(attr.modulexml) then
            let (_, xlst', names') = List.assoc subnam !(attr.modulexml) in
                begin
                output_string fd (indent^"Cell traverse: "^subnam^"\n");
                List.iter (function
                    | IO _ -> ()
                    | VAR (_, inflst, typ', "ifaceref") ->
		        List.iter (fun itm -> output_string fd (indent^"Ifaceref: "^itm^":"^dumptab typ'^"\n")) inflst
                    | VAR _ -> ()
                    | IVAR _ -> ()
                    | TYP _ -> ()
                    | FNC _ -> ()
                    | TASKDEF _ -> ()
                    | CA _ -> ()
                    | BGN _ -> ()
                    | INIT _ -> ()
                    | ALWYS _ -> ()
                    | IMP _ -> ()
                    | SYS _ -> ()
                    | INST (_, _, _, (kind, portlst)) ->
                         output_string fd (indent^"Searching: "^kind^"\n");
                         let (_, xlst''', names'') = List.assoc kind !(attr.modulexml) in
                         List.iter (function
                        | PORT (_, formal, Dvif bus, [VRF (actual, _, [])]) ->
			if List.mem_assoc formal names'' then
			    begin
			    let formtyp = List.assoc formal names'' in
			    let (typenc1,str1,typmap1,typmaplst1) = !formtyp in
			    if List.mem_assoc actual names' then
			        begin
				let actualtyp = List.assoc actual names' in
				let (typenc2,str2,typmap2,typmaplst2) = !actualtyp in
                                let str3 = if str2 = "" then !bus else str2 in
(*			        if !str1 <> !str2 then *)
				    begin
				    output_string fd (indent^"formal: "^formal^", actual: "^actual^"\n");
				    output_string fd (indent^"formaltype: "^dumptab !formtyp^", actualtype: "^dumptab !actualtyp^"\n");
				    formtyp := (typenc1,str3,typmap1,typmaplst1);
				    end;
				end
			    else
				begin
				output_string fd (indent^"Actual "^actual^" not found\n");
				List.iter (fun (k,_) -> output_string fd ("List: "^k)) names'
				end
			    end    
			else
			    begin
			    output_string fd (indent^"Formal "^formal^" not found"^"\n");
			    List.iter (fun (k,_) -> output_string fd ("List: "^k)) names''
			    end
                        | _ -> ()) portlst;
                         output_string fd ("completed: "^kind^"\n");
			 let fd' = open_out (kind^".names") in
			 List.iter (fun (a,b) -> output_string fd' (a^":"^dumptab !b^"\n")) names'';
			 close_out fd'
                    | oth -> cellopt := Some oth; failwith ("cellopt: "^dumpitm oth)) xlst'
                end
        else output_string fd (indent^subnam^":missing\n")

let namedcnt = ref 0

let rec uniqnam cnt stem instances =
    let uniq' = if cnt = 0 then stem else stem^"_"^string_of_int cnt in
    if List.mem_assoc uniq' !instances then
        uniqnam (cnt+1) stem instances
    else
        uniq'

let empty_itms names'' = {
io=ref [];
v=ref [];
iv=ref [];
ir=ref [];
ca=ref [];
alwys=ref [];
init=ref [];
func=ref [];
task=ref [];
gen=ref [];
imp=ref [];
inst=ref [];
cnst=ref [];
needed=ref [];
remove_interfaces = false;
names''=names'' }

let rev_itms prev = {
io=ref (List.rev !(prev.io));
v=ref (List.rev !(prev.v));
iv=ref (List.rev !(prev.iv));
ir=ref (List.rev !(prev.ir));
ca=ref (List.rev !(prev.ca));
alwys=ref (List.rev !(prev.alwys));
init=ref (List.rev !(prev.init));
func=ref (List.rev !(prev.func));
task=ref (List.rev !(prev.task));
gen=ref (List.rev !(prev.gen));
imp=ref (List.rev !(prev.imp));
inst=ref (List.rev !(prev.inst));
cnst=ref (List.rev !(prev.cnst));
needed=ref (List.rev !(prev.needed));
remove_interfaces = prev.remove_interfaces;
names''=prev.names'' }

let copy_itms prev = {
io=ref !(prev.io);
v=ref !(prev.v);
iv=ref !(prev.iv);
ir=ref !(prev.ir);
ca=ref !(prev.ca);
alwys=ref !(prev.alwys);
init=ref !(prev.init);
func=ref !(prev.func);
task=ref !(prev.task);
gen=ref !(prev.gen);
imp=ref !(prev.imp);
inst=ref !(prev.inst);
cnst=ref !(prev.cnst);
needed=ref !(prev.needed);
remove_interfaces = prev.remove_interfaces;
names''=prev.names'' }

let num x = NUM (HEX x)

let mkextendfunc = function
| Uextends(anchor,w,wm) as op ->
let fref = unaryopv op in
let typ1 = (BASDTYP, "logic", TYPRNG (HEX(w-1), HEX 0), []) in
let typ2 = (BASDTYP, "logic", TYPRNG (HEX(wm-1), HEX 0), []) in
let arg = VRF ("arg", typ2, []) in
let cext = CNST (32, SHEX (w-wm)) in
let wid1 = CNST (32, HEX 1) in
let wmin = CNST (32, HEX (wm-1)) in
let sel = SEL (anchor, arg :: wmin :: wid1 :: []) in
let repl = if w-wm > 1 then REPL (anchor, 3, sel :: cext :: []) else sel in
let body = 
  [IO (anchor, [fref], typ1, Doutput, "logic", []);
   IO (anchor, ["arg"], typ2, Dinput, "logic", []);
   BGN (None,
  [ASGN (false, anchor,
    [CAT (anchor, repl :: arg :: []);
     VRF (fref, typ1, [])])])] in (anchor,typ1,body,empty_itms [])
| (Unknown|Unot|Ulognot|Unegate|Uunsigned|Usigned|Uextend _) as op -> failwith (unaryopv op)

let rec expr modul = function
| VRF (id, _, []) -> IDENT id :: []
| CNST (s,n) -> SIZED (s,n) :: []
| UNRY (Uextend(w,wm), expr1 :: []) -> LCURLY :: SIZED (w-wm, HEX 0) :: COMMA :: expr modul expr1 @ [RCURLY]
| UNRY ((Usigned|Uunsigned|Uextends _) as op, expr1 :: []) ->
    IDENT (unaryopv op) :: LPAREN :: expr modul expr1 @ [RPAREN]
| UNRY (op, expr1 :: []) -> LPAREN :: IDENT (unaryopv op) :: expr modul expr1 @ [RPAREN]
| CMP ((Clts|Cltes|Cgtes|Cgts) as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr modul (UNRY (Usigned, expr1 :: [])) @ CMPOP op :: expr modul (UNRY (Usigned, expr2 :: [])) @ [RPAREN]
| CMP (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ CMPOP op :: expr modul expr2 @ [RPAREN]
| LOGIC (op, expr1 :: []) -> LPAREN :: IDENT (logopv op) :: expr modul expr1 @ [RPAREN]
| LOGIC (Lshiftrs as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr modul (UNRY (Usigned, expr1 :: [])) @ (IDENT (logopv op) :: expr modul expr2) @[RPAREN]
| LOGIC (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ (IDENT (logopv op) :: expr modul expr2) @[RPAREN]
| ARITH (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ (IDENT (arithopv op) :: expr modul expr2) @[RPAREN]
| SEL ("", ((VRF _ | XRF _ | ASEL _) as expr1) :: (CNST(szlo, lo')) :: (CNST(szw,wid')) :: []) ->
    expr modul expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: NUM lo' :: RBRACK :: []
    | _ -> LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: [])
| SEL ("", ((VRF _ | XRF _ | ASEL _) as expr1) :: expr2 :: CNST(szw, wid') :: []) ->
    expr modul expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: expr modul expr2 @ [RBRACK]
    | _ -> LBRACK :: expr modul expr2 @ [PLUS;COLON] @ (NUM wid' :: RBRACK :: []))
| SEL ("", expr1 :: CNST (32,(HEX 0 | SHEX 0)) :: CNST(szw,HEX wid') :: []) ->
    expr modul (LOGIC (Land, expr1 :: CNST (wid', HEX ((1 lsl wid') - 1)) :: []))
| SEL (o, CND ("", expr1 :: lft :: rght :: []) :: expr2 :: expr3 :: []) ->
    LPAREN :: expr modul expr1 @ [QUERY] @ expr modul (SEL (o, lft :: expr2 :: expr3 :: [])) @ [COLON] @ expr modul (SEL (o, rght :: expr2 :: expr3 :: [])) @ [RPAREN]
(* *)
| SEL (orig, (UNRY (Uextend (n,n'), VRF (id, typ', []) :: []) :: (CNST _ as expr2) :: (CNST _ as expr3) :: [])) ->
    expr modul (SEL (orig, VRF (id, typ', []) :: expr2 :: expr3 :: []))
(* *)
| SEL (orig,
     (LOGIC (Lshiftl,
       (UNRY (Uextend (n, n'),
         (VRF (id, typ', []) :: [])) ::
        (CNST _ as expr1) ::[])) ::
      (CNST _ as expr2) :: (CNST _ as expr3) :: [])) ->
(*
  let exp' = LOGIC (Lshiftl,
       (UNRY (Uextend (n, n'),
         (VRF (id, typ', []) :: [])) ::
        (expr1) ::[])) in
*)
  expr modul (SEL (orig, LOGIC (Lshiftl, VRF (id, typ', []) :: expr1 :: []) :: expr2 :: expr3 :: []))
| SEL (orig, (CNST (32, SHEX n) :: CNST (32, (HEX lo'|SHEX lo')) :: CNST (32, (HEX wid'|SHEX wid')) :: [])) ->
    SIZED (wid', SHEX (n asr lo')) :: []
    
| ASEL (VRF (lval, _, []) :: expr1 :: []) -> IDENT lval :: LBRACK :: expr modul expr1 @ [RBRACK]
| ASEL (ASEL _ as multi :: expr' :: []) -> expr modul multi @ LBRACK :: expr modul expr' @ [RBRACK]
| CND ("", expr1 :: lft :: rght :: []) -> LPAREN :: expr modul expr1 @ [QUERY] @ expr modul lft @ [COLON] @ expr modul rght @ [RPAREN]
| CAT ("", expr1 :: expr2 :: []) -> LCURLY :: expr modul expr1 @ [COMMA] @ expr modul expr2 @ [RCURLY]
| FRF ("", fref, arglst) -> let delim = ref LPAREN in
    let lst = IDENT fref :: List.flatten (List.map (function
        | ARG (arg :: []) -> let lst = !delim :: expr modul arg in delim := COMMA; lst
        | _ -> [QUERY]) arglst) in
    lst @ (if !delim = LPAREN then [LPAREN;RPAREN] else [RPAREN])
| REPL ("", tid, arg :: CNST (sz,n') :: []) -> LCURLY :: NUM n' :: LCURLY :: expr modul arg @ [RCURLY;RCURLY]
| IRNG (nam, expr2 :: expr1 :: []) -> IDENT nam :: LBRACK :: expr modul expr1 @ [COLON] @ expr modul expr2 @ [RBRACK]
| XRF ("", id, tid, dotted, dirop) as xrf ->
    xrflst := xrf :: !xrflst;
    IDENT (dotted^(match dirop with Dinam _ when modul.remove_interfaces -> "_" | _ -> ".")^id) :: []
| TPLSRGS ("", id, tid, _) -> IDENT "$test$plusargs" :: LPAREN :: DQUOTE :: IDENT id :: DQUOTE :: RPAREN :: []
| VPLSRGS ("", tid, CNST (len, fmt) :: VRF (arg, _, []) :: []) -> IDENT "$value$plusargs" :: LPAREN :: NUM fmt :: COMMA :: IDENT arg :: RPAREN :: []
| SYS ("", "$rand", []) -> SP :: IDENT "$random" :: []
| SYS ("", "$powsu", fst::snd::[]) -> LPAREN :: expr modul fst @ POW :: expr modul snd @ RPAREN :: []
| SYS ("", "$streaml", hd::tl) -> LCURLY :: LSHIFT :: LCURLY :: expr modul hd @ RCURLY :: RCURLY :: []
| SYS ("", "$initarray", arglst) -> let delim = ref SP in
    let initarr ix itm = let lst' = !delim :: num ix :: COLON :: expr modul itm in delim := COMMA; lst' in
    QUOTE :: LCURLY :: List.flatten (List.mapi initarr arglst) @ [RCURLY]
| SYS ("", fn, arglst) -> IDENT fn :: LPAREN :: eiter modul SP arglst @ [RPAREN]
(*
| SEL _ as sel -> IDENT (dumpitm sel) :: []
*)
| SEL ("", expr1 :: lo :: wid :: []) as sel -> selopt := Some sel; failwith "expr: selopt"
| TIM (origin) -> IDENT "$time" :: []
| ITM ("", itm, CNST (s, n) :: []) -> SIZED (s,n) :: []
| SCOPE n -> IDENT ("SCOPE: "^n) :: []
| SFMT (fmt,
    [SYS ("", "$itord",
	      [VRF (vrf, (BASDTYP, "logic", TYPRNG (HEX hi, HEX lo), []), [])])]) -> IDENT ("sfmt"^fmt) :: []
| TASKRF ("", nam, ARG _ :: _) as task -> IDENT(dumpitm task) :: []
| oth -> exprothlst := oth :: !exprothlst; failwith ("exprothlst: "^dumpitm oth)

and eiter modul tok = function
| IRNG (_, [CNST (w, HEX lo); CNST (w', HEX hi)]) :: [] ->
    eiter modul SP (Array.to_list (Array.init (hi-lo+1) (fun x -> CNST (w, HEX (x+lo)))))
| lst ->
    let delim = ref tok in
    List.flatten (List.map (fun itm -> let lst' = !delim :: expr modul itm in delim := COMMA; lst') lst)

let implicit s =
  let sub = "__pinNumber" in
  let l = String.length s and m = String.length sub in
  l > m && String.sub s 0 m = sub

let rec portconn modul = function
| VRF (id, _, []) -> DOT :: IDENT id :: []
| PORT ("", id_o, dir, []) -> DOT :: IDENT id_o :: LPAREN :: RPAREN :: []
| PORT ("", id_i, dir, expr1 :: []) when implicit id_i -> LPAREN :: expr modul expr1 @ [RPAREN]
| PORT ("", id_i, dir, expr1 :: []) -> DOT :: IDENT id_i :: LPAREN :: expr modul expr1 @ [RPAREN]
| RNG [CNST (_,lft); CNST (_,rght)] -> LCOMMENT :: NUM lft :: COLON :: NUM rght :: RCOMMENT :: []
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let stmtdly = function
| false -> SP :: ASSIGNMENT :: SP :: []
| true -> SP :: ASSIGNDLY :: SP :: []

let rec reformat0 = function
| [] -> []
| END :: SEMI :: (ENDCASE|ENDFUNCTION|ELSE|DEFAULT|SP as tok) :: tl -> END :: reformat0 ( tok :: tl )
| ENDCASE :: SEMI :: ELSE :: tl -> ENDCASE :: reformat0 ( ELSE :: tl )
| SEMI :: SEMI :: END :: tl -> SEMI :: reformat0 ( END :: tl )
| oth :: tl -> oth :: reformat0 tl

let rec reformat1 = function
| [] -> []
| prior :: (BEGIN _|END|ELSE as tok) :: tl when prior <> NL -> prior :: NL :: tok :: reformat1 (NL :: tl)
| (BEGIN _|END|ELSE as tok) :: post :: tl when post <> NL && post <> COLON -> NL :: tok :: NL :: post :: reformat1 tl
| DIR dir :: tl -> NL :: DIR dir :: reformat1 tl
| SP :: DOT :: tl -> NL :: DOT :: reformat1 tl
| COMMA :: DOT :: tl -> COMMA :: NL :: DOT :: reformat1 tl
| SEMI :: IFF :: tl -> SEMI :: NL :: IFF :: reformat1 tl
| NL :: NL :: tl -> reformat1 (NL :: tl)
| oth :: tl -> oth :: reformat1 tl

let rec reformat2 = function
| [] -> []
| BEGIN lbl :: NL :: NL :: tl -> reformat2 (BEGIN lbl :: NL :: tl)
| ENDCASE :: SEMI :: tl -> ENDCASE :: reformat2 ( NL :: tl )
| END :: NL :: SEMI :: NL :: (END|ENDCASE as end') :: tl -> reformat2 (END :: NL :: end' :: tl)
| END :: NL :: SEMI :: NL :: ALWAYS :: tl -> END :: NL :: NL :: reformat2 (ALWAYS :: tl)
| END :: NL :: SEMI :: NL :: tl -> END :: reformat2 (SEMI :: NL :: tl)
| ELSE :: NL :: IFF :: tl -> ELSE :: SP :: IFF :: reformat2 tl
| END :: NL :: NL :: ELSE :: tl -> END :: reformat2 (NL :: ELSE :: tl)

| oth :: tl -> oth :: reformat2 tl

let reviter modul lst =
    let delim = ref COLON in
    List.rev (List.flatten (List.map (fun itm -> let lst' = !delim :: expr modul itm in delim := COMMA; lst') lst))

let rec cntbasic = function
| (BASDTYP, typ, TYPRNG(HEX hi, HEX lo), []) when (function "logic"|"integer"|"int"|"bit"|"wire"|"byte"|"longint" -> true | _ -> false) typ -> ARNG (hi, lo) :: []
| (STRDTYP,_,typmap,rw_lst) -> ADD (List.map cntmembers rw_lst) :: []
| (UNIDTYP,_,typmap,rw_lst) -> MAX (List.map cntmembers rw_lst) :: []
| (BASDTYP, "reg", TYPNONE, []) -> REG :: []
| (BASDTYP, "wire", TYPNONE, []) -> WIRE :: []
| (BASDTYP, ("logic"|"bit"), TYPNONE, []) -> BIT :: []
| (BASDTYP, "real", TYPNONE, []) -> REAL :: []
| (BASDTYP, "string", TYPNONE, []) -> STRING :: []
| (IFCRFDTYP _, _, TYPNONE, []) -> UNKARR :: []
| (PACKADTYP, _, RECTYP subtyp, TYPRNG((HEX n|SHEX n), (HEX n'|SHEX n'))::_) -> PACKED(n, n') :: findmembers subtyp
| (UNPACKADTYP, _, RECTYP subtyp, TYPRNG ((HEX n|SHEX n), (HEX n'|SHEX n'))::_) -> UNPACKED(n, n') :: findmembers subtyp
| (MEMBDTYP, id, SUBTYP subtyp, []) -> failwith ("SUBTYP")
| (BASDTYP, ("logic"|"bit"|"wire"|"reg"), TYPRNG(lft,rght), []) -> VECTOR (lft, rght) :: []
| oth -> typopt := Some oth; failwith ("typopt;;1425:"^dumptab oth)

and cntmembers = function
| TYPMEMBER typ' -> MEMBER (findmembers typ')
| oth -> memothlst := oth :: !memothlst; failwith "memothlst"

and findmembers' typ' =
        let (t,s,m1,ml) = typ' in
        (cntbasic typ', s="const", match m1 with TYPRNG _ -> true | _ -> false)

and findmembers typ' = let (widlst,cnst,rng) = findmembers' typ' in widlst

let rec comment' = function
| UNKARR -> "UNKARR"
| BIT -> "BIT"
| REG -> "REG"
| WIRE -> "WIRE"
| REAL -> "REAL"
| STRING -> "STRING"
| ARNG(int1,int2) -> "ARNG("^string_of_int int1^":"^string_of_int int2^")"
| PACKED(int1,int2) -> "PACKED("^string_of_int int1^":"^string_of_int int2^")"
| UNPACKED(int1,int2) -> "UNPACKED("^string_of_int int1^":"^string_of_int int2^")"
| ADD arrtyp_lst -> "ADD ["^String.concat ";" (List.map comment' arrtyp_lst)^"]"
| MAX arrtyp_lst -> "MAX ["^String.concat ";" (List.map comment' arrtyp_lst)^"]"
| MEMBER arrtyp_lst -> "MEMBER ["^String.concat ";" (List.map comment' arrtyp_lst)^"]"
| VECTOR (lft,rght) -> "VECTOR ("^dumpr lft^", "^dumpr rght^")"

let comment lst = LCOMMENT :: List.flatten (List.map (fun itm -> SP :: IDENT (comment' itm) :: []) (List.rev lst)) @ RCOMMENT :: []

let rec widadd = function
| [] -> []
| BIT :: [] -> BIT :: []
| BIT :: BIT :: [] -> ARNG(1,0) :: []
| ARNG(hi,lo) :: BIT :: [] -> ARNG(hi+1,lo) :: []
| BIT :: ARNG(hi,lo) :: [] -> ARNG(hi+1,lo) :: []
| ARNG(hi,lo) :: [] -> ARNG(hi,lo) :: []
| ADD lst :: tl -> widadd lst @ tl
| MAX lst :: tl -> widmax lst @ tl
| MEMBER lst :: tl -> widadd (widadd lst @ widadd tl)
| ARNG(hi,lo) :: ARNG(hi',lo') :: tl -> widadd (ARNG(hi+hi'-lo-lo'+1, 0) :: tl)
| ARNG(hi,lo) :: MEMBER[BIT] :: tl -> widadd (ARNG(hi-lo+2, 0) :: tl)
| ARNG(hi,lo) :: MEMBER[ARNG(hi',lo')] :: tl -> widadd (ARNG(hi+hi'-lo-lo'+1, 0) :: tl)
| ARNG(hi,lo) :: MEMBER[oth] :: tl -> widadd (ARNG(hi-lo+1, 0) :: oth :: tl)
| ARNG(hi,lo) :: ADD lst :: tl -> widadd (ARNG(hi-lo+1, 0) :: lst @ tl)
| PACKED(hi,lo) :: ARNG(hi',lo') :: tl -> widadd (ARNG((hi-lo+1)*(hi'-lo'+1)-1, 0) :: tl)
| PACKED(high,low) :: PACKED(hi,lo) :: ARNG(hi',lo') :: tl -> widadd (ARNG((high-low+1)*(hi-lo+1)*(hi'-lo'+1)-1, 0) :: tl)
| PACKED(high,low) :: ADD lst :: tl -> widadd (widadd (ARNG((high-low+1)-1,0) :: lst@tl))
| oth -> arropt :=  oth; failwith ("arropt1807;; ["^String.concat ";" (List.map comment' oth)^"]")

and widmax = function
| [] -> []
| BIT :: [] -> BIT :: []
| BIT :: BIT :: [] -> ARNG(1,0) :: []
| ARNG(hi,lo) :: BIT :: [] -> ARNG(hi,lo) :: []
| BIT :: ARNG(hi,lo) :: [] -> ARNG(hi,lo) :: []
| ARNG(hi,lo) :: [] -> ARNG(hi,lo) :: []
| ADD lst :: tl -> widadd lst @ tl
| MAX lst :: tl -> widmax lst @ tl
| MEMBER lst :: tl -> widmax (widmax lst @ widmax tl)
| ARNG(hi,lo) :: ARNG(hi',lo') :: tl -> widmax (ARNG(max(hi-lo) (hi'-lo'), 0) :: tl)
| ARNG(hi,lo) :: MEMBER[ARNG(hi',lo')] :: tl -> widmax (ARNG(max(hi-lo) (hi'-lo'), 0) :: tl)
| PACKED(hi,lo) :: ARNG(hi',lo') :: tl -> widmax (ARNG((hi-lo+1)*(hi'-lo'+1)-1, 0) :: tl)
| oth -> arropt :=  oth; failwith ("arropt1822;; ["^String.concat ";" (List.map comment' oth)^"]")

let widadd lst = match (widadd lst) with
| (ARNG _ :: []) as rng -> rng
| oth -> arropt :=  oth; failwith ("arropt1482;; ["^String.concat ";" (List.map comment' oth)^"]")

let rec cnstexpr = function
| ERR string -> LCOMMENT :: SP :: IDENT string :: SP :: RCOMMENT :: []
| BIN char -> SP :: []
| HEX int -> SP :: num int :: []
| SHEX int -> SP :: []
| STRING string -> SP :: IDENT string :: SP :: []
| ENUMVAL (int, string) -> SP :: []
| FLT float -> SP :: []
| BIGINT int64t -> SP :: []
| CNSTEXP (Aunknown, STRING fn::rght::[]) -> IDENT fn :: LPAREN :: cnstexpr rght @ [RPAREN]
| CNSTEXP (arithop, cexp_lst) -> List.flatten (List.mapi (fun ix itm -> (if ix > 0 then IDENT (arithopv arithop) else SP) :: cnstexpr itm) cexp_lst)

let rec widshow id rng = function
| [] -> []
| UNKARR :: tl -> failwith "UNKARR"
| (BIT|REG|WIRE) :: tl -> SP :: IDENT id :: SP :: widshow id rng tl
| STRING :: tl -> SP :: IDENT id :: SP :: widshow id rng tl
| ARNG(hi,lo) :: PACKED(hi',lo') :: tl -> widshow id rng (ARNG((hi-lo+1)*(hi'-lo'+1)-1 , 0) :: tl)
| ARNG(hi,lo) :: tl -> LBRACK :: num hi :: COLON :: num lo :: RBRACK :: SP :: IDENT id :: SP :: widshow id rng tl
| PACKED(hi,lo) :: tl -> LBRACK :: num hi :: COLON :: num lo :: RBRACK :: SP :: widshow id rng tl
| UNPACKED(hi,lo) :: tl -> widshow id rng tl @ SP :: LBRACK :: num hi :: COLON :: num lo :: RBRACK :: []
| ADD lst :: tl -> widshow id rng (widadd lst @ tl)
| MAX lst :: tl -> widshow id rng (widmax lst @ tl)
| VECTOR (lft,rght) :: tl -> LBRACK :: cnstexpr lft @ COLON :: cnstexpr rght @ RBRACK :: SP :: IDENT id :: widshow id rng tl
| oth -> arropt :=  oth; failwith ("arropt1501;; ["^String.concat ";" (List.map comment' oth)^"]")

let widshow id rng lst = widshow id rng (List.rev lst)

let othvar = ref None

let varlst modul delim typ' id =
    let (widlst,cnst,rng) as found = findmembers' typ' in
    let kind = match found with
        | (STRING :: [],_,_) -> VSTRING
        | (UNPACKED _ :: ARNG _ :: [],_,_) -> REG
        | (BIT :: [], false, _) -> LOGIC
        | (ARNG _ :: [], false, _) -> LOGIC
        | (WIRE :: [], false, _) -> WIRE
        | (REG :: [], false, _) -> REG
        | (VECTOR _ :: [], false, _) -> LOGIC
        | oth -> othvar := Some oth; failwith "othvar" in
    let decl = !delim :: kind :: SP :: widshow id rng widlst in decl @
    comment widlst @ (if List.mem_assoc id !(modul.cnst) then
                         begin
                         let (isc,init) = List.assoc id !(modul.cnst) in
                         if cnst && isc then
                             begin
                             SP :: ASSIGNMENT :: SP :: SIZED init :: []
                             end
                         else
                             begin
                             SEMI :: NL :: INITIAL :: SP :: IDENT id :: SP :: ASSIGNMENT :: SP :: SIZED init :: []
                             end
                         end else [])

let rec iter2 delim modul dly lst =
    List.flatten (List.map (fun itm -> let lst = !delim @ cstmt modul dly itm in delim := [SEMI;NL]; lst) lst)

and csitm modul dly origin cexplst =
        let lbl, stmts = List.partition (function CNST _ | VRF _ | LOGIC _ | IRNG _ | SEL _ -> true | _ -> false) cexplst in
        (if lbl <> [] then (eiter modul SP lbl) else DEFAULT :: []) @
        COLON :: (match stmts with
                        | [] -> []
                        | itm :: [] -> NL :: cstmt modul dly itm
                        | _ -> BEGIN None :: iter2 (ref []) modul dly stmts @ [SEMI;END])

and ifcond = function
| LPAREN :: tl -> IFF :: SP :: LPAREN :: tl
| oth -> IFF :: SP :: LPAREN :: oth @ (RPAREN :: [])

and ewidth = function
| CAT (_, lst) -> fold1 (+) (List.map ewidth lst)
| SEL (_, VRF (_, _, []) :: CNST (_,_) :: CNST (_,HEX wid) :: []) -> wid
| SEL (_, UNRY (Uextends _, _) :: CNST (_,_) :: CNST (_,HEX wid) :: []) -> wid
| CNST (n, _) -> n
| oth -> widthlst := oth :: !widthlst; failwith "widthlst"

and cstmt modul dly = function
| JMPG (_,[]) -> []
| BGN(lbl, rw_lst) ->
    let decl,body = List.partition (function VAR _ -> true | _ -> false) rw_lst in
    BEGIN lbl :: iter2 (ref []) modul dly (decl@body) @ [SEMI;END]
| IF("", cnd :: then_stmt :: []) -> ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt)
| IF("", cnd :: (BGN _ as then_stmt) :: else_stmt :: []) ->
    ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt) @ [ELSE] @ cstmt modul dly else_stmt
| IF("", cnd :: then_stmt :: else_stmt :: []) ->
    ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt) @ [SEMI;ELSE] @ cstmt modul dly else_stmt
| ASGN(true, "", 
    src ::
    SEL (_,
        ASEL (VRF (lval, _, []) ::
        expr1 ::
        []) ::
    CNST (szlo,lo') ::
    CNST (szw,wid') ::
    []) ::
 []) -> 
    IDENT lval :: LBRACK :: expr modul expr1 @ RBRACK :: LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: stmtdly true @ expr modul src 
| ASGN(dly, _, src :: dst :: []) ->
    expr modul dst @ stmtdly dly @ expr modul src
| CS ("", sel :: lst) ->
    let delim = ref SP in
    CASE :: LPAREN :: expr modul sel @
    RPAREN :: NL :: List.flatten (List.map (function
        | CSITM (o, cexplst) ->
            let lst = !delim :: csitm modul dly o cexplst in delim := SEMI; lst
        | oth -> csothitm := oth; failwith "csothitm") lst) @ [SEMI;ENDCASE]
| CA("", rght::lft::[]) -> ASSIGN :: SP :: expr modul lft @ stmtdly dly @ expr modul rght
| VAR ("", idlst, typ', _) -> List.flatten (List.map (fun id -> varlst modul (ref NL) typ' id) idlst)
| FORSTMT (o,kind,cnd,VRF(ix, typ', []),strt,stop,inc,stmts) ->
    let compnd = List.length stmts > 1 in
    FOR :: LPAREN :: (if kind = "logic" then varlst modul (ref SP) typ' ix else if kind <> "" then [IDENT kind; SP; IDENT ix] else [IDENT ix]) @
    ASSIGNMENT :: SIZED strt :: SEMI ::
    SIZED stop :: CMPOP cnd :: IDENT ix :: SEMI ::
    IDENT ix :: ASSIGNMENT :: IDENT ix :: PLUS :: SIZED inc :: RPAREN ::
    (if compnd then BEGIN None else SP) :: iter2 (ref []) modul dly stmts @ (if compnd then [SEMI;END] else [SEMI])
| DSPLY ("", typ, SFMT (fmt, arglst) :: []) -> IDENT typ :: LPAREN :: DQUOTE :: IDENT fmt :: DQUOTE :: eiter modul COMMA arglst @ [RPAREN]
| DSPLY ("", typ, SFMT (fmt, arglst) :: expr1 :: []) ->
    IDENT typ :: LPAREN :: expr modul expr1 @ (COMMA :: DQUOTE :: IDENT fmt :: DQUOTE :: COMMA :: eiter modul SP arglst) @ [RPAREN]
| SYS ("", ("$fopen" as fn), frst::secnd::thrd::[]) ->
    expr modul frst @ ASSIGNMENT :: IDENT fn :: LPAREN :: expr modul secnd @ COMMA :: expr modul thrd @ [RPAREN]
| SYS ("", fn, arglst) -> IDENT fn :: LPAREN :: eiter modul SP arglst @ [RPAREN]
| CNST((s,n)) -> SIZED (s,n) :: []
| TASKRF ("", nam, arglst) -> IDENT nam :: (if arglst <> [] then eiter modul LPAREN arglst @ [RPAREN] else [])
| JMPL("", rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| JMPBLK("", rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| WHL(rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| CMP (op, [lhs; rhs]) -> expr modul lhs @ CMPOP op :: expr modul rhs
| IO ("", ["ch"], (BASDTYP, "byte", TYPRNG (HEX 7, HEX 0), []), Dinput, "byte", []) as io -> IDENT (dumpitm io) :: []
| SNTRE (rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| SNITM (itm, rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| VRF (vrf, _, rw_lst) -> BEGIN None :: iter2 (ref []) modul dly rw_lst @ [END]
| SCOPE s -> LCOMMENT :: IDENT s :: RCOMMENT :: []
| UNKNOWN -> LCOMMENT :: IDENT "UNKNOWN" :: RCOMMENT :: []
| LOGIC _ as exp -> LCOMMENT :: expr modul exp @ RCOMMENT :: []
| CONTAINER (itms, SCOPE scope) -> LCOMMENT :: SP :: IDENT scope :: SP :: RCOMMENT :: []
| oth -> stmtothlst := oth :: !stmtothlst; failwith ("stmtothlst: "^dumpitm oth)

let flatten1 modul dly = function
| BGN _ :: tl as lst -> let delim = ref SP in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt modul dly itm in delim := SEMI; lst') lst)
| lst -> let delim = ref (BEGIN None) in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt modul dly itm in delim := SEMI; lst') lst) @ [SEMI;END]

let find_source origin =
    let last = ref 0 in
    for i = String.length origin - 1 downto 0 do
        match origin.[i] with '0'..'9' -> last := i | _ -> ();
    done;
    let k = String.sub origin 0 !last in
    let source = if Hashtbl.mem files k then Hashtbl.find files k else (origin^": origin_unknown") in
    let line = String.sub origin !last (String.length origin - !last) in
    (source, try int_of_string line with err -> 0)

let fsrc src = SRC (find_source src)
let catch_escapes = ref true

let rec optim2 = function
| [] -> []
| ASGN(dly, o1, expr :: tmp1 :: []) :: ASGN(dly', o2, SEL (_, tmp1' :: lst) :: dst :: []) ::
  ASGN(dly'', o3, expr' :: tmp2 :: []) :: ASGN(dly''', o4, SEL (_, tmp2' :: lst') :: dst' :: []) :: tl when (tmp1=tmp1') && (tmp2=tmp2') && (expr=expr') && (dly=false) && (dly'=true) && (dly''=false) && (dly'''=true) ->
    (match optim2 (ASGN(false, o1, expr :: tmp1 :: []) :: ASGN(true, o2, SEL (o2, tmp1 :: lst) :: dst :: []) :: tl) with
        | hd0::hd1::tl -> hd0::hd1::ASGN(true, o4, SEL (o4, tmp1 :: lst') :: dst' :: []) :: tl
	| _ -> failwith "optim2")
| hd :: tl -> hd :: optim2 tl

let rec optitm3 = function
| [] -> []
| BGN (Some lbl, tl) :: tl' -> BGN (Some lbl, optitm3 tl) :: optitm3 tl'
| BGN (None, tl) :: BGN (None, tl') :: tl'' -> optitm3 (BGN (None, tl @ tl') :: tl'')
| BGN (None, tl) :: tl' -> BGN (None, optitm3 tl) :: optitm3 tl'
| CS(o,rw_lst) :: tl -> CS(o,optitm3 rw_lst) :: optitm3 tl
| CAT(o,rw_lst) :: tl -> CAT(o,optitm3 rw_lst) :: optitm3 tl
| CSITM(o,rw_lst) :: tl -> CSITM(o,optitm3 rw_lst) :: optitm3 tl
| WHL(rw_lst) :: tl -> WHL(optitm3 rw_lst) :: optitm3 tl
| FORSTMT(o,kind,cmpop,ix,strt,stop,inc,rw_lst) :: tl -> FORSTMT(o,kind,cmpop,ix,strt,stop,inc,optitm3 rw_lst) :: optitm3 tl
| TASKDEF("", nam, rw_lst) :: tl -> TASKDEF("", nam, optitm3 rw_lst) :: optitm3 tl
| TASKRF("", nam, rw_lst) :: tl -> TASKRF("", nam, optitm3 rw_lst) :: optitm3 tl
| ASGN _ as oth :: tl -> oth :: optitm3 tl
| IF("", cnd :: then_stmt :: []) :: tl -> IF ("", cnd :: BGN(None, optitm3 [then_stmt]) :: []) :: optitm3 tl
| IF("", cnd :: then_stmt :: else_stmt :: []) :: tl -> IF ("", cnd :: BGN(None, optitm3 [then_stmt]) :: BGN(None, optitm3 [else_stmt]) :: []) :: optitm3 tl
| (CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _ | UNRY _) as oth :: tl -> oth :: optitm3 tl
| oth :: tl when !catch_escapes -> optothlst := oth :: !optothlst; failwith ("optothlst3: "^dumpitm oth)
| hd :: tl -> hd :: optitm3 tl

let lvalidate = function
| ('A'..'Z' | 'a'..'z' | '0'..'9') as ch -> ch
| _ -> '_'

let lcombine = function
| None, None -> None
| None, Some lbl -> Some (String.map lvalidate lbl)
| Some lbl, None -> Some (String.map lvalidate lbl)
| Some lbl, Some lbl' -> Some (String.map lvalidate (lbl^"_"^lbl'))

let rec optitm4 = function
| BGN (lbl, BGN (lbl', tl) :: []) -> optitm4 (BGN(lcombine(lbl, lbl'), tl)) 
| BGN (lbl, tl) -> BGN (lbl, List.map optitm4 tl)
| IF("", cnd :: then_else_stmt_lst) -> IF ("", cnd :: List.map optitm4 then_else_stmt_lst)
| CS ("", sel :: lst) -> CS ("", sel :: List.map optitm4 lst)
| CSITM("", rw_lst) -> CSITM("", List.map optitm4 rw_lst)
| WHL(rw_lst) -> WHL(List.map optitm4 rw_lst)
| FORSTMT(o,kind,cmpop,ix,strt,stop,inc,rw_lst) -> FORSTMT(o,kind,cmpop,ix,strt,stop,inc,List.map optitm4 rw_lst)
| TASKDEF("", nam, rw_lst) -> TASKDEF("", nam, List.map optitm4 rw_lst)
| TASKRF("", nam, rw_lst) -> TASKRF("", nam, List.map optitm4 rw_lst)
| (ASGN _  | CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _) as oth -> oth
| oth -> optothlst := oth :: !optothlst; failwith "optothlst4"

let optitm lst =
    let lst' = optitm3 lst in
    optopt := Some (lst, lst');
    let lst'' = List.map optitm4 lst' in
    lst''

let rec is_cnst itms id =
    if List.mem_assoc id !(itms.v) then
        begin
        let (_, ((a,b,c,d) as typ'), kind', n) = List.assoc id !(itms.v) in
        print_endline (dumptab typ');
        b = "const"
        end
    else
        false
    
let chktyp = function
| "logic" -> LOGIC
| "wire" -> WIRE
| oth -> LOGIC

let iolst modul delim dir io = function
| (IFCRFDTYP dir, kind, TYPNONE, []) as typ' -> !delim :: IDENT kind :: DOT :: IDENT dir :: SP :: IDENT io :: SP :: LCOMMENT :: IDENT (dumptab typ') :: RCOMMENT :: []
| (STRDTYP, _, TYPNONE, typlst) as typ' ->
    let (widlst,cnst,rng) = findmembers' typ' in
    !delim :: DIR dir :: SP :: LOGIC :: SP :: widshow io rng widlst @ comment widlst
| (typenc, kind, typmap, rng) as typ' ->
    let (widlst,cnst,rng) = findmembers' typ' in
    !delim :: DIR dir :: SP :: chktyp kind :: SP :: widshow io rng widlst @ comment widlst

let fndlm = function
| FIRSTG -> LPAREN::RPAREN::SEMI::[]
| IOSTG -> RPAREN::SEMI::[]
| VARSTG
| JMPSTG
| BDYSTG -> SEMI :: []
	     
let rec fnstmt modul dly stg = function
| IO ("", iolst', typ', dir, _, lst) ->
    let lst = List.flatten (List.map (fun io -> iolst modul (ref (if !stg = FIRSTG then LPAREN else COMMA)) dir io typ') iolst') in
    stg := IOSTG;
    lst
| VAR ("", idlst, typ', _) -> 
    let dlm = fndlm !stg in
    stg := VARSTG;
    dlm @ List.flatten (List.map (fun id -> varlst modul (ref NL) typ' id) idlst)
| JMPL("", rw_lst) ->
    let dlm = fndlm !stg in
    stg := JMPSTG;
    dlm @ BEGIN None :: (List.flatten (List.map (fnstmt modul dly stg) rw_lst)) @ [SEMI;END]
| JMPG (_,[]) -> []
| itm ->
    let dlm = fndlm !stg in
    stg := BDYSTG;
    dlm @ cstmt modul dly itm

let rec taskstmt modul dly nam = function
| BGN(_,rw_lst) -> List.flatten (List.map (taskstmt modul dly nam) rw_lst)
| itm -> cstmt modul dly itm @ SEMI :: []

let outnam ext f = f^ext^".v"
let outtok f = f^"_tokens.txt"
let othcomb = ref []

let needed modul (kind,nam) = match kind with
| FUNCTION ->
    print_endline ("Searching function: "^nam);
    let found = List.mem_assoc nam !(modul.func) in
    let (_, typ', lst, itms') = if found then
        List.assoc nam !(modul.func)
    else
        Hashtbl.find functable nam in
    let stg = ref FIRSTG in let lst = fsrc "" :: FUNCTION :: SP :: (varlst modul (ref NL) typ' nam) @
    List.flatten (List.map (fnstmt modul false stg) (List.tl lst)) in
    lst @ (fndlm !stg @ [ENDFUNCTION;NL])
| TASK ->
    print_endline ("Searching task: "^nam);
    let found = List.mem_assoc nam !(modul.task) in
    let (_, lst, itms') = if found then
        List.assoc nam !(modul.task)
    else
        Hashtbl.find tasktable nam in
    let lst = List.flatten (List.map (taskstmt modul false nam) lst) in
    fsrc "" :: TASK :: SP :: IDENT nam :: SEMI :: BEGIN None :: lst @ END :: ENDTASK :: []
| oth -> failwith "needed"

let dump intf f modul =
  let appendlst = ref [] in
  let append lst = appendlst := lst :: !appendlst in
  let head = ref [fsrc ""; if intf then INTERFACE else MODULE; SP; IDENT f] in
  let parmlst = List.filter (function (nam, (_, (w, HEX n))) -> true | _ -> false) (!(modul.cnst)) in
  if parmlst <> [] then
    begin
    let delim = ref (HASH :: LPAREN :: PARAMETER :: []) in
    List.iter (function (nam, (_, (w, HEX n))) ->
      head := !head @ !delim @ SP :: IDENT nam :: EQUALS :: num n :: [];
      delim := COMMA :: [];
      | oth -> ()) parmlst;
    head := !head @ [RPAREN];
    end;
  let delim = ref LPAREN in
  List.iter (fun (io, (_, typ', dir, kind', lst)) -> 
    head := !head @ iolst modul delim dir io typ';
    delim := COMMA;
    ) (!(modul.io));
  head := !head @ (if !delim <> COMMA then !delim :: [] else []) @ [RPAREN;SEMI];
  List.iter (function
	     | (nam, (_, (w, CNSTEXP (Aunknown, lst)))) -> let delim = ref (NL :: IDENT "typedef" :: SP :: IDENT "enum" :: SP :: SP :: LCURLY :: []) in
 head := !head @ NL :: List.flatten (List.map (function
| ENUMVAL(n,s) -> let field = !delim @ (IDENT s :: EQUALS :: num n :: []) in delim := COMMA :: SP :: []; field
| oth -> []) lst) @ [RCURLY; SP ; IDENT nam; SEMI; NL] | _ -> ()) (!(modul.cnst));
  List.iter (fun (id, (_, typ', kind', n)) -> append (fsrc "" :: varlst modul (ref NL) typ' id @ SEMI :: NL :: []);
                 ) (List.rev !(modul.v));
  List.iter (fun itm -> append (needed modul itm)) (List.rev !(modul.needed));
  List.iter (fun (a, (_, lst)) -> let delim = ref LPAREN in
    let lst = MODPORT :: SP :: IDENT a ::
    List.flatten (List.map (fun (nam',dir') -> let lst = !delim :: DIR dir' :: SP :: IDENT nam' :: [] in delim := COMMA; lst) lst) @
    [RPAREN; SEMI] in
    append ((fsrc "") :: lst)) !(modul.imp);  
  List.iter (fun (_, dst, src) ->
                 append (fsrc "" :: ASSIGN :: SP :: expr modul dst @ (SP :: ASSIGNMENT :: SP:: expr modul src @ SEMI :: NL :: []));
                 ) (List.rev !(modul.ca));
  List.iter (function
    | ("", COMB, (SNTRE []) :: lst) -> othcomb := lst;
      append (fsrc "" :: ALWAYS :: AT :: STAR :: flatten1 modul false lst);
    | ("", COMB, (SNTRE deplst) :: lst) ->
      append (fsrc "" :: ALWAYS :: AT :: let delim = ref [SP;LPAREN] in List.flatten (List.map (function
      | VRF (nam, _, _) -> let dep = !delim @ [IDENT nam] in delim := SP :: IDENT "or" :: SP :: []; dep
      | oth -> []) deplst) @ RPAREN :: SP :: flatten1 modul false lst);
    | ("", POSPOS (ck, rst), lst) ->
      append (fsrc "" :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: POSEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | ("", POSNEG (ck, rst), lst) ->
      append (fsrc "" :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | ("", NEGNEG (ck, rst), lst) ->
      append (fsrc "" :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | ("", POSEDGE (ck), lst) ->
      append (fsrc "" :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | ("", NEGEDGE (ck), lst) ->
      append (fsrc "" :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | (_, _, lst) -> failwith "edge specification not implemented";
    ) (List.rev !(modul.alwys));
  List.iter (fun (inst, (_, kind, lst)) ->
                 let delim = ref SP in
                 let lst = List.flatten (List.map (fun term -> let lst = !delim :: portconn modul term in delim := COMMA; lst) lst) in
                 append (fsrc "" :: IDENT kind :: SP :: IDENT inst :: LPAREN :: lst @ [RPAREN;SEMI]);
                 ) !(modul.inst);
  List.iter (fun (_, tok, lst) -> append (fsrc "" :: tok :: flatten1 modul false lst);
                 ) !(modul.init);
  !head @ List.flatten (List.sort compare !appendlst) @ [NL;if intf then ENDINTERFACE else ENDMODULE;NL;NL]

  let dump' ext (f, (_, tophash)) =
    print_endline ("dump' "^f);
    if true then print_endline ("f \""^f^"\";; /* "^outnam ext f^" */");
    let rawtok = dump false f tophash in
    let d = reformat0 rawtok in
    let lst = reformat2 (reformat1 d) in
    let indent = ref 0 in
    Hashtbl.add modtokens f (rawtok,d,lst);
    let fd = open_out (outnam ext f) in
    List.iter (tokenout fd indent) lst;
    close_out fd

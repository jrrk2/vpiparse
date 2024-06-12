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

open Input

let othscan = ref Work
let loc = LOC(0,0,0,0)
let wid = Width (0,0,false)

let rec rw_scan scanh = function
| NOT_FOUND (tok, LOC _) -> NOT_FOUND(tok, loc)
| LOC _ -> loc
| Width _ -> wid
| STRING_CONST _   -> STRING_CONST ""
| STRING _  -> STRING ""
| SLIST _  -> SLIST []
| ELIST _ -> ELIST []
| BIN _ -> BIN ""
| OCT _ -> OCT ""
| DEC _ -> DEC ""
| HEX _ -> HEX ""
| Int _ -> Int 0
| STRING _ -> STRING ""
| Vpidecompile _ -> Vpidecompile ""
| (Work   | Weaklyreferenced   | WAITING   | Var_select   | VBAR   | Unindent   | Uhdmtoppackages   | Uhdmtopmodules   | Uhdmallpackages   | Uhdmallmodules   | Uhdmallclasses   | UNDERSCORE   | UINT   | Try_put   | Try_peek   | Try_get   | Task_call   | Task   | TILDE   | System   | Sys_func_call   | Suspend   | String   | Status   | State   | Semaphore   | Self   | SUSPENDED   | SLASH   | Resume   | Restored   | Ref_var   | Ref_typespec   | Ref_obj   | Ref_module   | Range   | RUNNING   | RPAREN   | RBRACK   | RBRACE   | Queue   | QUOTE   | QUERY   | Put   | Process   | Pre_Elab   | Post_Elab   | Port   | Peek   | Part_select   | Parameter   | Param_assign   | Package   | PLING   | PERCENT   | Operation   | Num   | New   | Named_begin   | Module_inst   | Message   | Mailbox   | Logic_var   | Logic_typespec   | Logic_net   | Line   | LPAREN   | LINEFEED   | LESS   | LBRACK   | LBRACE   | Kill   | Keycount   | KILLED   | Io_decl   | Integer_var   | Integer_typespec   | Int_var   | Int_typespec   | Initial   | Indexed_part_select   | Indent   | If_stmt   | If_else   | INT   | HYPHEN   | HASH   | Get   | Gen_scope_array   | Gen_scope   | Gen_region   | Gen_if_else   | Gen_case   | GREATER   | Function   | For_stmt   | File   | FINISHED   | Event_control   | Enum_var   | Enum_typespec   | Enum_const   | Endln   | ERROR_TOKEN   | ERROR   | EOF_TOKEN   | END   | EMPTY_TOKEN   | Design   | DOUBLEQUOTE   | DOT   | DOLLAR   | DESIGN   | DEFAULT   | Cont_assign   | Constant   | Clk   | Class_var   | Class_typespec   | Class_defn   | Case_stmt   | Case_item   | COMMA   | COLON   | CARET   | Builtin   | Bound   | Bit_select   | Begin   | BACKSLASH   | BACKQUOTE   | Await   | Attribute   | Assignment   | Array_var   | Array_typespec   | Array_net   | Array   | Any_sverilog_class   | Always   | AT   | AMPERSAND   | ACCEPT) as x -> x
| (Assignment | Begin | Constant | UINT | INT | Port | Part_select | Bit_select | Logic_net | Logic_typespec | Logic_var | Ref_module | Ref_obj | Ref_typespec | Ref_var | Vpiactive | Vpiactivetimeformat | Vpiactual | Vpiaddop | Vpialways | Vpialwaystype | Vpiandprim | Vpiargument | Vpiarithlshiftop | Vpiarithrshiftop | Vpiarray | Vpiarraynet | Vpiarraytype | Vpiassignment | Vpiassignstmt | Vpiattribute | Vpiautomatic | Vpiautomatics | Vpibaseexpr | Vpibasetypespec | Vpibegin | Vpibinaryconst | Vpibit | Vpibitandop | Vpibitnegop | Vpibitorop | Vpibitselect | Vpibitxnorop | Vpibitxorop | Vpiblocking | Vpibufif0prim | Vpibufif1prim | Vpibufprim | Vpicallback | Vpicase | Vpicaseeqop | Vpicaseexact | Vpicaseitem | Vpicaseneqop | Vpicasetype | Vpicasex | Vpicasez | Vpicell | Vpicellinstance | Vpichargestrength | Vpiclassdefn | Vpicmosprim | Vpicombprim | Vpiconcatop | Vpicondition | Vpiconditionop | Vpiconfig | Vpiconnbyname | Vpiconstant | Vpiconstantselect | Vpiconsttype | Vpicontassign | Vpicontassignbit | Vpidatapolarity | Vpideassign | Vpidecconst | Vpidefattribute | Vpidefdecaytime | Vpidefdelaymode | Vpideffile | Vpideflineno | Vpidefname | Vpidefnettype | Vpidefparam | Vpidelay | Vpidelaycontrol | Vpidelaydevice | Vpidelaymodedistrib | Vpidelaymodemtm | Vpidelaymodenone | Vpidelaymodepath | Vpidelaymodeunit | Vpidelaymodezero | Vpidelayterm | Vpidelaytype | Vpidirection | Vpidisable | Vpidivop | Vpidriver | Vpiedge | Vpielaborated | Vpielemtypespec | Vpielsestmt | Vpienumconst | Vpieqop | Vpieventcontrol | Vpieventorop | Vpieventstmt | Vpiexpanded | Vpiexplicitname | Vpiexplicitscalared | Vpiexplicitvectored | Vpiexpr | Vpifile | Vpifinish | Vpifor | Vpiforce | Vpiforever | Vpiforincstmt | Vpiforinitstmt | Vpifork | Vpiframe | Vpifullname | Vpifullskew | Vpifunccall | Vpifunction | Vpifunctype | Vpigate | Vpigatearray | Vpigenscope | Vpigenscopearray | Vpigenstmt | Vpigenvar | Vpigeop | Vpigtop | Vpihexconst | Vpihighconn | Vpihighz | Vpihold | Vpiif | Vpiifelse | Vpiimplicitdecl | Vpiindex | Vpiindexedpartselect | Vpiindexedpartselecttype | Vpiinitial | Vpiinout | Vpiinput | Vpiinstance | Vpiinstancearray | Vpiintconst | Vpiintegervar | Vpiinterm | Vpiintermodpath | Vpiintermodpathdelay | Vpiinternalscope | Vpiintfunc | Vpiiodecl | Vpiismemory | Vpiisprotected | Vpiiterator | Vpiiteratortype | Vpileftrange | Vpileop | Vpilhs | Vpilibrary | Vpilineno | Vpilistop | Vpiload | Vpilocaldriver | Vpilocalload | Vpilocalparam | Vpilogandop | Vpilogorop | Vpilowconn | Vpilshiftop | Vpiltop | Vpimemory | Vpimemoryword | Vpimethod | Vpimintypmaxop | Vpiminusop | Vpimipdelay | Vpimixedio | Vpimoddatapathin | Vpimodop | Vpimodpath | Vpimodpathdelay | Vpimodpathhasifnone | Vpimodpathin | Vpimodpathout | Vpimodule | Vpimodulearray | Vpimulticoncatop | Vpimultop | Vpiname | Vpinamedbegin | Vpinamedevent | Vpinamedeventarray | Vpinamedfork | Vpinandprim | Vpinegative | Vpinegedgeop | Vpinegindexed | Vpineqop | Vpinet | Vpinetarray | Vpinetbit | Vpinetdeclassign | Vpinettype | Vpinmosprim | Vpinochange | Vpinodirection | Vpinone | Vpinorprim | Vpinotif0prim | Vpinotif1prim | Vpinotop | Vpinotprim | Vpinullop | Vpinullstmt | Vpioctconst | Vpioffset | Vpioperand | Vpioperation | Vpioptype | Vpiorprim | Vpioutput | Vpioutterm | Vpioverriden | Vpiparamassign | Vpiparameter | Vpiparent | Vpipartselect | Vpipathfull | Vpipathparallel | Vpipathterm | Vpiperiod | Vpiplusop | Vpipmosprim | Vpipolarity | Vpiport | Vpiportbit | Vpiportindex | Vpiportinst | Vpiports | Vpiposedge | Vpiposedgeop | Vpiposindexed | Vpipositive | Vpipowerop | Vpiprimitive | Vpiprimitivearray | Vpiprimterm | Vpiprimtype | Vpiprocess | Vpiprotected | Vpipull0 | Vpipull1 | Vpipulldownprim | Vpipullupprim | Vpirange | Vpircmosprim | Vpirealconst | Vpirealfunc | Vpirealvar | Vpirecovery | Vpirecrem | Vpirefmodule | Vpireg | Vpiregarray | Vpiregbit | Vpirelease | Vpiremoval | Vpirepeat | Vpirepeatcontrol | Vpireset | Vpiresolvednettype | Vpireturn | Vpirhs | Vpirightrange | Vpirnmosprim | Vpirpmosprim | Vpirshiftop | Vpirtranif0prim | Vpirtranif1prim | Vpirtranprim | Vpisaverestartid | Vpisaverestartlocation | Vpiscalar | Vpischedevent | Vpischeduled | Vpiscope | Vpiseqprim | Vpisetinteractivescope | Vpisetup | Vpisetuphold | Vpisigned | Vpisimnet | Vpisize | Vpisizedfunc | Vpisizedsignedfunc | Vpiskew | Vpispecparam | Vpistmt | Vpistop | Vpistrength0 | Vpistrength1 | Vpistringconst | Vpisubop | Vpisupply0 | Vpisupply1 | Vpiswitch | Vpiswitcharray | Vpisysfunccall | Vpisystaskcall | Vpisystfcall | Vpitableentry | Vpitask | Vpitaskcall | Vpitaskfunc | Vpitchk | Vpitchkdataterm | Vpitchknotifier | Vpitchkrefterm | Vpitchkterm | Vpitchktype | Vpitermindex | Vpitimeconst | Vpitimefunc | Vpitimeprecision | Vpitimequeue | Vpitimeskew | Vpitimeunit | Vpitimevar | Vpitop | Vpitopmodule | Vpitranif0prim | Vpitranif1prim | Vpitranprim | Vpitri | Vpitri0 | Vpitri1 | Vpitriand | Vpitrior | Vpitrireg | Vpitype | Vpitypedef | Vpitypespec | Vpiudp | Vpiudparray | Vpiudpdefn | Vpiuintconst | Vpiunaryandop | Vpiunarynandop | Vpiunarynorop | Vpiunaryorop | Vpiunaryxnorop | Vpiunaryxorop | Vpiunconndrive | Vpiundefined | Vpiunknown | Vpiuse | Vpiuserdefn | Vpiusersystf | Vpiuwire | Vpivalid | Vpivariables | Vpivarselect | Vpivector | Vpivisibility | Vpiwait | Vpiwand | Vpiwhile | Vpiwidth | Vpiwidthexpr | Vpiwire | Vpiwor | Vpixnorprim | Vpixorprim as x) -> x 
| TLIST (STRING _ :: _ as lst) -> let pat = TLIST [Work] in Hashtbl.replace scanh pat (); pat
| TLIST slst ->
  let _ = TLIST (List.map (rw_scan scanh) slst) in
  let pat = TLIST [Work] in
  Hashtbl.replace scanh pat (); pat
| TUPLE2((Vpilhs|Vpirhs|Vpicondition _ as op), b) ->
  let _ = rw_scan scanh b in
  let pat = TUPLE2(op, Work) in Hashtbl.replace scanh pat ();
  pat
| TUPLE2 ((Vpileftrange|Vpirightrange|Vpihighconn|Vpilowconn|Vpiposedgeop|Vpinegedgeop|Vpiunaryandop|Vpiunarynandop|Vpiunaryorop|Vpiunarynorop|Vpiunaryxorop|Vpiunaryxnorop|Vpibitnegop|Vpiplusop|Vpiminusop|Vpinotop as op), b) ->
  let _ = TUPLE2(op, rw_scan scanh b) in
  let pat = TUPLE2(op, Work) in
  Hashtbl.replace scanh pat (); pat
| TUPLE2(a,b) -> let pat = TUPLE2((rw_scan scanh) a, (rw_scan scanh) b) in Hashtbl.replace scanh pat (); pat
| TUPLE3((Vpieventorop|Vpiaddop|Vpisubop|Vpimultop|Vpidivop|Vpimodop|Vpipowerop|Vpilshiftop|Vpiarithlshiftop|Vpirshiftop|Vpiarithrshiftop|Vpilogandop|Vpilogorop|Vpibitandop|Vpibitorop|Vpibitxorop|Vpibitxnorop|Vpieqop|Vpineqop|Vpiltop|Vpileop|Vpigeop|Vpigtop as op),b,c) ->
  let _ = TUPLE3(op, (rw_scan scanh) b, (rw_scan scanh) c) in
  let pat = TUPLE3(op,Work,Work) in
  Hashtbl.replace scanh pat (); pat
| TUPLE3(a,b,c) -> let pat = TUPLE3((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c) in Hashtbl.replace scanh pat (); pat
| TUPLE4((Vpiconditionop|Assignment|If_else as op),b,c,d) ->
  let _ = TUPLE4(op, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d) in
  let pat = TUPLE4(op,Work,Work,Work) in
  Hashtbl.replace scanh pat (); pat
| TUPLE4(a,b,c,d) -> let pat = TUPLE4((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d) in Hashtbl.replace scanh pat (); pat
| TUPLE5(a,b,c,d,e) -> let pat = TUPLE5((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e) in Hashtbl.replace scanh pat (); pat
| TUPLE6(a,b,c,d,e,f) -> let pat = TUPLE6((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e, (rw_scan scanh) f) in Hashtbl.replace scanh pat (); pat
| TUPLE7(a,b,c,d,e,f,g) -> let pat = TUPLE7((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e, (rw_scan scanh) f, (rw_scan scanh) g) in Hashtbl.replace scanh pat (); pat
| TUPLE8(a,b,c,d,e,f,g,h) -> let pat = TUPLE8((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e, (rw_scan scanh) f, (rw_scan scanh) g, (rw_scan scanh) h) in Hashtbl.replace scanh pat (); pat
| TUPLE9(a,b,c,d,e,f,g,h,i) -> let pat = TUPLE9((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e, (rw_scan scanh) f, (rw_scan scanh) g, (rw_scan scanh) h, (rw_scan scanh) i) in Hashtbl.replace scanh pat (); pat
| TUPLE10(a,b,c,d,e,f,g,h,i,j) -> let pat = TUPLE10((rw_scan scanh) a, (rw_scan scanh) b, (rw_scan scanh) c, (rw_scan scanh) d, (rw_scan scanh) e, (rw_scan scanh) f, (rw_scan scanh) g, (rw_scan scanh) h, (rw_scan scanh) i, (rw_scan scanh) j) in Hashtbl.replace scanh pat (); pat
| oth -> othscan := oth; failwith "rw_scan"

let rw_scan_lst lst =
  let scanh = Hashtbl.create 65535 in
  let scanlst = ref [] in
  let _ = List.map (rw_scan scanh) lst in 
  Hashtbl.iter (fun k _ -> if not (List.mem k !scanlst) then scanlst := k :: !scanlst) scanh;
  List.sort_uniq compare !scanlst
  

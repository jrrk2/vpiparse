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
open Input_types

let othpp = ref Work

let locache = ref (Hashtbl.create 1)

let rec event_collapse = function
| TUPLE3 (Vpieventorop, ev, ev') -> ev' :: event_collapse ev
| oth -> [oth]

let rec concat op = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> Concat(op, hd, concat op tl)

let rec rw' = function
| LOC _ as loc -> loc
| NOT_FOUND (kw, loc) -> (try Hashtbl.find !locache (kw, loc) with _ -> TLIST [])
| (Work   | Width _ | Weaklyreferenced   | WAITING   | Var_select   | VBAR   | Unindent   | Uhdmtoppackages   | Uhdmtopmodules   | Uhdmallpackages   | Uhdmallmodules   | Uhdmallclasses   | UNDERSCORE   | UINT   | Try_put   | Try_peek   | Try_get   | Task_call   | Task   | TILDE   | System   | Sys_func_call   | Suspend   | String   | Status   | State   | Semaphore   | Self   | SUSPENDED   | STRING_CONST _   | STRING _  | SLIST _  | SLASH   | Resume   | Restored   | Ref_var   | Ref_typespec   | Ref_obj   | Ref_module   | Range   | RUNNING   | RPAREN   | RBRACK   | RBRACE   | Queue   | QUOTE   | QUERY   | Put   | Process   | Pre_Elab   | Post_Elab   | Port   | Peek   | Part_select   | Parameter   | Param_assign   | Package   | PLING   | PERCENT   | Operation   | Num   | New   | Named_begin   | Module_inst   | Message   | Mailbox   | Logic_var   | Logic_typespec   | Logic_net   | Line   | LPAREN   | LINEFEED   | LESS   | LBRACK   | LBRACE   | Kill   | Keycount   | KILLED   | Io_decl   | Integer_var   | Integer_typespec   | Int_var   | Int_typespec   | Initial   | Indexed_part_select   | Indent   | If_stmt   | If_else   | INT   | HYPHEN   | HASH   | Get   | Gen_scope_array   | Gen_scope   | Gen_region   | Gen_if_else   | Gen_case   | GREATER   | Function   | For_stmt   | File   | FINISHED   | Event_control   | Enum_var   | Enum_typespec   | Enum_const   | Endln   | ERROR_TOKEN   | ERROR   | EOF_TOKEN   | END   | EMPTY_TOKEN   | ELIST _ | Design   | DOUBLEQUOTE   | DOT   | DOLLAR   | DESIGN   | DEFAULT   | Cont_assign   | Constant   | Clk   | Class_var   | Class_typespec   | Class_defn   | Case_stmt   | Case_item   | COMMA   | COLON   | CARET   | Builtin   | Bound   | Bit_select   | Begin   | BACKSLASH   | BACKQUOTE   | Await   | Attribute   | Assignment   | Array_var   | Array_typespec   | Array_net   | Array   | Any_sverilog_class   | Always   | AT   | AMPERSAND   | ACCEPT) as x -> x
| (Assignment | Begin | Constant | BIN _ | OCT _ | DEC _ | HEXS _ | Int _ | STRING _ | UINT | INT | Port | Part_select | Bit_select | Logic_net | Logic_typespec | Logic_var | Ref_module | Ref_obj | Ref_typespec | Ref_var | Vpiactive | Vpiactivetimeformat | Vpiactual | Vpiaddop | Vpialways | Vpialwaystype | Vpiandprim | Vpiargument | Vpiarithlshiftop | Vpiarithrshiftop | Vpiarray | Vpiarraynet | Vpiarraytype | Vpiassignment | Vpiassignstmt | Vpiattribute | Vpiautomatic | Vpiautomatics | Vpibaseexpr | Vpibasetypespec | Vpibegin | Vpibinaryconst | Vpibit | Vpibitandop | Vpibitnegop | Vpibitorop | Vpibitselect | Vpibitxnorop | Vpibitxorop | Vpiblocking | Vpibufif0prim | Vpibufif1prim | Vpibufprim | Vpicallback | Vpicase | Vpicaseeqop | Vpicaseexact | Vpicaseitem | Vpicaseneqop | Vpicasetype | Vpicasex | Vpicasez | Vpicell | Vpicellinstance | Vpichargestrength | Vpiclassdefn | Vpicmosprim | Vpicombprim | Vpiconcatop | Vpicondition | Vpiconditionop | Vpiconfig | Vpiconnbyname | Vpiconstant | Vpiconstantselect | Vpiconsttype | Vpicontassign | Vpicontassignbit | Vpidatapolarity | Vpideassign | Vpidecconst | Vpidecompile _ | Vpidefattribute | Vpidefdecaytime | Vpidefdelaymode | Vpideffile | Vpideflineno | Vpidefname | Vpidefnettype | Vpidefparam | Vpidelay | Vpidelaycontrol | Vpidelaydevice | Vpidelaymodedistrib | Vpidelaymodemtm | Vpidelaymodenone | Vpidelaymodepath | Vpidelaymodeunit | Vpidelaymodezero | Vpidelayterm | Vpidelaytype | Vpidirection | Vpidisable | Vpidivop | Vpidriver | Vpiedge | Vpielaborated | Vpielemtypespec | Vpielsestmt | Vpienumconst | Vpieqop | Vpieventcontrol | Vpieventorop | Vpieventstmt | Vpiexpanded | Vpiexplicitname | Vpiexplicitscalared | Vpiexplicitvectored | Vpiexpr | Vpifile | Vpifinish | Vpifor | Vpiforce | Vpiforever | Vpiforincstmt | Vpiforinitstmt | Vpifork | Vpiframe | Vpifullname | Vpifullskew | Vpifunccall | Vpifunction | Vpifunctype | Vpigate | Vpigatearray | Vpigenscope | Vpigenscopearray | Vpigenstmt | Vpigenvar | Vpigeop | Vpigtop | Vpihexconst | Vpihighconn | Vpihighz | Vpihold | Vpiif | Vpiifelse | Vpiimplicitdecl | Vpiindex | Vpiindexedpartselect | Vpiindexedpartselecttype | Vpiinitial | Vpiinout | Vpiinput | Vpiinstance | Vpiinstancearray | Vpiintconst | Vpiintegervar | Vpiinterm | Vpiintermodpath | Vpiintermodpathdelay | Vpiinternalscope | Vpiintfunc | Vpiiodecl | Vpiismemory | Vpiisprotected | Vpiiterator | Vpiiteratortype | Vpileftrange | Vpileop | Vpilhs | Vpilibrary | Vpilineno | Vpilistop | Vpiload | Vpilocaldriver | Vpilocalload | Vpilocalparam | Vpilogandop | Vpilogorop | Vpilowconn | Vpilshiftop | Vpiltop | Vpimemory | Vpimemoryword | Vpimethod | Vpimintypmaxop | Vpiminusop | Vpimipdelay | Vpimixedio | Vpimoddatapathin | Vpimodop | Vpimodpath | Vpimodpathdelay | Vpimodpathhasifnone | Vpimodpathin | Vpimodpathout | Vpimodule | Vpimodulearray | Vpimulticoncatop | Vpimultop | Vpiname | Vpinamedbegin | Vpinamedevent | Vpinamedeventarray | Vpinamedfork | Vpinandprim | Vpinegative | Vpinegedgeop | Vpinegindexed | Vpineqop | Vpinet | Vpinetarray | Vpinetbit | Vpinetdeclassign | Vpinettype | Vpinmosprim | Vpinochange | Vpinodirection | Vpinone | Vpinorprim | Vpinotif0prim | Vpinotif1prim | Vpinotop | Vpinotprim | Vpinullop | Vpinullstmt | Vpioctconst | Vpioffset | Vpioperand | Vpioperation | Vpioptype | Vpiorprim | Vpioutput | Vpioutterm | Vpioverriden | Vpiparamassign | Vpiparameter | Vpiparent | Vpipartselect | Vpipathfull | Vpipathparallel | Vpipathterm | Vpiperiod | Vpiplusop | Vpipmosprim | Vpipolarity | Vpiport | Vpiportbit | Vpiportindex | Vpiportinst | Vpiports | Vpiposedge | Vpiposedgeop | Vpiposindexed | Vpipositive | Vpipowerop | Vpiprimitive | Vpiprimitivearray | Vpiprimterm | Vpiprimtype | Vpiprocess | Vpiprotected | Vpipull0 | Vpipull1 | Vpipulldownprim | Vpipullupprim | Vpirange | Vpircmosprim | Vpirealconst | Vpirealfunc | Vpirealvar | Vpirecovery | Vpirecrem | Vpirefmodule | Vpireg | Vpiregarray | Vpiregbit | Vpirelease | Vpiremoval | Vpirepeat | Vpirepeatcontrol | Vpireset | Vpiresolvednettype | Vpireturn | Vpirhs | Vpirightrange | Vpirnmosprim | Vpirpmosprim | Vpirshiftop | Vpirtranif0prim | Vpirtranif1prim | Vpirtranprim | Vpisaverestartid | Vpisaverestartlocation | Vpiscalar | Vpischedevent | Vpischeduled | Vpiscope | Vpiseqprim | Vpisetinteractivescope | Vpisetup | Vpisetuphold | Vpisigned | Vpisimnet | Vpisize | Vpisizedfunc | Vpisizedsignedfunc | Vpiskew | Vpispecparam | Vpistmt | Vpistop | Vpistrength0 | Vpistrength1 | Vpistringconst | Vpisubop | Vpisupply0 | Vpisupply1 | Vpiswitch | Vpiswitcharray | Vpisysfunccall | Vpisystaskcall | Vpisystfcall | Vpitableentry | Vpitask | Vpitaskcall | Vpitaskfunc | Vpitchk | Vpitchkdataterm | Vpitchknotifier | Vpitchkrefterm | Vpitchkterm | Vpitchktype | Vpitermindex | Vpitimeconst | Vpitimefunc | Vpitimeprecision | Vpitimequeue | Vpitimeskew | Vpitimeunit | Vpitimevar | Vpitop | Vpitopmodule | Vpitranif0prim | Vpitranif1prim | Vpitranprim | Vpitri | Vpitri0 | Vpitri1 | Vpitriand | Vpitrior | Vpitrireg | Vpitype | Vpitypedef | Vpitypespec | Vpiudp | Vpiudparray | Vpiudpdefn | Vpiuintconst | Vpiunaryandop | Vpiunarynandop | Vpiunarynorop | Vpiunaryorop | Vpiunaryxnorop | Vpiunaryxorop | Vpiunconndrive | Vpiundefined | Vpiunknown | Vpiuse | Vpiuserdefn | Vpiusersystf | Vpiuwire | Vpivalid | Vpivariables | Vpivarselect | Vpivector | Vpivisibility | Vpiwait | Vpiwand | Vpiwhile | Vpiwidth | Vpiwidthexpr | Vpiwire | Vpiwor | Vpixnorprim | Vpixorprim as x) -> x 
| TLIST slst -> TLIST (List.map rw' slst)
| TUPLE2(a,b) -> TUPLE2(rw' a, rw' b)
| TUPLE3 (Vpieventorop, TUPLE3 (Vpieventorop, _, _), _) as ev -> TUPLE2 (Vpieventorop, TLIST (List.map rw' (event_collapse ev)))
| TUPLE3(a,b,c) -> TUPLE3(rw' a, rw' b, rw' c)
| TUPLE4(a,b,c,d) -> TUPLE4(rw' a, rw' b, rw' c, rw' d)
| TUPLE5(a,b,c,d,e) -> TUPLE5(rw' a, rw' b, rw' c, rw' d, rw' e)
| TUPLE6(a,b,c,d,e,f) -> TUPLE6(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f)
| TUPLE7(a,b,c,d,e,f,g) -> TUPLE7(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g)
| TUPLE8(a,b,c,d,e,f,g,h) -> TUPLE8(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h)
| TUPLE9(a,b,c,d,e,f,g,h,i) -> TUPLE9(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h, rw' i)
| TUPLE10(a,b,c,d,e,f,g,h,i,j) -> TUPLE10(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h, rw' i, rw' j)
| oth -> othpp := oth; failwith "rw'"

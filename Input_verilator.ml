(*
BSD 2-Clause License

Copyright (c) 2018, jrrk
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Input
open Input_dump
open Input_types
open Dump_types

let files = Hashtbl.create 255
let functable = Hashtbl.create 255
let tasktable = Hashtbl.create 255

let itmothlst = ref []
 let posneglst = ref []
let subothlst = ref []
let ntlopt = ref None
let rngopt = ref None
let portopt = ref None
let instopt = ref None
let itmopt = ref None

let matchcnt = ref 0

let constnet = function
| "1'h1" -> "supply1"
| "1'h0" -> "supply0"
| oth -> "wire"

let errlst = ref []

let rec rw' attr = function
| Xml.Element ("verilator_xml", [], xlst) ->
    let decl,hier = List.partition (function Xml.Element (("files"|"module_files"|"netlist"), _, _) -> true | _ -> false) xlst in
    XML (List.map (rw' attr) (decl@hier))
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' attr) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", lang)], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) ->
    let rlst = List.rev xlst in
    let tlst = List.tl rlst in
    let typtable = match rw' attr (List.hd rlst) with TYPETABLE typarr -> typarr | _ -> failwith "netlist" in
    let uniq = ref [] in
    attr.instances := List.sort compare (List.map (function
        | Xml.Element (kw, ("loc", _) :: ("name", kind) :: ("origName", orignam) :: _, _) ->
            let uniq' = uniqnam 0 orignam uniq in uniq := (kind,uniq') :: !uniq;
            (kind, ((match kw with
            | "module" -> MODULE
            | "iface" -> INTERFACE
            | "package" -> PACKAGE
            | oth -> failwith ("unexpected instance: "^oth)),uniq'))
        | oth -> instopt := Some oth; failwith "instopt") tlst);
(*
    let fd = open_out "instances.txt" in
    let indent = ref 0 in List.iter (fun (k, (kind,n)) -> output_string fd (k^": "^tokencnv indent kind^" "^n^"\n")) !(attr.instances);
    close_out fd;
*)
    let attr' = {attr with typetable=typtable} in
    let tlst' = List.map (rw' attr') tlst in
    let opt1 = optitm' false tlst' in
    let opt2 = optitm' true opt1 in
    ntlopt := Some opt2;
    NTL opt2
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
    let typ' = attr.typetable.(int_of_string tid) in
    attr.names := (nam, ref typ') :: !(attr.names);
    IO ("", [nam], typ', dirop dir, typ, List.map (rw' attr) xlst)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("dir", dir); ("pinIndex", pi); ("vartype", typ); ("origName", nam')], xlst) ->
    let typ' = attr.typetable.(int_of_string tid) in
    attr.names := (nam, ref typ') :: !(attr.names);
    IO ("", [nam], typ', dirop dir, typ, List.map (rw' attr) xlst)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", ("ifaceref" as typ)); ("origName", nam')], []) ->
    let (vif, sub) = chkvif nam in
    let nam' = if vif then sub else nam in
    let rslt = match attr.typetable.(int_of_string tid) with
               | (UNPACKADTYP, _, RECTYP attr', [TYPRNG ((HEX hi|SHEX hi), (HEX lo|SHEX lo))]) ->
                   (match attr' with
                       | (IFCRFDTYP _, dir, TYPNONE, []) as typ' ->
                           attr.names := expandbraket lo hi (fun istr ->
                               let nam' = nam'^istr in print_endline ("@"^nam'); (nam', ref typ')) @ !(attr.names);
                           let exp' = expandbraket lo hi (fun istr -> nam'^istr) in
                           if vif then VAR ("", exp', attr', typ) else IO ("", exp', attr', Dinam dir, "logic", [])
                       | oth -> typopt := Some oth; failwith ("typopt;;582: "^dumptab oth))
               | (IFCRFDTYP _, dir, TYPNONE, []) as typ' ->
                   print_endline ("@"^nam');
                   if not (List.mem_assoc nam' !(attr.names)) then
                       attr.names := (nam', ref typ') :: !(attr.names)
                   else
                       print_endline (dumptab typ'^":"^dumptab !(List.assoc nam' !(attr.names)));
                   if vif then VAR ("", [sub], typ', typ) else IO ("", [nam], typ', Dinam dir, "logic", [])
               | oth -> typopt := Some oth; failwith ("typopt;;587: "^dumptab oth) in rslt
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               let pat = "__Vconcswap" in
               let l = String.length nam and l' = String.length pat in
               let anchor = if l > l' && String.sub nam 0 l' = pat then attr.anchor else "" in
               VAR (anchor, [nam], attr.typetable.(int_of_string tid), typ)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam'); (("param"|"localparam"), "true")], const) ->
               let pat = "__Vconcswap" in
               let l = String.length nam and l' = String.length pat in
               let anchor = if l > l' && String.sub nam 0 l' = pat then attr.anchor else "" in
               VAR (anchor, [nam], attr.typetable.(int_of_string tid), typ)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("loc", _); ("name", _); ("dtype_id", cid)], []) as lev]) ->
                             IVAR ("", nam, attr.typetable.(int_of_string tid), [rw' attr lev], int_of_string cid)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam'); ("param", param')],
               [Xml.Element ("const", [("loc", _); ("name", _); ("dtype_id", cid)], []) as lev]) ->
                             IVAR ("", nam, attr.typetable.(int_of_string tid), [rw' attr lev], int_of_string cid)
| Xml.Element ("var", [("loc", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],

	       [Xml.Element ("initarray", [("loc", _); ("dtype_id", cid)], initlst)]) ->
                             IVAR ("", nam, attr.typetable.(int_of_string tid), List.map (rw' attr) initlst, int_of_string cid)
| Xml.Element ("const", [("loc", _); ("name", value); ("dtype_id", tid)], []) -> CNST (cexp value)
| Xml.Element ("contassign", [("loc", _); ("dtype_id", tid)], xlst) -> CA ("", List.map (rw' attr) xlst)
| Xml.Element ("not"|"negate"|"lognot" as op, [("loc", _); ("dtype_id", tid)], xlst) ->
	       UNRY (unaryop op, List.map (rw' attr) xlst)
| Xml.Element ("extend"|"extends" as op, [("loc", _); ("dtype_id", tid); ("width", w); ("widthminv", wm)], xlst) ->
	       UNRY (extendop attr.anchor (int_of_string w) (int_of_string wm) op, List.map (rw' attr) xlst)
| Xml.Element ("varref", [("loc", _); ("name", nam); ("dtype_id", tid)], xlst) ->
               VRF (snd (chkvif nam), attr.typetable.(int_of_string tid), List.map (rw' attr) xlst)
| Xml.Element ("instance", [("loc", _); ("name", nam); ("defName", mangled); ("origName", _)], xlst) ->
               let instkind,dnam = if List.mem_assoc mangled !(attr.instances) then
                   List.assoc mangled !(attr.instances)
               else
                   (print_endline (mangled^": missing"); (MODULE,mangled)) in
	       let rnglst, attrlst = List.partition (function RNG _ -> true | _ -> false) (List.map (rw' attr) xlst) in
               let exportlst = ref [] in
               List.iter (function
                   | PORT (orig, nam, Dvif _, [VRF (nam', typ', [])]) as port when List.mem_assoc nam' !(attr.names) -> 
                       (match !(List.assoc nam' !(attr.names)) with
                           | (IFCRFDTYP _, dir, TYPRNG(HEX hi, HEX lo), []) ->
                               exportlst := expandbraket lo hi (fun istr -> PORT (orig, nam^istr, Dvif (ref dir), [VRF (nam'^istr, typ', [])])) @ !exportlst
                           | (IFCRFDTYP _, dir, TYPNONE, []) -> exportlst := port :: !exportlst
                           | oth -> typopt := Some oth; failwith ("typopt;;599: "^dumptab oth))
                   | PORT (orig, nam, Dvif _, [ASEL (VRF (nam', typ', []) :: CNST(_,HEX idx) :: [])]) when List.mem_assoc nam' !(attr.names) -> 
                       (match !(List.assoc nam' !(attr.names)) with
                           | (IFCRFDTYP _, dir, TYPRNG(hi,lo), []) ->
                               exportlst := expandbraket idx idx (fun istr -> PORT (orig, nam, Dvif (ref dir), [VRF (nam'^istr, typ', [])])) @ !exportlst
                           | (IFCRFDTYP _, dir, TYPNONE, []) -> failwith ("indexing a scalar interface: "^nam)
                           | oth -> typopt := Some oth; failwith ("typopt;;599: "^dumptab oth))
                   | PORT (orig, nam, Dvif _, [VRF (nam', _, [])]) ->
                       print_endline ("vif interface "^nam'^" not found: ["^String.concat ";" (List.map (fun (k,_) -> k) !(attr.names))^"] ?")
                   | PORT (orig, nam, dir, connlst) as port -> exportlst := port :: !exportlst
                   | oth -> portopt := Some oth; failwith "portopt") attrlst;
               let attrlst = List.rev !exportlst in
               let inst = match (instkind,rnglst) with
                   | (INTERFACE, RNG (CNST (_, (HEX hi|SHEX hi)) :: CNST (_, (HEX lo|SHEX lo)) :: []) :: []) ->
                       begin
                       print_endline ("@"^nam^"["^string_of_int hi^":"^string_of_int lo^"]");
                       attr.names := (nam, ref (IFCRFDTYP nam, dnam, TYPRNG(HEX hi, HEX lo), [])) :: !(attr.names);
                       INST ("", instkind, expandbraket lo hi (fun istr -> nam^istr), (dnam, attrlst))
                       end
                   | (INTERFACE, []) -> attr.names := (nam, ref (IFCRFDTYP nam, dnam, TYPNONE, [])) :: !(attr.names);
		           INST ("", instkind, nam :: [], (dnam, attrlst))
                   | (MODULE, []) -> INST ("", instkind, nam :: [], (dnam, attrlst))
                   | oth -> rngopt := Some oth; failwith "rngopt" in
               inst
| Xml.Element ("range", [("loc", _)], xlst) -> RNG (List.map (rw' attr) xlst)
| Xml.Element ("port", [("loc", _); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT ("", nam, dirop dir, List.map (rw' attr) xlst)
| Xml.Element ("port", [("loc", _); ("name", nam); ("portIndex", idx)], xlst) -> let (vif,sub) = chkvif nam in
               PORT ("", sub, Dvif (ref sub), List.map (rw' attr) xlst)
| Xml.Element ("sel", [("loc", _); ("dtype_id", tid)], xlst) -> SEL ("", List.map (rw' attr) xlst)
| Xml.Element ("arraysel", [("loc", _); ("dtype_id", tid)], xlst) -> ASEL (List.map (rw' attr) xlst)
| Xml.Element ("always", [("loc", _)], xlst) -> ALWYS ("", List.map (rw' {attr with anchor=""}) xlst)
| Xml.Element ("sentree", [("loc", _)], xlst) -> SNTRE (List.map (rw' attr) xlst)
| Xml.Element ("senitem", [("loc", _); ("edgeType", etyp)], xlst) -> SNITM (etyp, List.map (rw' attr) xlst)
| Xml.Element ("begin", [("loc", _); ("name", namedblk)], xlst) ->
    let anonblk = let l = String.length namedblk and pat = "unnamedblk" in let l' = String.length pat in 
        (if l > l' && String.sub namedblk 0 l' = pat then pat else namedblk)^"_"^string_of_int !namedcnt in
    incr namedcnt;
    while_opt "" (Some anonblk) (List.map (rw' attr) xlst)
| Xml.Element ("begin", _, xlst) -> while_opt "" None (List.map (rw' attr) xlst)
| Xml.Element (("assign"|"assigndly") as dly, [("loc", _); ("dtype_id", tid)], hd::tl::[]) ->
    let src = rw' attr hd and dst = rw' attr tl in
    let smpl = match simplify_asgn (dlyenc dly) attr dst src with hd :: [] -> hd | lst -> XML lst in
    smpl
| Xml.Element ("if", [("loc", _)], xlst) -> IF ("", List.map (rw' attr) xlst)
| Xml.Element ("add"|"sub"|"mul"|"muls" as op, [("loc", _); ("dtype_id", tid)], xlst) -> ARITH (arithop op, List.map (rw' attr) xlst)
| Xml.Element ("and"|"redand"|"or"|"redor"|"xor"|"redxor"|"xnor"|"redxnor"|"shiftl"|"shiftr"|"shiftrs" as log,
               [("loc", _); ("dtype_id", tid)], xlst) -> LOGIC (logop log, List.map (rw' attr) xlst)
| Xml.Element ("eq"|"neq"|"gt"|"gts"|"gte"|"gtes"|"eqwild"|"neqwild"|"ltes"|"lte"|"lt"|"lts" as cmp, [("loc", _); ("dtype_id", tid)], xlst) ->
    CMP (cmpop cmp, List.map (rw' attr) xlst)
| Xml.Element ("initial"|"final" as action, [("loc", _)], xlst) -> INIT ("", action, List.map (rw' attr) xlst)
| Xml.Element ("package", [("loc", _); ("name", nam); ("origName", nam')], xlst) -> PKG ("", nam, List.map (rw' attr) xlst)
| Xml.Element ("typedef", [("loc", _); ("name", nam); ("dtype_id", tid)], xlst) ->
    let idx = int_of_string tid in TYP (idx, (TYPDF nam, "", TYPNONE, List.map (fun arg -> (rw' attr arg)) xlst))
| Xml.Element ("func", [("loc", _); ("name", nam); ("dtype_id", tid)], xlst) ->
    FNC ("", nam, attr.typetable.(int_of_string tid), List.map (rw' attr) xlst)
| Xml.Element ("jumplabel", [("loc", _)], xlst) -> jump_opt "" (List.map (rw' attr) xlst)
| Xml.Element ("jumpblock", [("loc", _)], xlst) -> JMPBLK ("", List.map (rw' attr) xlst)
| Xml.Element ("jumpgo", [("loc", _)], xlst) -> JMPG ("", List.map (rw' attr) xlst)
| Xml.Element ("concat", [("loc", _); ("dtype_id", tid)], xlst) -> CAT ("", List.map (rw' attr) xlst)
| Xml.Element ("cvtpackstring", [("loc", _); ("dtype_id", tid)], xlst) -> CPS ("", List.map (rw' attr) xlst)
| Xml.Element ("cond", [("loc", _); ("dtype_id", tid)], xlst) -> CND ("", List.map (rw' attr) xlst)
| Xml.Element ("time", [("loc", _); ("dtype_id", tid)], []) -> TIM ("")
| Xml.Element ("sformatf", [("loc", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' attr) xlst)
| Xml.Element ("module", ("loc", _) :: ("name", nam) :: ("origName", _) :: attr', xlst) ->
    let (_,nam') = List.assoc nam !(attr.instances) in
    let attr' = {attr with anchor="";names=ref [];tmpvar=ref []} in
    let xlst' = List.map (rw' attr') xlst in
    attr.modulexml := (nam', ("", xlst', !(attr'.names))) :: !(attr.modulexml);
(*
    let fd = open_out (nam'^".elem") in
    output_string fd (dumplst xlst');
    output_string fd ("\n["^String.concat ";\n " (List.map (fun (k,x) -> dumps k^", "^dumptab !x) !(attr'.names))^"]\n");
    close_out fd;
*)
    MODUL ("", nam', xlst', !(attr'.tmpvar))
| Xml.Element ("case", [("loc", _)], xlst) -> CS ("", List.map (rw' attr) xlst)
| Xml.Element ("caseitem", [("loc", _)], xlst) -> CSITM ("", List.map (rw' attr) xlst)
| Xml.Element ("while", [("loc", _)], xlst) -> WHL (List.map (rw' attr) xlst)
| Xml.Element ("insiderange", [("loc", _); ("dtype_id", tid)], xlst) -> IRNG ("", List.map (rw' attr) xlst)
| Xml.Element ("funcref", [("loc", _); ("name", nam); ("dtype_id", tid)], xlst) -> FRF ("", nam, List.map (rw' attr) xlst)
| Xml.Element ("varxref", [("loc", _); ("name", nam); ("dtype_id", tid); ("dotted", dotted)], []) ->
    let typ' = attr.typetable.(int_of_string tid) in
    print_endline (dumptab typ');
    let dirop = if List.mem_assoc dotted !(attr.names) then
        begin
        let (dtype, _, _, _) as typ' = !(List.assoc dotted !(attr.names)) in
        print_endline (dumptab typ');
        match dtype with
            | IFCRFDTYP ifc -> print_endline ifc; Dinam ifc
            | oth -> print_endline (dumptyp oth); Dunknown
        end
    else
        begin
        print_endline (dotted^" is not io");
        Dunknown
        end in
    XRF ("", nam, tid, dotted, dirop)
| Xml.Element ("arg", [("loc", _)], xlst) -> ARG (List.map (rw' attr) xlst)
| Xml.Element ("readmem", [("loc", _)], xlst) -> SYS ("", "$readmemh", List.map (rw' attr) xlst)
| Xml.Element (("fclose"|"finish"|"stop" as sys), [("loc", _)], xlst) -> SYS ("", "$"^sys, List.map (rw' attr) xlst)
| Xml.Element ("initarray"|"sformat"|"initialstatic"|"stmtexpr" as op, _, xlst) -> SYS ("", "$"^op, List.map (rw' attr) xlst)
| Xml.Element ("inititem", [("index", ix)], xlst) -> ITM ("", ix, List.map (rw' attr) xlst)
| Xml.Element ("initarray"|"streaml"|"powsu"|"powss"|"realtobits"|"itord"|"rand"|"clog2"|"div"|"fopen" as op, [("loc", _); ("dtype_id", tid)], xlst) ->
    SYS ("", "$"^op, List.map (rw' attr) xlst)
| Xml.Element ("replicate", [("loc", _); ("dtype_id", tid)], xlst) -> REPL ("", int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("iface", [("loc", _); ("name", bus); ("origName", _)], xlst) ->
    let (_,bus') = List.assoc bus !(attr.instances) in
    let attr' = {attr with anchor="";names=ref []} in
    let xlst' = List.map (rw' attr') xlst in
    attr.modulexml := (bus', ("", xlst', !(attr'.names))) :: !(attr.modulexml);
    IFC ("", bus', xlst')
| Xml.Element ("ifacerefdtype", [("loc", _); ("id", num); ("modportname", nam)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num in
    TYP(idx,(IFCRFDTYP nam, nam, TYPNONE, xlst'))
| Xml.Element ("modport", [("loc", _); ("name", port)], xlst) -> IMP ("", port, List.map (rw' attr) xlst)
| Xml.Element ("modportvarref", [("loc", _); ("name", member); ("direction", dir)], xlst) -> IMRF ("", member, dirop dir, List.map (rw' attr) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype"|"voiddtype" as dtyp', ("loc", _) :: ("id", num) :: rnglst, xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and dtyp = typenc dtyp' in
    (match rnglst with
      | ("name", nam) :: tl ->
          TYP(idx, (dtyp,nam,typmap tl,xlst'))
      | _ -> TYP(idx, (dtyp,"",typmap rnglst,xlst')));
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp', [("loc", _); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and dtyp = typenc dtyp' in
    TYP(idx, (dtyp,nam,SUBTYP sub,xlst'))
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp', [("loc", _); ("id", num); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and typid = typenc dtyp' in    
    TYP(idx, (typid,"",SUBTYP sub,xlst'))
| Xml.Element ("enumitem" as dtyp, [("loc", _); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' attr) xlst)
| Xml.Element ("cells", [], xlst) ->
    attr.intf := [];
    let xlst' = List.map (rw' attr) xlst in
(*    let fd = open_out "traverse.cell" in
    let rec traverse' indent = function
        | CELL("", nam, subnam', hier, xlst) -> cell_traverse fd attr indent (nam,subnam'); List.iter (traverse' (indent^"  ")) xlst
        | _ -> () in
    List.iter (traverse' "") xlst';
    close_out fd;
*)
    CELLS(xlst', attr)
| Xml.Element ("cell", [("loc", _); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    let (_,subnam') = if List.mem_assoc subnam !(attr.instances) then
        List.assoc subnam !(attr.instances)
    else
        begin
        print_endline ("Cell not found: "^subnam);
        (INVALID, subnam)
        end in
    CELL("", nam, subnam', hier, List.map (rw' attr) xlst)
| Xml.Element ("display", [("loc", _); ("displaytype", nam)], xlst) -> DSPLY ("", nam, List.map (rw' attr) xlst)
| Xml.Element ("task", [("loc", _); ("name", nam)], xlst) -> TASKDEF("", nam, List.map (rw' attr) xlst)
| Xml.Element ("taskref", [("loc", _); ("name", nam); ("dtype_id", tid)], xlst) -> TASKRF("", nam, List.map (rw' attr) xlst)
| Xml.Element ("valueplusargs", [("loc", _); ("dtype_id", tid)], xlst) -> VPLSRGS("", int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("testplusargs", [("loc", _); ("dtype_id", tid)], xlst) ->
    TPLSRGS("", "", int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("modportftaskref", [("loc", _); ("name", nam)], []) -> MODPORTFTR ("", nam)
| Xml.Element ("typetable", _, xlst) ->
    let types = List.map (fun itm -> (function TYP(ix,t) -> (ix,t) | _ -> (0,(UNKDTYP, "",TYPNONE, []))) (rw' attr itm)) xlst in
    let max = fold1 (max) (List.map (function (ix,_) -> ix) types) in
    let typarr = Array.make (max+1) (UNKDTYP, "", TYPNONE, []) in
    let rec subtypmap = function
    | RNG [CNST (b,n); CNST (b',n')] -> TYPRNG(n,n')
    | EITM ("enumitem", itm, "", n, [CNST (w',n')]) -> TYPENUM(itm, n, (w',n'))
    | TYP (idx, ((MEMBDTYP, id, SUBTYP idx', []) as typ')) -> typarr.(idx) <- maptyp' typ'; TYPMEMBER(maptyp idx')
    | oth -> subothlst := oth :: !subothlst; failwith "subothlst"
    and lookup ix = if List.mem_assoc ix types then List.assoc ix types else (UNKDTYP, "", TYPNONE, [])
    and maptyp' = function
        | (CNSTDTYP, (s:string), SUBTYP idx', []) -> let (a,b,c,d) = maptyp idx' in (CNSTDTYP, s, RECTYP (a,"const",c,d), [])
        | (t, s, SUBTYP idx', lst) -> (t, s, RECTYP (maptyp idx'), List.map subtypmap lst)
	| (t, s, typ, lst) -> (t, s, typ, List.map subtypmap lst)
    and maptyp ix = maptyp' (lookup ix) in
    List.iter (fun (ix, typ') -> typarr.(ix) <- maptyp ix) types;
(*
    let fd = open_out "typetable.debug" in
    Array.iteri (fun ix itm -> output_string fd (string_of_int ix^":"^dumptab itm^"\n")) typarr;
    close_out fd;
*)
    TYPETABLE typarr
| Xml.Element ("scopename", [("loc", _); ("dtype_id", tid)], []) -> SCOPE tid
| Xml.Element ("conspackuorstruct", [("loc", _); ("dtype_id", tid)], xlst) -> CONSPACK (tid, List.map (rw' attr) xlst)
| Xml.Element ("conspackmember", [("loc", _); ("dtype_id", tid)], xlst) -> CONSPACKMEMB (tid, List.map (rw' attr) xlst)
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> errlst := err :: !errlst; failwith ("XML element "^str^" not supported")
	
let rec catitm modules packages interfaces (pth:string option) itms names' = function
| UNRY(Uextends _ as op, rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    let fref = unaryopv op in
    if not (List.mem (FUNCTION,fref) !(itms.needed)) then
        begin
        print_endline ("Generated function: "^fref);
        itms.needed := (FUNCTION,fref) :: !(itms.needed);
        Hashtbl.add functable fref (mkextendfunc op);
        end
| NTL(rw_lst)
| RNG(rw_lst)
| SNTRE(rw_lst)
| IRNG(_,rw_lst)
| JMPL(_,rw_lst)
| JMPG(_,rw_lst)
| JMPBLK(_,rw_lst)
| CS(_,rw_lst)
| CSITM(_,rw_lst)
| WHL(rw_lst)
| FORSTMT(_,_,_,_,_,_,_,rw_lst)
| ARG(rw_lst)
| FILS(_, rw_lst)
| EITM(_, _, _, _, rw_lst)
| VRF(_, _, rw_lst)
| SFMT(_, rw_lst)
| SYS(_,_, rw_lst)
| PORT(_, _, _, rw_lst)
| SEL(_, rw_lst)
| ASEL(rw_lst)
| SNITM(_, rw_lst)
| ASGN(_, _, rw_lst)
| ARITH(_, rw_lst)
| LOGIC(_, rw_lst)
| CMP(_, rw_lst)
| CAT(_, rw_lst)
| CPS(_, rw_lst)
| CND(_, rw_lst)
| DSPLY(_, _, rw_lst)
| VPLSRGS(_, _, rw_lst)
| UNRY(_, rw_lst)
| REPL(_, _, rw_lst) -> List.iter (catitm modules packages interfaces pth itms names') rw_lst
| XML(FILS _ :: rw_lst) -> List.iter (catitm modules packages interfaces pth itms names') rw_lst
| XML _ -> failwith "catitm XML"
| IO("", str1lst, typ1, dir, str3, clst) ->
    List.iter (fun str1 ->
        let typ' = ref typ1 in
        if List.mem_assoc str1 itms.names'' then typ' := !(List.assoc str1 itms.names'');
        List.iter (fun itm ->
            itms.ca := ("", VRF(str1, typ1,[]), itm) :: !(itms.ca);
            let (a,b,c,d) = typ1 in typ' := (a,"wire",c,d)) clst;
        itms.io := (str1, ("", !typ', dir, str3, List.map ioconn clst)) :: !(itms.io);
        ) str1lst
| VAR("", str1lst, typ1, "ifaceref") -> List.iter (fun str1 -> itms.ir := ("", str1, typ1) :: !(itms.ir)) str1lst
| VAR("", str1lst, typ1, str2) -> List.iter (fun str1 ->
    if not (List.mem_assoc str1 !(itms.v)) then
        itms.v := (str1, ("", typ1, str2, (UNKDTYP,"",TYPNONE,[]))) :: !(itms.v)) str1lst
| IVAR("", str1, typ', rwlst, int2) -> itms.iv := (str1, ("", typ', rwlst, int2)) :: !(itms.iv)
| CA("", (rght::lft::[] as args)) ->
    List.iter (catitm modules packages interfaces pth itms names') args;
    itms.ca := ("", lft, rght) :: !(itms.ca)
| INST("", instkind, str1lst, (kind, port_lst)) ->
    List.iter (fun str1 ->
        let pth' = match instkind, lcombine(pth,Some str1) with INTERFACE, _ -> str1 | MODULE, Some s -> s | _, _ -> failwith "lcombine" in
        print_endline ("Instance "^str1^" path is: "^pth');
        itms.inst := (pth', ("", kind, port_lst)) :: !(itms.inst)
        ) str1lst
| ALWYS("", SNTRE(SNITM ("POS", [VRF (ck, _, [])]) :: SNITM ("POS", [VRF (rst, _, [])]) :: []) :: rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    itms.alwys := ("", POSPOS(ck,rst), optitm rw_lst) :: !(itms.alwys)    
| ALWYS("", SNTRE(SNITM ("POS", [VRF (ck, _, [])]) :: []) :: rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    let rw_lst' = optitm rw_lst in
    if rw_lst' <> [] then itms.alwys := ("", POSEDGE(ck), rw_lst') :: !(itms.alwys)
| ALWYS("", SNTRE(SNITM ("NEG", [VRF (ck, _, [])]) :: []) :: rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    itms.alwys := ("", NEGEDGE(ck), optitm rw_lst) :: !(itms.alwys)
| ALWYS("", SNTRE(SNITM (("POS"|"NEG") as edg, [VRF (ck, _, [])]) :: SNITM ("NEG", [VRF (rst, _, [])]) :: []) :: rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    let rw_lst' = (function
       | BGN(lbl, (IF("", VRF(rst', typ',[]) :: thn :: els :: []) :: [])) :: [] ->
           BGN(lbl, (IF("", UNRY(Unot, VRF(rst', typ',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | IF("", VRF(rst', typ',[]) :: thn :: els :: []) :: [] ->
           BGN(None, (IF("", UNRY(Unot, VRF(rst', typ',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | oth -> posneglst := oth :: !posneglst; oth) rw_lst in
    itms.alwys := ("", (match edg with "POS" -> POSNEG(ck,rst) | "NEG" -> NEGNEG(ck,rst) | _ -> UNKNOWN), optitm rw_lst') :: !(itms.alwys)
| ALWYS("", rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    itms.alwys := ("", COMB, rw_lst) :: !(itms.alwys)
| INIT ("", "initial", rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    (match rw_lst with
        | ASGN (false, _, (CNST cnst) :: VRF (id, _, []) :: []) :: [] ->
             itms.cnst := (id,(is_cnst itms id,cnst)) :: !(itms.cnst);
             print_endline ("initial found : "^id);
        | _ -> itms.init := ("", INITIAL, rw_lst) :: !(itms.init));
| INIT ("", "final", rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    itms.init := ("", FINAL, rw_lst) :: !(itms.init)
| BGN(pth', rw_lst) ->
    let newpth = lcombine (pth,pth') in
    print_endline ("New path: "^match newpth with Some pth -> pth | None -> "");
    List.iter (catitm modules packages interfaces newpth itms names') rw_lst
| FNC("", nam, typ', rw_lst) ->
    let itms' = empty_itms [] in
    List.iter (catitm modules packages interfaces pth itms' names') rw_lst;
    let fn = ("", typ', rw_lst, itms') in
    itms.func := (nam, fn) :: !(itms.func);
    Hashtbl.add functable nam fn;
| IF("", rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    itms.gen := ("",rw_lst) :: !(itms.gen)
| IMP("", nam, rw_lst) ->
    itms.imp := (nam, ("", List.map (function
    | IMRF(_, str1, dir, []) -> (str1, dir)
    | MODPORTFTR (_,str1) -> (str1, Dunknown)
    | oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst;;1442") rw_lst)) :: !(itms.imp)
| IMRF("", str1, str2, []) -> ()
| TASKDEF ("", nam, rw_lst) ->
    let itms' = empty_itms [] in
    List.iter (catitm modules packages interfaces pth itms' names') rw_lst;
    let tsk = ("", rw_lst, itms') in
    itms.task := (nam, tsk) :: !(itms.task);
    Hashtbl.add tasktable nam tsk;
| TASKRF ("", nam, rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    if not (List.mem (TASK,nam) !(itms.needed)) then
        itms.needed := (TASK,nam) :: !(itms.needed)
| CNST _ -> ()
| FRF(_, nam, rw_lst) ->
    List.iter (catitm modules packages interfaces pth itms names') rw_lst;
    if not (List.mem (FUNCTION,nam) !(itms.needed)) then
        begin
        print_endline ("Needed function: "^nam);
        itms.needed := (FUNCTION,nam) :: !(itms.needed);
        end
| XRF("", str1, str2, str3, dirop) -> ()
| MODUL("", nam', rw_lst, tmpvar) ->
    print_endline ("Module: "^nam');
    itmopt := Some rw_lst;
    let (orig'', xlst'', names'') = List.assoc nam' names' in
    let itms = empty_itms [] in
    List.iter (fun (str1, (str2, typ1)) ->
        itms.v := (str1, ("", typ1, str1, (UNKDTYP,"",TYPNONE,[]))) :: !(itms.v)) tmpvar;
    List.iter (catitm modules packages interfaces None itms names') rw_lst;
    let itms' = rev_itms itms in
    Hashtbl.add modules nam' ("", itms')
| PKG("", nam', rw_lst) ->
    print_endline ("Package: "^nam');
    itmopt := Some rw_lst;
    let itms = empty_itms [] in
    List.iter (catitm modules packages interfaces (Some nam') itms names') rw_lst;
    Hashtbl.add packages nam' ("", itms)
| IFC("", nam', rw_lst) ->
    let (orig'', xlst'', names'') = List.assoc nam' names' in
    let itms' = empty_itms [] in
    List.iter (catitm modules packages interfaces (Some nam') itms' names') rw_lst;
    Hashtbl.add interfaces nam' ("", itms')
| FIL(enc, fil) ->
    Hashtbl.add files enc fil
| CELLS(rw_lst,_) -> ()
| TPLSRGS (_, id, tid, _) -> ()
| TYPETABLE _ -> ()
| TYP _ -> ()
| TIM _ -> ()
| SCOPE _ -> ()
| ITM _ -> ()
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst;;1508"

(*
let chktyp = function
| "wire" -> WIRE
| "logic" -> LOGIC
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

let outnam f = f^".v"
let outnamopt f = let l = String.length f in f^(if l < 4 || String.sub f (l-4) 4 <> "_opt" then "_opt.v" else ".v")
let outtok f = f^"_tokens.txt"
let outtcl f = "./"^f^"_fm.tcl"

let needed modul (kind,nam) = match kind with
| FUNCTION ->
    print_endline ("Searching function: "^nam);
    let found = List.mem_assoc nam !(modul.func) in
    let ("", typ', lst, itms') = if found then
        List.assoc nam !(modul.func)
    else
        Hashtbl.find functable nam in
    let stg = ref FIRSTG in let lst = fsrc "" :: FUNCTION :: SP :: (varlst modul (ref NL) typ' nam) @
    List.flatten (List.map (fnstmt modul false stg) (List.tl lst)) in
    lst @ (fndlm !stg @ [ENDFUNCTION;NL])
| TASK ->
    print_endline ("Searching task: "^nam);
    let found = List.mem_assoc nam !(modul.task) in
    let ("", lst, itms') = if found then
        List.assoc nam !(modul.task)
    else
        Hashtbl.find tasktable nam in
    let lst = List.flatten (List.map (taskstmt modul false nam) lst) in
    fsrc "" :: TASK :: SP :: IDENT nam :: SEMI :: BEGIN None :: lst @ END :: ENDTASK :: []
| oth -> failwith "needed"

let othedg = ref None

let dump intf f ("", modul) =
  let appendlst = ref [] in
  let append lst = appendlst := lst :: !appendlst in
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" : "^outtcl f ^" */");
  let head = ref [fsrc ""; if intf then INTERFACE else MODULE; SP; IDENT f] in
  let delim = ref LPAREN in
  List.iter (fun (io, ("", typ', dir, kind', lst)) -> 
    head := !head @ iolst modul delim dir io typ';
    delim := COMMA;
    ) (!(modul.io));
  head := !head @ (if !delim <> COMMA then !delim :: [] else []) @ [RPAREN;SEMI];
  List.iter (fun (id, ("", typ', kind', n)) -> append (fsrc "" :: varlst modul (ref NL) typ' id @ SEMI :: NL :: []);
                 ) (List.rev !(modul.v));
  List.iter (fun itm -> append (needed modul itm)) (List.rev !(modul.needed));
  List.iter (fun (a, ("", lst)) -> let delim = ref LPAREN in
    let lst = MODPORT :: SP :: IDENT a ::
    List.flatten (List.map (fun (nam',dir') -> let lst = !delim :: DIR dir' :: SP :: IDENT nam' :: [] in delim := COMMA; lst) lst) @
    [RPAREN; SEMI] in
    append ((fsrc "") :: lst)) !(modul.imp);  
  List.iter (fun ("", dst, src) ->
                 append (fsrc "" :: ASSIGN :: SP :: expr modul dst @ (SP :: ASSIGNMENT :: SP:: expr modul src @ SEMI :: NL :: []));
                 ) (List.rev !(modul.ca));
  List.iter (function
    | ("", COMB, (SNTRE [] :: lst)) ->
      append (fsrc "" :: ALWAYS :: AT :: STAR :: flatten1 modul false lst);
    | ("", COMB, (BGN (None, lst) :: [])) ->
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
    | ("", _, lst) as edg -> othedg := Some edg; failwith "edge specification not implemented";
    ) (List.rev (List.map (fun ("",edg,lst) -> ("", edg, lst)) !(modul.alwys)));
  List.iter (fun (inst, ("", kind, lst)) ->
                 let delim = ref SP in
                 let lst = List.flatten (List.map (fun term -> let lst = !delim :: portconn modul term in delim := COMMA; lst) lst) in
                 append (fsrc "" :: IDENT kind :: SP :: IDENT inst :: LPAREN :: lst @ [RPAREN;SEMI]);
                 ) !(modul.inst);
  List.iter (fun ("", tok, lst) -> append (fsrc "" :: tok :: flatten1 modul false lst);
                 ) !(modul.init);
  !head @ List.flatten (List.sort compare !appendlst) @ [NL;if intf then ENDINTERFACE else ENDMODULE;NL;NL]

let rec iterate depth f (modorig, modul) =
    let indent = String.make depth ' ' in
    let newitms = copy_itms modul in
    newitms.ir := [];
    newitms.inst := [];
    print_string (indent^"Scanning instances of "^f^": ["^String.concat ";" (List.map (fun (inst, (_,kind,_)) -> inst^"("^kind^")") !(modul.inst))^"]");
    List.iter (fun (inst, ("", kind, iolst)) ->
        if Hashtbl.mem interfaces kind then
           begin
           let (_, intf) = Hashtbl.find interfaces kind in
           List.iter (fun (nam, ("", typ', kind, n)) ->
                 let pth = inst^"_"^nam in
                 newitms.v := (pth, (modorig, typ', kind, n)) :: !(newitms.v);
                ) !(intf.v);
           end
        else if Hashtbl.mem modules kind then
           begin
           print_endline (indent^"Iterating: "^kind);
           let (kindorig, itms) = Hashtbl.find modules kind in
           let newiolst = ref [] in
           let newinnerlst = ref [] in
	   let previolst = !(itms.io) in
           List.iter2 (fun ((_, ("", typ', idir', typ'', ilst)) as inr) -> function
		       | VRF (id, typ', []) ->
                           newiolst := PORT("", id, idir', [VRF(id, typ', [])]) :: !newiolst;
                           newinnerlst := inr :: !newinnerlst;
		       | PORT ("", id_i, Dvif bus, VRF (id, _, []) :: []) as pat ->
                           if List.mem_assoc id !(modul.inst) then
                               begin
                               let (_,inam,_) = List.assoc id !(modul.inst) in
                               print_endline (indent^id^" is a local bus mapped to "^inam);
                               bus := inam;
                               end;
                           print_endline (indent^id^" connects to "^id_i);
                           let inam = !bus in
                           print_endline (indent^id^" maps to "^inam);
                           if Hashtbl.mem interfaces inam then (match typ' with (IFCRFDTYP iport, simple, TYPNONE, []) ->
                              begin
                              print_endline (indent^iport^" matched");
                              let (_, intf) = Hashtbl.find interfaces inam in
                              print_endline (indent^"Searching for "^ iport);
                              if List.mem_assoc iport !(intf.imp) then
                              let ("", lst) = List.assoc iport !(intf.imp) in
                              List.iter (fun (nam, dir) ->
                                    print_endline (indent^inam^":"^iport^":"^nam^":"^id_i);
                                    let (_, typ', kind, _) = List.assoc nam !(intf.v) in
                                    newiolst := PORT("", id_i^"_"^nam, dir, [VRF(id^"_"^nam, typ', [])]) :: !newiolst;
                                    newinnerlst := (id_i^"_"^nam, ("", typ', dir, typ'', ilst)) :: !newinnerlst) lst
                              else print_endline (indent^"Direction "^ iport ^" not found");
                              end
                           | _ ->
                              print_endline (indent^dumptab typ'^" did not match");
                              newiolst := pat :: !newiolst; newinnerlst := inr :: !newinnerlst)
			   else
                               begin
			       newiolst := pat :: !newiolst;
			       newinnerlst := inr :: !newinnerlst;
			       end
		       | PORT _ as pat -> newiolst := pat :: !newiolst; newinnerlst := inr :: !newinnerlst
		       | oth -> portothlst := oth :: !portothlst; failwith "portothlst"
		       ) previolst iolst;
           let newiolst = List.rev !newiolst in
           let newinnerlst = List.rev !newinnerlst in
	   let kind_opt = kind^"_opt" in
           if not (Hashtbl.mem modules_opt kind_opt) then
               begin
               let newinneritms = copy_itms itms in
               newinneritms.io := newinnerlst;
               let newhash = (kindorig, newinneritms) in
	       iterate (depth+2) kind newhash
               end;
           newitms.inst := (inst, ("", kind_opt, newiolst)) :: !(newitms.inst);
           end
        ) !(modul.inst);
    newitms.inst := List.rev (!(newitms.inst));
    Hashtbl.replace modules_opt (f^"_opt") (modorig, newitms);
    print_endline (indent^f^" done")

let dumpform f f' separate = 
    let fd = open_out (outtcl f') in
    let srcpath = try Sys.getenv "XMLSRCPATH" with err -> "." in
    Printf.fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
    Printf.fprintf fd "set hdlin_warn_on_mismatch_message \"FMR_ELAB-115 FMR_ELAB-117 FMR_ELAB-146 FMR_ELAB-147 FMR_VLOG-928\"\n";
    Printf.fprintf fd "read_sverilog -container r -libname WORK -12 { \\\n";
    let plst = ref [] in Hashtbl.iter (fun _ (s,_) -> plst := fst (find_source s) :: !plst) packages;
    let iflst = List.map snd (if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else []) in
    let hlst = List.sort_uniq compare (List.map (fun k -> let (s, _) = if Hashtbl.mem modules k then Hashtbl.find modules k else (k, empty_itms []) in fst (find_source s)) (f::iflst)) in
    let slst = !plst @ hlst in
    List.iter (fun src -> if src.[0] == '/' then Printf.fprintf fd "%s \\\n" src else Printf.fprintf fd "%s/%s \\\n" srcpath src) slst;
    Printf.fprintf fd "}\n";
    Printf.fprintf fd "set_top r:/WORK/%s\n" f;
    Printf.fprintf fd "read_sverilog -container i -libname WORK -12 { \\\n";
    let hlst' = List.sort_uniq compare (f' :: (if separate then iflst else [])) in
    List.iter (fun nam -> Printf.fprintf fd "%s \\\n" (outnamopt nam)) hlst';
    Printf.fprintf fd "}\n";
    Printf.fprintf fd "set_top i:/WORK/%s\n" f';
    Printf.fprintf fd "match\n";
    Printf.fprintf fd "report_potentially_constant_registers\n";
    Printf.fprintf fd "verify\n";
    Printf.fprintf fd "report_failing_points -inputs unmatched -inputs undriven\n";
    Printf.fprintf fd "analyze_points -all\n";
    Printf.fprintf fd "quit\n";
    close_out fd;
    Unix.chmod (outtcl f') 0o740    
*)
    
let rec debug f (origin, modul) =
  let fd = open_out (f^".debug") in
  dumpitms fd modul;
  close_out fd

let readxml xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos, xml) | None -> (0, (0,0), xml)

let rewrite errlst xmlf =
    let (line,range,xml) = readxml xmlf in
    let attr' = empty_attr errlst in
    let rwxml = rw' attr' xml in
    (line,range,xml,rwxml)

let translate errlst xmlf =
    let modules = Hashtbl.create 255 in
    let packages = Hashtbl.create 255 in
    let interfaces = Hashtbl.create 255 in
    let (line,range,xml,rwxml) = rewrite errlst xmlf in
    let cell_lst,topattr = match rwxml with XML
       [FILS ("files", fil_lst);
	FILS ("module_files", fil_lst');
	NTL modlst;
	CELLS (cell_lst,topattr)] -> (cell_lst,topattr) | _ -> ([],empty_attr()) in
    let toplst = List.flatten(List.map cell_hier cell_lst) in
    let empty = empty_itms [] in
    catitm modules packages interfaces None empty !(topattr.modulexml) rwxml;
    let top = snd(List.hd toplst) in
    print_endline ("toplevel is "^top);
    let mods = ref [] in
    Hashtbl.iter (fun k (_,x) ->
        let rawtok = dump true k x in
        let d = reformat0 rawtok in
        mods := (k, rawtok, d, reformat2 (reformat1 d)) :: !mods) interfaces;
    Hashtbl.iter (fun k (_,x) ->
        let rawtok = dump false k x in
        let d = reformat0 rawtok in
        mods := (k, rawtok, d, reformat2 (reformat1 d)) :: !mods) modules;
    let mods = List.sort compare !mods in
    (line,range,rwxml,xml,mods,toplst,topattr,modules,packages,interfaces)

open Input
open Input_rewrite
open Hardcaml
open Always
open Signal
open Input
open Input_rewrite
open Input_lex

type remapp =
  | Void
  | Id of string
  | Alw of Hardcaml.Always.t
  | Const of string
  | Infix of string
  | Prefix of string
  | Add of remapp * remapp
  | Eq of remapp * remapp
  | If_ of remapp * remapp list * remapp list
  | Asgn of remapp * remapp * remapp
  | Concat of remapp * remapp
  | Select of remapp * string * string

type remap =
  | Invalid
  | Sig of Hardcaml.Signal.t
  | Alw of Hardcaml.Always.t
  | Var of Hardcaml.Always.Variable.t
  | Con of Hardcaml.Constant.t

let othr = ref Invalid
let othlhs = ref Void
let othrhs = ref Void
let othrmlhs = ref Invalid
let othrmrhs = ref Invalid
let othlhs' = ref Void
let othrhs' = ref Void
let othrmlhs' = ref Invalid
let othrmrhs' = ref Invalid
let othp = ref Void
let otht = ref None
let othdim = ref None
let othremap = ref []

let cnv v =
begin
let p = Input_rewrite.parse v in

let reflst = ref [] in
let _ = Hashtbl.iter (fun k x -> reflst := (k,x) :: !reflst) refh in
let reflst = !reflst in
let declare_inputh = Hashtbl.create 255 in
let declare_wireh = Hashtbl.create 255 in
let declare_regh = Hashtbl.create 255 in

let declare_input port =
  if false then print_endline port;
    let wid = if Hashtbl.mem refh (STRING port) then
      match Hashtbl.find refh (STRING port) with
    | {lft = VpiNum lft'; rght = VpiNum rght'; lfttyp = Vpiuintconst;
   rghttyp = Vpiuintconst; lftsiz = VpiNum "64"; rghtsiz = VpiNum "64"} -> int_of_string lft' - int_of_string rght' + 1
    | oth -> othdim := Some oth; 1 else 1 in
  Hashtbl.add declare_inputh port (Signal.input port wid) in

let declare_wire = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TLIST pth)), STRING wire) ->
  if true then print_endline wire;
    let wid = if Hashtbl.mem refh (STRING wire) then
      match Hashtbl.find refh (STRING wire) with
    | {lft = VpiNum lft'; rght = VpiNum rght'; lfttyp = Vpiuintconst;
   rghttyp = Vpiuintconst; lftsiz = VpiNum "64"; rghtsiz = VpiNum "64"} -> int_of_string lft' - int_of_string rght' + 1
    | oth -> othdim := Some oth; 1 else 1 in
  Hashtbl.add declare_wireh wire (Always.Variable.wire ~default:(Signal.zero wid))
| _ -> () in

let declare_reg = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TLIST pth)), STRING reg) -> if Hashtbl.mem declare_regh reg then () else
  begin
  if true then print_endline reg;
    let wid = if Hashtbl.mem refh (STRING reg) then
      match Hashtbl.find refh (STRING reg) with
    | {lft = VpiNum lft'; rght = VpiNum rght'; lfttyp = Vpiuintconst;
   rghttyp = Vpiuintconst; lftsiz = VpiNum "64"; rghtsiz = VpiNum "64"} -> int_of_string lft' - int_of_string rght' + 1
    | oth -> othdim := Some oth; 1 else 1 in
  let clock = Hashtbl.find declare_inputh "clock" in
  let clear = Hashtbl.find declare_inputh "clear" in
  let r_sync = Reg_spec.create ~clock ~clear () in
  Hashtbl.add declare_regh reg (Always.Variable.reg ~enable:Signal.vdd r_sync ~width:wid);
  end
| _ -> () in

let rec traverse pass2 = function
| TUPLE3 (Always, Vpiassignstmt,
   TUPLE3 (Assignment, lhs, rhs)) ->
   traverse pass2 lhs;
   traverse pass2 rhs;
   if not pass2 then declare_wire lhs
| TUPLE3 (Assignment, lhs, rhs) ->
   traverse pass2 lhs;
   traverse pass2 rhs;
   if pass2 then declare_reg lhs
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, edg), stmt) -> traverse pass2 edg; traverse pass2 stmt
| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> traverse pass2 cond; traverse pass2 lhs; traverse pass2 rhs
| TUPLE4 (If_else, cond, lhs, rhs) -> traverse pass2 cond; traverse pass2 lhs; traverse pass2 rhs
| TUPLE3 (Vpiaddop, lhs, rhs) -> traverse pass2 lhs; traverse pass2 rhs
| TUPLE3 (Vpieqop, lhs, rhs) -> traverse pass2 lhs; traverse pass2 rhs
| TUPLE2 (Vpiconcatop, TLIST lst) -> List.iter (traverse pass2) lst
| TUPLE4 (Part_select, STRING nam, VpiNum lft, VpiNum rght) -> ()
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TLIST pth)), STRING wire) -> ()
| TUPLE2 (Vpioutput, STRING port)	    -> ()
| TUPLE2 (Vpiinput, STRING port)	    -> if not pass2 then declare_input port
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> ()
| TUPLE3 (Logic_net, Vpinet, STRING net) -> ()
| TUPLE2 (Logic_net, STRING net) -> ()
| STRING s -> ()
| TLIST [] -> ()
| VpiNum s -> ()
| (Always|Vpitopmodule|Vpitop|Vpiname) -> ()
| oth -> otht := Some oth in

let _ = List.iter (traverse false) p in
let _ = List.iter (traverse true) p in
  
let edgstmt = ref Work in
let edgstmt' = ref Void in
let conlst = ref [] in
let conlst' = ref [] in

let rec (remapp:token->remapp) = function
| TUPLE2 (TUPLE2 (Vpiactual, TUPLE2 (Logic_net, TLIST pth)), STRING wire) ->
if Hashtbl.mem declare_inputh wire then Id (wire)
else if Hashtbl.mem declare_regh wire then Id (wire)
else if Hashtbl.mem declare_wireh wire then Id (wire)
else Void
| TUPLE3 (Vpieqop, lhs, rhs) -> Eq (remapp lhs, remapp rhs)
| TUPLE4 (If_else, cond, lhs, rhs) ->
  let then_ =  (remapp lhs) in
  let else_ =  (remapp rhs) in
  let cond_ =  (remapp cond) in
  If_ (cond_, [then_], [else_])
| TUPLE3 (Always, Vpiassignstmt,
   TUPLE3 (Assignment, lhs, rhs)) ->
   let lhs = (remapp lhs) in
   let rhs = (remapp rhs) in
   Asgn (lhs, Infix "<--", rhs)
| TUPLE3 (Assignment, lhs, rhs) ->
   let lhs = (remapp lhs) in
   let rhs = (remapp rhs) in
   Asgn (lhs, Infix "<--", rhs)
| TUPLE3 (Always, TUPLE2 (Vpiposedgeop, edg), TUPLE4(If_else, clear, rst, stmt)) ->
   edgstmt := stmt; 
   let stmt' = remapp stmt in
   edgstmt' := stmt';
   stmt'
| TUPLE4 (Vpiconditionop, cond, lhs, rhs) -> If_ (remapp cond, [remapp lhs], [remapp rhs])
| TUPLE3 (Vpiaddop, lhs, rhs) -> Add (remapp lhs, remapp rhs)
| TUPLE2 (Vpiconcatop, TLIST lst) ->
   conlst := lst;
   let lst' = List.map remapp lst in
   conlst' := lst';
   let rec concat = function
     | [] -> failwith "concat"
     | hd :: [] -> hd
     | hd :: tl -> Concat(hd, concat tl) in
   concat lst'
| TUPLE4 (Part_select, STRING nam, VpiNum lft, VpiNum rght) -> Select(Id nam, lft, rght)
| TUPLE2 (Vpioutput, STRING port)	    -> Void
| TUPLE2 (Vpiinput, STRING port)	    -> Void
| TUPLE3 (Logic_net, Vpireg, STRING reg) -> Void
| TUPLE3 (Logic_net, Vpinet, STRING net) -> Void
| TUPLE2 (Logic_net, STRING net) -> Void
| STRING s -> Void
| TLIST [] -> Void
| VpiNum s ->
    let cons = Scanf.sscanf s "%d'b%s" (fun w s -> Const s) in
    cons
| (Always|Vpitopmodule|Vpitop|Vpiname) -> Void
| oth -> otht := Some oth; Void in

let remapp' = List.filter (function Void -> false |_ -> true) (List.map remapp p) in

let rec strength_reduce = function
| Asgn (Id c, Infix "<--",
  If_ (cond, then_, else_)) ->
  If_ (cond, List.map (fun itm -> Asgn (Id c, Infix "<--", itm)) then_, List.map (fun itm -> Asgn (Id c, Infix "<--", itm)) else_)
| oth -> oth in

let rec combiner = function
| If_ (Eq (a, b), c, d) ::
  If_ (Eq (a', b'), c', d') :: [] when a=a' && b=b' -> If_ (Eq (a, b), c@c', d@d') :: []
| oth -> oth in

let remapp'' = List.map strength_reduce remapp' in
let remapp''' = combiner remapp'' in

let sig' = function
| Sig x -> x
| oth -> othr := oth; failwith "sig'"
 in
let alw' = function
| Alw x -> x
| oth -> othr := oth; failwith "alw'" in

let var' = function
| Var x -> x
| oth -> othr := oth; failwith "var'" in

let rec (remap:remapp->remap) = function
| Id wire ->
if Hashtbl.mem declare_inputh wire then Sig (Hashtbl.find declare_inputh wire)
else if Hashtbl.mem declare_regh wire then Var (Hashtbl.find declare_regh wire)
else if Hashtbl.mem declare_wireh wire then Var (Hashtbl.find declare_wireh wire)
else Invalid
| Eq (lhs, rhs) -> Sig (sig' (remap lhs) ==: sig' (remap rhs))
| Add (lhs, rhs) -> Sig (sig' (remap lhs) +: sig' (remap rhs))
| If_ (cond, lhs, rhs) ->
  let cond_ = sig' (remap cond) in
  let then_ = List.map (fun itm -> alw' (remap itm)) lhs in
  let else_ = List.map (fun itm -> alw' (remap itm)) rhs in
  Alw (if_ (cond_) then_ else_)
| Asgn (lhs, Infix "<--", rhs) ->
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = var' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Alw (lhs <-- rhs)
| Const s ->
    let cons = Signal.of_bit_string s in
    Sig cons
| Concat (lhs, rhs) -> 
   othlhs' := lhs;
   othrhs' := rhs;
   othrmlhs' := remap lhs;
   othrmrhs' := remap rhs;
   let lhs = sig' (remap lhs) in
   let rhs = sig' (remap rhs) in
   Sig (lhs @: rhs)
| Select(nam, lft, rght) -> Sig (select (sig' (remap nam)) (int_of_string lft) (int_of_string rght))
| oth -> othp := oth; failwith "remap"
 in

(*
let aa = Signal.input "aa" 8 in
let bb = Signal.input "bb" 8 in
let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()
let cc_reg = Always.Variable.reg ~enable:Signal.vdd r_sync ~width:8
let cc_wire = Always.Variable.wire ~default:(Signal.zero 8)

let blk' = [
    if_ (aa ==: bb) [cc_reg <-- (select aa 6 0) @: Signal.of_bit_string "0";
                     cc_wire <-- (select aa 6 0) @: Signal.of_bit_string "0"]
    [cc_reg <-- aa +: bb; cc_wire <-- aa +: bb]
 ]
 
let () = Always.(compile blk') in

let cct' = Hardcaml.Circuit.create_exn ~name:"creat'" (List.map (fun (s,v) -> output s v) ["cc_wire", cc_wire.value; "cc_reg", cc_reg.value])
*)

let remap' = List.filter (function Invalid -> false | Sig _ -> false |_ -> true) (List.map remap remapp''') in
let remap'' = List.map alw' remap' in
let () = Always.compile remap'' in
othremap := remapp''';
let c_reg = Hashtbl.find declare_regh "c_reg" in
let c_wire = Hashtbl.find declare_wireh "c_wire" in
let cct = Hardcaml.Circuit.create_exn ~name:"creat" (List.map (fun (s,v) -> output s v) ["c_wire", c_wire.value; "c_reg", c_reg.value]) in
cct
end

let _ = if Array.length Sys.argv > 1 then Hardcaml.Rtl.print Verilog (cnv Sys.argv.(1))

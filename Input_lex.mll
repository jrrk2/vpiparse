(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  open Input
  open Input_types

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let oldcnt = ref 1

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
FINISHED, "FINISHED";
INT, "|INT";
KILLED, "KILLED";
Post_Elab, "Post-Elab";
Pre_Elab, "Pre-Elab";
RUNNING, "RUNNING";
Restored, "Restored";
SUSPENDED, "SUSPENDED";
UINT, "|UINT";
WAITING, "WAITING";
Always, "\\_always";
Assignment, "\\_assignment";
Begin, "\\_begin";
Class_defn, "\\_class_defn";
Class_typespec, "\\_class_typespec";
Class_var, "\\_class_var";
Constant, "\\_constant";
DESIGN, "design";
Design, "\\_design";
Enum_const, "\\_enum_const";
Enum_typespec, "\\_enum_typespec";
Enum_var, "\\_enum_var";
Event_control, "\\_event_control";
Function, "\\_function";
Int_typespec, "\\_int_typespec";
Int_var, "\\_int_var";
Io_decl, "\\_io_decl";
Logic_net, "\\_logic_net";
Logic_typespec, "\\_logic_typespec";
Logic_var, "\\_logic_var";
Module_inst, "\\_module_inst";
Operation, "\\_operation";
Package, "\\_package";
Port, "\\_port";
Range, "\\_range";
Ref_obj, "\\_ref_obj";
Ref_typespec, "\\_ref_typespec";
Task, "\\_task";
Var_select, "\\_var_select";
Weaklyreferenced, "\\_weaklyReferenced";
Any_sverilog_class, "any_sverilog_class";
Array, "array";
Await, "await";
Bound, "bound";
Builtin, "builtin";
(*
Clk, "clk";
*)
Endln, "endln";
File, "file";
Get, "get";
Keycount, "keyCount";
Kill, "kill";
Line, "line";
Mailbox, "mailbox";
Message, "message";
New, "new";
Num, "num";
Peek, "peek";
Process, "process";
Put, "put";
Queue, "queue";
Resume, "resume";
Self, "self";
Semaphore, "semaphore";
State, "state";
Status, "status";
String, "string";
Suspend, "suspend";
System, "system";
Try_get, "try_get";
Try_peek, "try_peek";
Try_put, "try_put";
Uhdmallclasses, "|uhdmallClasses";
Uhdmallmodules, "|uhdmallModules";
Uhdmallpackages, "|uhdmallPackages";
Uhdmtopmodules, "|uhdmtopModules";
Uhdmtoppackages, "|uhdmtopPackages";
Vpiactual, "|vpiActual";
Vpialwaystype, "|vpiAlwaysType";
Vpiclassdefn, "|vpiClassDefn";
Vpicondition, "|vpiCondition";
Vpiconsttype, "|vpiConstType";
Vpidecompile, "|vpiDecompile";
Vpidefname, "|vpiDefName";
Vpidirection, "|vpiDirection";
Vpielaborated, "|vpiElaborated";
Vpienumconst, "|vpiEnumConst";
Vpiexpr, "|vpiExpr";
Vpifullname, "|vpiFullName";
Vpiiodecl, "|vpiIODecl";
Vpiindex, "|vpiIndex";
Vpiinstance, "|vpiInstance";
Vpileftrange, "|vpiLeftRange";
Vpilhs, "|vpiLhs";
Vpilowconn, "|vpiLowConn";
Vpimethod, "|vpiMethod";
Vpiname, "|vpiName";
Vpinet, "|vpiNet";
Vpinettype, "|vpiNetType";
Vpioptype, "|vpiOpType";
Vpioperand, "|vpiOperand";
Vpiparent, "|vpiParent";
Vpiport, "|vpiPort";
Vpiprocess, "|vpiProcess";
Vpirange, "|vpiRange";
Vpireturn, "|vpiReturn";
Vpirhs, "|vpiRhs";
Vpirightrange, "|vpiRightRange";
Vpisigned, "|vpiSigned";
Vpisize, "|vpiSize";
Vpistmt, "|vpiStmt";
Vpitop, "|vpiTop";
Vpitopmodule, "|vpiTopModule";
Vpitypedef, "|vpiTypedef";
Vpitypespec, "|vpiTypespec";
Vpivariables, "|vpiVariables";
Vpivisibility, "|vpiVisibility";
Work, "work";
      ];
    fun s -> Hashtbl.find h s

let tok' x = "\""^Ord_input.getstr x^"\""

let import_seen = ref false

let tok arg =
  if false then print_endline (match arg with STRING s -> s | _ -> tok' arg);
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '|' '\\'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$' ]*
let number = ['0'-'9']['0'-'9' '_']*
let space = [' ' '\t' '\r']+
let newline = ['\n'] [' ']*
let qstring = '"'[^'"']*'"'
let comment = '.''.'[^'\n']*
let vpimethod = "|vpiMethod:"['0'-'9']+
   
rule token = parse
| '#' { tok ( HASH ) }
| ''' { tok ( QUOTE ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| ',' { tok ( COMMA ) }
| ':' { tok ( COLON ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }
| '.' { tok ( DOT ) }
| '(' { tok ( LPAREN ) }
| ')' { tok ( RPAREN ) }
| '@' { tok ( AT ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
(*
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
*)

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline as s
      { let old = !oldcnt in oldcnt := String.length s; if old < !oldcnt then Indent else token lexbuf }
  | number as n
      { tok ( VpiNum (int_of_string n) ) }
  | vpimethod as s
      { tok ( Vpimethodint (Scanf.sscanf s "|vpiMethod:%d" (fun n -> n))) }
  | ident as s
      { tok ( try keyword s with Not_found -> STRING s ) }
  | qstring as s
      { tok ( STRING (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Input_lex: "^String.make 1 oth) ) }

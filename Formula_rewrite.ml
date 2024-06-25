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

open Formula_types
open Formula

let parse_output_ast_from_string str =
  let lb = Lexing.from_string str in
  let output = try
      Formula.ml_start Formula_lex.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Formula.parse: parse error at character %d" n);
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  output

let parse arg =
  let rslt = parse_output_ast_from_string arg in
  rslt

let unhand = ref None

let rec rw' = function
| IDENT id -> Rtl_parser.IDSTR id
| TUPLE2 (PLING, arg) -> DOUBLE (NOT, rw' arg)
| TUPLE3 (lft, AMPERSAND, rght) -> TRIPLE (rw' lft, P_AMPERSAND, rw' rght)
| TUPLE3 (lft, VBAR, rght) -> TRIPLE (rw' lft, P_VBAR, rw' rght)
| TUPLE3 (LPAR, arg, RPAR) -> rw' arg
| oth -> unhand := Some oth; failwith "rw'"

let rewrite arg =
  let p = parse arg in
  let rw = rw' p in
  rw

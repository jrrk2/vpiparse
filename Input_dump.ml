open Input_types

let rec dump = function
| Always (Edge deplst, body) -> "always @("^dumplst " or " deplst^") "^dumplst "; " body
| Class -> "/* Class */"
| Void n -> "Void"^string_of_int n
| Id _ -> failwith "Id"
| Alw _ -> failwith "Alw"
| Bin (s, w) -> string_of_int w^"'b"^s
| Oct (s, w) -> string_of_int w^"'o"^s
| Dec (s, w) -> string_of_int w^"'d"^s
| Hex (s, w) -> string_of_int w^"'h"^s
| If_ (c, a, b) -> "if ("^dump c^") "^dumplst "; " a^"; else "^dumplst "; " b
| Mux2 (_, _, _) -> failwith "Mux2"
| Always (_, _) -> failwith "Always"
| Asgn (a, b) -> dump a^" = "^dump b
| Block (_, _) -> failwith "Block"
| Concat (_, a, b) -> "{"^dump a^", "^dump b^"}"
| Selection (nam, lft, rght, _, _) -> dump nam^"["^string_of_int lft^":"^string_of_int rght^"] "
| Update (_, _, _, _, _) -> failwith "Update"
| Bitsel (a, b) -> dump a^"["^dump b^"]"
| Seq lst -> "begin "^dumplst "; " lst^"; end "
| Unary (_, _) -> failwith "Unary"
| Dyadic (_, _, _) -> failwith "Dyadic"
| Case (exp, lst) -> "case ("^dump exp^") "^dumplst "; " lst^" endcase"
| Item (_, n, stmt) -> "case "^dump n^": "^dump stmt
| Signed _ -> failwith "Signed"
| Unsigned _ -> failwith "Unsigned"
| Conpp _ -> failwith "Conpp"
| Inppp _ -> failwith "Inppp"
| Inpspp _ -> failwith "Inpspp"
| Sigpp _ -> failwith "Sigpp"
| Sigspp _ -> failwith "Sigspp"
| Alwpp _ -> failwith "Alwpp"
| Regpp (_, _, _, _) -> failwith "Regpp"
| Itmpp _ -> failwith "Itmpp"
| Wirepp _ -> failwith "Wirepp"
| Othpp _ -> failwith "Othpp"
| Ident s -> s
| Integer n -> string_of_int n
| Port s -> s
| Enum e -> "enum "^e
| Not _ -> failwith "Not"
| Lneg a -> "(~"^dump a^")"
| Aneg _ -> failwith "Aneg"
| Sneg _ -> failwith "Sneg"
| Mux (_, _) -> failwith "Mux"
| Add (a, b) -> "("^dump a^" + "^dump b^")"
| Sub (a, b) -> "("^dump a^" - "^dump b^")"
| And (a, b) -> "("^dump a^" & "^dump b^")"
| Or (a, b) -> "("^dump a^" | "^dump b^")"
| Xor (a, b) -> "("^dump a^" ^ "^dump b^")"
| LogAnd (a, b) -> "("^dump a^" && "^dump b^")"
| LogOr (a, b) -> "("^dump a^" || "^dump b^")"
| Assign (a, b) -> dump a^" <= "^dump b
| Partsel (_, _, _) -> failwith "Partsel"
| Lt (a, b) -> "("^dump a^" < "^dump b^")"
| Le (a, b) -> "("^dump a^" <= "^dump b^")"
| Eq (a, b) -> "("^dump a^" == "^dump b^")"
| Ne (a, b) -> "("^dump a^" != "^dump b^")"
| Ge (a, b) -> "("^dump a^" >= "^dump b^")"
| Gt (a, b) -> "("^dump a^" > "^dump b^")"
| LshiftL (a, b) -> "("^dump a^" << "^dump b^")"
| LshiftR (a, b) -> "("^dump a^" >> "^dump b^")"
| AshiftR (a, b) -> "("^dump a^" >>> "^dump b^")"
| Posedge e -> "(posedge "^dump e^") "
| Range (_, _) -> "Range"
| Place (n, _, _) -> "/* Place"^string_of_int n^" */"
| Array_var _ -> failwith "Array_var"
| Edge _ -> failwith "Edge"

and dumplst delim lst = String.concat delim (List.map dump lst)

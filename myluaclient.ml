let saved_args = ref []

module LuaChar = struct
    type 'a t	    = char
    let tname	    = "char"
    let eq _	    = fun x y -> x = y
    let to_string   = fun _ c -> String.make 1 c 
end

module Pair = struct
    type 'a t	    = 'a * 'a
    let tname	    = "pair"
    let eq _	    = fun x y -> x = y
    let to_string   = fun f (x,y) -> Printf.sprintf "(%s,%s)" (f x) (f y)
    let mk  x y     = (x,y)
    let fst         = fst
    let snd         = snd
end


module T =			    (* new types *)
    Lua.Lib.Combine.T3              (* T3 == link 3 modules *)
	(LuaChar)                   (* TV1 *)
	(Pair)                      (* TV2 *)    
	(Luaiolib.T)                (* TV3 *)

module LuaCharT     = T.TV1
module PairT        = T.TV2
module LuaioT       = T.TV3

module MakeLib 
    (CharV: Lua.Lib.TYPEVIEW with type 'a t        = 'a LuaChar.t)
    (PairV: Lua.Lib.TYPEVIEW with type 'a t        = 'a Pair.t
                             and  type 'a combined = 'a CharV.combined)
	: Lua.Lib.USERCODE with type 'a userdata' = 'a CharV.combined = struct
	
    type 'a userdata' = 'a PairV.combined
    module M (C: Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
	module V = C.V
	let ( **-> ) = V.( **-> )
	let ( **->> ) x y = x **-> V.result y
        module Map = struct
    let pair = PairV.makemap V.userdata V.projection
    let char = CharV.makemap V.userdata V.projection
end    

let init g = 

let wrap1 f a = try f a with e -> Printexc.print_backtrace stdout; raise e in
let wrap2 f a b = try f a b with e -> Printexc.print_backtrace stdout; raise e in

C.register_module "Pair"
    [ "mk", V.efunc (V.value **-> V.value **->> Map.pair) Pair.mk
    ; "fst",V.efunc (Map.pair **->> V.value)              Pair.fst
    ; "snd",V.efunc (Map.pair **->> V.value)              Pair.snd
    ] g;

    C.register_module "Char"
    [ "mk", V.efunc (V.string **->> Map.char)
            (function 
            | "" -> C.error "Char.mk: empty string"   
            | s  -> s.[0]
            )
    ] g;        
 
    C.register_module "Sys" [
    "arg", V.efunc (V.int **->> V.string) (fun ix -> List.nth !saved_args ix);
    "argv", V.efunc (V.unit **->> V.list V.string) (fun () -> !saved_args);
    "getenv", V.efunc (V.string **->> V.string) Sys.getenv;
    "test", V.efunc (V.string **-> V.string **->> V.string) (fun x y -> x^y);
    ] g;
 
    C.register_module "verible" [
    "tranlst", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.ltranlst);
    "cnvitm", V.efunc (V.string **-> V.string **->> V.string) (wrap2 Input_equiv_verible.lcnvitm);
    "mapitm", V.efunc (V.string **-> V.string **->> V.string) (wrap2 Input_equiv_verible.lmapitm);
    "satitm", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.lcnvsat);
    "cmpitm", V.efunc (V.string **-> V.string **->> V.string) (wrap2 Input_equiv_verible.lcmpitm);
    ] g;

    C.register_module "verilator" [
    "tran", V.efunc (V.string **-> V.string **->> V.unit) (wrap2 Input_equiv_verilator.tran);
    ] g;

    C.register_module "pipe" [
    "uhdm", V.efunc (V.string **-> V.string **->> V.unit) (wrap2 Input_equiv.tran);
    "rtlil", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.lrtlil);
    ] g;

    C.register_module "hardcaml" [
    "cnv", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.lhardcnv);
    ] g;

    C.register_module "itms" [
    "itm", V.efunc (V.unit **->> V.string) (wrap1 Input_equiv_verible.litms);
    "nam", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.lnam);
    "dump", V.efunc (V.string **-> V.string **->> V.unit) (wrap2 Input_equiv_verible.ldump);
    ] g;

    C.register_module "liberty" [
    "read", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.lreadlib);
    "dump", V.efunc (V.string **->> V.unit) (wrap1 Input_equiv_verible.ldumplib);
    ] g;

    C.register_module "external" [
    "eqv", V.efunc (V.string **->> V.string) (wrap1 Input_equiv_verible.eqv);
    "sta", V.efunc (V.string **-> V.string **-> V.string **->> V.unit) (wrap2 Input_equiv_verible.sta);
    ] g;

    end (* M *)
end (* MakeLib *)

module W = Lua.Lib.WithType (T)
module C  =
    Lua.Lib.Combine.C5  (* C5 == combine 4 code modules *)
	(Luaiolib.Make(LuaioT))
        (Luacamllib.Make(LuaioT))
	(W (Luastrlib.M))
	(W (Luamathlib.M))
	(MakeLib (LuaCharT) (PairT))


module I =			    (* interpreter *)  
    Lua.MakeInterp
	(Lua.Parser.MakeStandard)
	(Lua.MakeEval (T) (C))

let cmdline verbose eval itm =
  if verbose then print_endline ("argv: "^itm);
  let fd = open_in itm in
  let itm = ref "" in
  try while true do let s = input_line fd in itm := !itm ^ "\n" ^ s done with _ -> ();
  close_in fd;
  if verbose then print_endline ("eval: "^ !itm);
  try eval !itm with e -> print_endline (Printexc.to_string_default e)

let main args =
    saved_args := Sys.argv.(0) :: args;
    let verbose = try int_of_string (Sys.getenv ("LUA_CLIENT_VERBOSE")) > 0 with _ -> false in
    let state   = I.mk () in (* fresh Lua interpreter *)
    let eval e  = ignore (I.dostring state e) in
    (match args with hd::tl -> cmdline verbose eval hd | _ -> print_endline "Interactive mode");
    while true do
    print_string "toplevel> ";
    let itm = ref "" in
    let scan = ref true in
    while !scan do let s = read_line() in if s = "" then scan := false else itm := !itm ^ "\n" ^ s done;
    if verbose then print_endline ("eval: "^ !itm);
    try eval !itm with e -> print_string (Printexc.to_string_default e^"\n* "); done

let eargs = String.split_on_char ';' (try Sys.getenv "LUA_ARGS" with _ -> "")

let _ = if false then List.iter print_endline ("eargs: "::eargs)

let _ = try main (if eargs <> [] then eargs else List.tl (Array.to_list Sys.argv)) with End_of_file -> ()

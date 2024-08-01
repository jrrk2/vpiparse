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
 
    C.register_module "Example" 
    ["argv",   (V.list V.string).V.embed (Array.to_list Sys.argv);
     "getenv", V.efunc (V.string **->> V.string) Sys.getenv;
    ] g;
 
    C.register_module "verible" 
    ["tran", V.efunc (V.string **->> V.unit) Input_equiv_verible.tran;
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


let main args =
    let verbose = try int_of_string (Sys.getenv ("LUA_CLIENT_VERBOSE")) > 1 with _ -> false in
    let state   = I.mk () in (* fresh Lua interpreter *)
    let eval e  = ignore (I.dostring state e) in
    List.iter (fun itm -> if verbose then print_endline ("eval: "^itm); eval itm) args;
    while true do
    print_string "toplevel> ";
    let itm = ref "" in
    let scan = ref true in
    while !scan do let s = read_line() in if s = "" then scan := false else itm := !itm ^ "\n" ^ s done;
    if verbose then print_endline ("eval: "^ !itm);
    try eval !itm with e -> print_string (Printexc.to_string_default e^"\n* "); done

let _ = main [] (* was: (List.tl (Array.to_list Sys.argv)) *)
    


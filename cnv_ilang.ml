open Input_types
open Dump_types
open Rtlil_input_rewrite_types

let othportmap = ref None
let othinstmap = ref None
let othidxmap = ref None
let othvmap = ref None
let othdirmap = ref None
let othtypmap = ref None

let (portmap:rw->ilang) = function
| PORT ("", pin, dir,
		     [SEL ("",
		       [VRF (net, (BASDTYP, "wire", TYPNONE, []), []);
		       CNST (_, HEX idx); CNST (_, SHEX 1)])]) -> TokConn ([TokID ("\\"^pin)], [Sigspec90 (("\\"^net), idx)])
| PORT ("", pin, dir,
	    [VRF (net, (BASDTYP, "wire", TYPNONE, []), [])]) -> TokConn ([TokID ("\\"^pin)], [TokID ("\\"^net)])
| PORT ("", pin, dir, [CNST (w, HEX n)]) -> TokConn ([TokID ("\\"^pin)], [TokVal (string_of_int w^"'"^string_of_int n)])
| oth -> othportmap := Some oth; failwith "cnv_ilang20"

let instmap = function
| (inst,
         ("", kind, portlst)) ->
	      Cell_stmt (("$"^kind), inst,
      [], List.map (portmap) portlst)
| oth -> othinstmap := Some oth; failwith "cnv_ilang21"

let dirmap ix = function
| Dinput -> Wire_optionsinput (ix+1)
| Doutput -> Wire_optionsoutput (ix+1)
| oth -> othdirmap := Some oth; failwith "cnv_ilang31"

let idxmap ix = function
| (pin, ("", (BASDTYP, "wire", TYPRNG (HEX hi, HEX lo), []), dir, "wire", [])) ->
     Wire_stmt ([Wire_optionswidth (hi-lo+1); dirmap ix dir], ("\\"^pin))
| oth -> othidxmap := Some oth; failwith "cnv_ilang10"

let typmap = function
| TYPRNG (HEX hi, HEX lo) -> [Wire_optionswidth (hi-lo+1)]
| TYPNONE -> []
| oth -> othtypmap := Some oth; failwith "cnv_ilang39"

let vmap = function
    | (nam, ("", (BASDTYP, "wire", rng, []), "wire", (UNKDTYP, "", TYPNONE, []))) ->
     Wire_stmt (typmap rng, nam)
    | oth -> othvmap := Some oth; failwith "cnv_ilang7"

let cnv_ilang uitms =
("\\multiplier_test",
  Module12 ("\\multiplier_test", List.map (vmap) !(uitms.v) @
    List.mapi (idxmap) !(uitms.io) @
    List.map (instmap) !(uitms.inst)) :: [])

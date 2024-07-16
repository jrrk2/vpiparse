open Input
open Dump_types

type signal =
| Sig_of_sig of (int*int*bool)
| Sig_of_signed of signal
| Sig_of_const of token
| Signed_of_sig of signal
| Sig_of_var of var
| Sig_of_reg of (int*int*bool*attr)
| Sig_of_remap of remapp
| Primary of (int*int*bool)
| NOT of signal
| LNeg of signal
| ANeg of signal
| SNeg of signal
| MUX2 of signal * signal list
| SUB of signal * signal
| ADD of signal * signal
| AND of signal * signal
| OR of signal * signal
| XOR of signal * signal
| ASSIGN of signal * signal
| BITSEL of signal * int
| CONCAT of signal * signal
| PARTSEL of signal * int * int

and remapp =
  | Void of int
  | Id of remapp
  | Alw of Hardcaml.Always.t
  | Bin of string * int
  | Oct of string * int
  | Dec of string * int
  | Hex of string * int
  | If_ of remapp * remapp list * remapp list
  | Mux2 of remapp * remapp * remapp
  | Always of remapp * remapp list
  | Asgn of remapp * remapp
  | Block of remapp * remapp
  | Concat of remapp list
  | Selection of remapp * int * int * int * int
  | Update of remapp * int * int * int * int
  | Bitsel of remapp * remapp
  | Seq of remapp list
  | Unary of remapp * remapp
  | Dyadic of remapp * remapp * remapp
  | Case of remapp * remapp list
  | Item of remapp * remapp
  | Default of remapp
  | Signed
  | Unsigned
  | Conpp of token
  | Inppp of signal
  | Inpspp of signal
  | Sigpp of signal
  | Sigspp of signal
  | Alwpp of signal
  | Regpp of (int*int*bool*attr)
  | Itmpp of signal list
  | Wirepp of signal
  | Othpp of signal
  | Ident of string
  | Integer of int
  | Port of string
  | Enum of string
  | Negate
  | Class
  | Not
  | Lneg
  | Aneg
  | Sneg
  | Mux
  | Sub
  | Add of string
  | Mult
  | Mults
  | Div
  | Divs
  | Mod
  | Mods
  | Pow
  | Pows
  | And
  | Nand
  | Or
  | Nor
  | Xor
  | Xnor
  | LogNot
  | LogAnd
  | LogNand
  | LogOr
  | LogXor
  | LogNor
  | LogXnor
  | Assign
  | Partsel
  | Lt
  | Lts
  | Le
  | Les
  | Eq
  | Ne
  | Newild
  | Ge
  | Gewild
  | Ges
  | Gt
  | Gts
  | LshiftL
  | LshiftR
  | AshiftR
  | Edge of remapp list
  | Posedge of remapp
  | Range of remapp * remapp
  | Place of int * remapp * remapp
  | Array_var of string

type relational =
| Unsigned_relational of token
| Signed_relational of token
| Relation of relational * signal * signal
| Relations of relational * signal * signal
| Remap of remapp
| If_else of signal * signal list * signal list
| Signal of signal

type stage =
| FIRSTG
| IOSTG
| VARSTG
| JMPSTG
| BDYSTG

type typmap =
| TYPNONE
| SUBTYP of int
| TYPRNG of cexp*cexp
| TYPMEMBER of typetable_t
| TYPENUM of string * int * (int*cexp)
| TYPDEF
| RECTYP of typetable_t
| TYPSIGNED

and typetable_t = typenc*string*typmap*typmap list
and typ_t = typenc*string*typmap*rw list
and xmlattr = {
    anchor: string;
    names: (string*typetable_t ref) list ref;
    typetable: typetable_t array;
    intf: (string*string) list ref;
    instances: (string*(token*string)) list ref;
    modulexml: (string*(string*rw list*(string*typetable_t ref) list)) list ref;
    tmpvar: (string*(string*typetable_t)) list ref;
    tmpasgn: (string*rw) list ref;
    }
and rw =
| UNKNOWN
| XML of rw list
| EITM of string * string * string * int * rw list
| IO of string * string list * typetable_t * dirop * string * rw list
| VAR of string * string list * typetable_t * string
| IVAR of string * string * typetable_t * rw list * int
| CNST of (int * cexp)
| VRF of string * typetable_t * rw list
| TYP of int * typ_t
| FNC of string * string * typetable_t * rw list
| TASKDEF of string * string * rw list
| TASKRF of string * string * rw list
| INST of string * token * string list * (string * rw list)
| SFMT of string * rw list
| SYS of string * string * rw list
| TPLSRGS of string * string * int * rw list
| VPLSRGS of string * int * rw list
| PORT of string * string * dirop * rw list
| CA of string * rw list
| UNRY of unaryop * rw list
| SEL of string * rw list
| ASEL of rw list
| SNITM of string * rw list
| ASGN of bool * string * rw list
| ARITH of arithop * rw list
| LOGIC of logop * rw list
| CMP of cmpop * rw list
| FRF of string * string * rw list
| XRF of string * string * string * string * dirop
| PKG of string * string * rw list
| CAT of string * rw list
| CPS of string * rw list
| CND of string * rw list
| TIM of string
| REPL of string * int * rw list
| MODUL of string * string * rw list * (string * (string * typetable_t)) list
| BGN of string option * rw list
| RNG of rw list
| ALWYS of string * rw list
| SNTRE of rw list
| IF of string * rw list
| INIT of string * string * rw list
| IRNG of string * rw list
| IFC of string * string * rw list
| IMP of string * string * rw list
| IMRF of string * string * dirop * rw list
| JMPL of string * rw list
| JMPG of string * rw list
| JMPBLK of string * rw list
| CS of string * rw list
| CSITM of string * rw list
| WHL of rw list
| FORSTMT of (string * string * cmpop * rw * (int * cexp) * (int * cexp) * (int * cexp) * rw list)
| ARG of rw list
| DSPLY of string * string * rw list
| FILS of string * rw list
| FIL of string * string
| NTL of rw list
| CELLS of rw list * xmlattr
| CELL of string * string * string * string * rw list
| POSPOS of string*string
| POSNEG of string*string
| NEGNEG of string*string
| POSEDGE of string
| NEGEDGE of string
| COMB
| MODPORTFTR of string * string
| TYPETABLE of typetable_t array
| SCOPE of string
| ITM of string * string * rw list
| CONSPACK of string * rw list
| CONSPACKMEMB of string * rw list
| CONTAINER of itms * rw

and itms = { 
  io: (string*(string*typetable_t*dirop*string*(int*cexp) list)) list ref;
  v: (string*(string*typetable_t*string*typetable_t)) list ref;
  iv: (string*(string*typetable_t*rw list*int)) list ref;
  ir: (string*string*typetable_t) list ref;
  ca: (string*rw*rw) list ref;
  alwys: (string*rw*rw list) list ref;
  init: (string*token*rw list) list ref;
  func: (string*(string*typetable_t*rw list*itms)) list ref;
  task: (string*(string*rw list*itms)) list ref;
  gen: (string*rw list) list ref;
  imp: (string*(string*(string*dirop) list)) list ref;
  inst: (string*(string*string*rw list)) list ref;
  cnst: (string*(bool*(int*cexp))) list ref;
  needed: (token*string) list ref;
  remove_interfaces: bool;
  mode: string;
  names'': (string * typetable_t ref) list;
}

type remap =
  | Invalid
  | Con of Hardcaml.Constant.t
  | Sig of Hardcaml.Signal.t
  | Sigs of Hardcaml.Signal.Signed.v
  | Alw of Hardcaml.Always.t
  | Var of Hardcaml.Always.Variable.t
  | Itm of (Hardcaml.Signal.t * Hardcaml.Always.t list)

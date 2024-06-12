open Input

type attr = {
  clock: string option;
  reset: string option;
  enable: string option;
}

type var =
| Var_of_reg of (int*int*bool*attr)

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
  | Concat of token * remapp * remapp
  | Selection of remapp * int * int * int * int
  | Update of remapp * int * int * int * int
  | Bitsel of remapp * remapp
  | Seq of remapp list
  | Unary of token * remapp
  | Dyadic of token * remapp * remapp
  | Case of remapp * remapp list
  | Item of remapp * remapp * remapp
  | Signed of remapp
  | Unsigned of remapp
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
  | Class
  | Not of remapp
  | Lneg of remapp
  | Aneg of remapp
  | Sneg of remapp
  | Mux of remapp * remapp list
  | Sub of remapp * remapp
  | Add of remapp * remapp
  | And of remapp * remapp
  | Or of remapp * remapp
  | Xor of remapp * remapp
  | LogAnd of remapp * remapp
  | LogOr of remapp * remapp
  | Assign of remapp * remapp
  | Partsel of remapp * int * int
  | Lt of remapp * remapp
  | Le of remapp * remapp
  | Eq of remapp * remapp
  | Ne of remapp * remapp
  | Ge of remapp * remapp
  | Gt of remapp * remapp
  | LshiftL of remapp * remapp
  | LshiftR of remapp * remapp
  | AshiftR of remapp * remapp
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

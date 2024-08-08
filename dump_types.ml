
type attr = {
  r_sync: Hardcaml__.Reg_spec.t option;
  enable: Hardcaml.Signal.t;
  dest: bool;
}

type var =
| Var_of_reg of (int*int*bool*attr)

type unaryop =
| Unknown
| Unot
| Ulognot
| Unegate
| Uunsigned
| Usigned
| Uextend of (int*int)
| Uextends of (string*int*int) [@@deriving yojson]

type cmpop =
| Cunknown
| Ceq
| Cneq
| Cgt
| Cgts
| Cgte
| Cgtes
| Ceqwild
| Cneqwild
| Cltes
| Clte
| Clt
| Clts [@@deriving yojson]

type logop =
| Lunknown
| Land
| Lredand
| Lrednand
| Lor
| Lredor
| Lrednor
| Lxor
| Lxnor
| Lredxor
| Lredxnor
| Lshiftl
| Lshiftr
| Lshiftrs [@@deriving yojson]

type arithop =
| Aadd of string
| Asub of string
| Amul of string
| Amuls of string
| Adiv
| Adivs
| Amod
| Amods
| Apow
| Apows
| Astring
| Aenum
| Asyscall [@@deriving yojson]

type dirop = 
| Dunknown
| Dinput
| Doutput
| Dinout
| Dvif of string ref
| Dinam of string
| Dport of (string * int * dirop * string * string list) [@@deriving yojson]

type typenc =
| UNKDTYP
| PACKADTYP
| UNPACKADTYP
| CNSTDTYP
| BASDTYP
| STRDTYP
| UNIDTYP
| REFDTYP
| ENUMDTYP
| MEMBDTYP
| PARMTDTYP
| IFCRFDTYP of string
| TYPDF of string [@@deriving yojson]

type arrtyp =
| UNKARR
| BIT
| REG
| WIRE
| REAL
| STR
| ARNG of (int*int)
| PACKED of (int*int)
| UNPACKED of (int*int)
| ADD of arrtyp list
| MAX of arrtyp list
| MEMBER of arrtyp list
| VECTOR of (cexp * cexp) [@@deriving yojson]

and cexp =
| ERR of string
| BIN of char
| HEX of int
| SHEX of int
| STRING of string
| ENUMVAL of int * string
| FLT of float
| BIGINT of Int64.t
| CNSTEXP of arithop * cexp list [@@deriving yojson]

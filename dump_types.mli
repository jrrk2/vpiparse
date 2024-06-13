
type attr = {
  clock: string option;
  reset: string option;
  enable: string option;
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
| Uextends of (string*int*int)

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
| Clts

type logop =
| Lunknown
| Land
| Lredand
| Lor
| Lredor
| Lxor
| Lxnor
| Lredxor
| Lredxnor
| Lshiftl
| Lshiftr
| Lshiftrs

type arithop =
| Aunknown
| Aadd
| Asub
| Amul
| Amuls

type dirop = 
| Dunknown
| Dinput
| Doutput
| Dinout
| Dvif of string ref
| Dinam of string
| Dport of (string * int * dirop * string * string list)

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
| TYPDF of string

type arrtyp =
| UNKARR
| BIT
| REAL
| STRING
| ARNG of (int*int)
| PACKED of (int*int)
| UNPACKED of (int*int)
| ADD of arrtyp list
| MAX of arrtyp list
| MEMBER of arrtyp list

type cexp =
| ERR of string
| BIN of char
| HEX of int
| SHEX of int
| STRING of string
| FLT of float
| BIGINT of Int64.t

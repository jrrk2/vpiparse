%{
  open Parsing

  let optype x = STRING (Vpi_types.vpi_expr (int_of_string x))

  let direction x = STRING (Vpi_types.vpi_port_net (int_of_string x))

  let net_type x = STRING (Vpi_types.vpi_port_net (int_of_string x))
  
  let rec to_list kind = function
    | Vpiparent :: tl -> to_list kind tl
    | oth -> TUPLE2(kind, TLIST oth)

  let rec to_tuple kind = function
    | [] -> kind
    | Vpiparent :: tl -> to_tuple kind tl
    | arg::[] -> TUPLE2(kind, arg)
    | arg1::arg2::[] -> TUPLE3(kind, arg1, arg2)
    | arg1::arg2::arg3::[] -> TUPLE4(kind, arg1, arg2, arg3)
    | arg1::arg2::arg3::arg4::[] -> TUPLE5(kind, arg1, arg2, arg3, arg4)
    | arg1::arg2::arg3::arg4::arg5::[] -> TUPLE6(kind, arg1, arg2, arg3, arg4, arg5)
    | arg1::arg2::arg3::arg4::arg5::arg6::[] ->
    TUPLE7(kind, arg1, arg2, arg3, arg4, arg5, arg6)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::[] ->
    TUPLE8(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::[] ->
    TUPLE9(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::[] ->
    TUPLE10(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::[] ->
    TUPLE11(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::[] ->
    TUPLE12(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::[] ->
    TUPLE13(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::[] ->
    TUPLE14(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::[] ->
    TUPLE15(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::arg15::[] ->
    TUPLE16(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::arg15::arg16::[] ->
    TUPLE17(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
    | oth -> print_endline ("kind:"^string_of_int (List.length oth)); TUPLE2(kind, TLIST oth)
%}

%token  Indent
%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  HYPHEN
%token  BACKQUOTE
%token  BACKSLASH
%token  SLASH
%token  CARET
%token  COLON
%token  COMMA
%token  LPAREN
%token  RPAREN
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR
%token  ERROR_TOKEN
%token  GREATER
%token  HASH
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  PERCENT
%token  PLING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token <string list> SLIST
%token <string> STRING
%token <string> STRING_CONST
%token  TILDE
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE15
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE16
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE17
%token <int> Vpimethodint
%token  UNDERSCORE
%token  VBAR
%token FINISHED
%token INT
%token <Int64.t> HEX
%token <string> BIN
%token KILLED
%token Post_Elab
%token Pre_Elab
%token RUNNING
%token Restored
%token SUSPENDED
%token UINT
%token WAITING
%token Always
%token Array_net
%token Assignment
%token Attribute
%token Begin
%token Bit_select
%token Case_item
%token Case_stmt
%token Class_defn
%token Class_typespec
%token Class_var
%token Constant
%token Cont_assign
%token DESIGN
%token Design
%token Enum_const
%token Enum_typespec
%token Enum_var
%token Event_control
%token For_stmt
%token Function
%token Gen_if_else
%token Gen_region
%token Gen_scope
%token Gen_scope_array
%token If_else
%token If_stmt
%token Indexed_part_select
%token Initial
%token Int_typespec
%token Int_var
%token Integer_typespec
%token Integer_var
%token Io_decl
%token Logic_net
%token Logic_typespec
%token Logic_var
%token Module_inst
%token Operation
%token Package
%token Param_assign
%token Parameter
%token Part_select
%token Port
%token Range
%token Ref_module
%token Ref_obj
%token Ref_typespec
%token Ref_var
%token Sys_func_call
%token Task
%token Task_call
%token Var_select
%token Weaklyreferenced
%token Any_sverilog_class
%token Array
%token Await
%token Bound
%token Builtin
%token Clk
%token Endln
%token File
%token Get
%token Keycount
%token Kill
%token Line
%token Mailbox
%token Message
%token New
%token Num
%token Peek
%token Process
%token Put
%token Queue
%token Resume
%token Self
%token Semaphore
%token State
%token Status
%token String
%token Suspend
%token System
%token Try_get
%token Try_peek
%token Try_put
%token Uhdmallclasses
%token Uhdmallmodules
%token Uhdmallpackages
%token Uhdmtopmodules
%token Uhdmtoppackages
%token Vpiactual
%token Vpialwaystype
%token Vpiargument
%token Vpiarraynet
%token Vpiattribute
%token Vpibaseexpr
%token Vpiblocking
%token Vpicaseitem
%token Vpicasetype
%token Vpiclassdefn
%token Vpicondition
%token Vpiconstantselect
%token Vpiconsttype
%token Vpicontassign
%token Vpidecompile
%token Vpideffile
%token Vpideflineno
%token Vpidefname
%token Vpidirection
%token Vpielaborated
%token Vpielsestmt
%token Vpienumconst
%token Vpiexpr
%token Vpiforincstmt
%token Vpiforinitstmt
%token Vpifullname
%token Vpigenscope
%token Vpigenscopearray
%token Vpigenstmt
%token Vpihighconn
%token Vpiindex
%token Vpiindexedpartselecttype
%token Vpiinstance
%token Vpiiodecl
%token Vpileftrange
%token Vpilhs
%token Vpilocalparam
%token Vpilowconn
%token Vpimethod
%token Vpimodule
%token Vpiname
%token Vpinet
%token Vpinetdeclassign
%token Vpinettype
%token Vpioperand
%token Vpioptype
%token Vpioverriden
%token Vpiparamassign
%token Vpiparameter
%token Vpiparent
%token Vpiport
%token Vpiprocess
%token Vpirange
%token Vpirefmodule
%token Vpireturn
%token Vpirhs
%token Vpirightrange
%token Vpisigned
%token Vpisize
%token Vpistmt
%token Vpitask
%token Vpitaskfunc
%token Vpitop
%token Vpitopmodule
%token Vpitypedef
%token Vpitypespec
%token Vpivariables
%token Vpivisibility
%token Vpiwidthexpr

%token Work
%token <string> VpiNum 
%type <token list> ml_start
%type <token> always_typ
%start ml_start
%%

ml_start: input_lst EOF_TOKEN { $1 }

package_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpiclassdefn COLON class_intro { TUPLE2(Vpiclassdefn, $3) }

class_intro: Class_defn COLON class_def Indent class_lst { to_tuple Class_defn $5 }

parent:
  | Package COLON Builtin LPAREN Builtin COLON COLON RPAREN { Package }
  | Design COLON LPAREN Work AT STRING RPAREN { Design }
  | Class_defn COLON class_def { Class_defn }
  | Function COLON function_def { Function }
  | Task COLON task_def { Task }
  | Class_var COLON class_var_def { Class_var }
  | Int_var COLON int_var_def { Int_var }
  | Integer_var COLON integer_var_def { Int_var }
  | Logic_var COLON logic_var_def { Logic_var }
  | Io_decl COLON io_decl_def { Io_decl }
  | Enum_var COLON enum_var_def { Enum_var }
  | Module_inst COLON module_inst_def { Module_inst }
  | Logic_net COLON logic_net_def { Logic_net }
  | Port COLON port_def { Port }
  | Always COLON always_def { Always }
  | Event_control COLON event_control_def { Event_control }
  | Operation COLON operation_def { Operation }
  | Begin COLON begin_def { TUPLE2(Begin, $3) }
  | Assignment COLON assignment_def { Assignment }
  | Var_select COLON var_select_def { Var_select }
  | Range COLON range_def { Range }
  | Parameter parameter_def { Parameter }
  | Param_assign COLON loc { Param_assign }
  | Constant COLON constant_def { Constant }
  | Initial COLON initial_def { Initial }
  | If_stmt COLON loc { If_stmt }
  | If_else COLON loc { If_else }
  | For_stmt COLON for_stmt_def { For_stmt }
  | Bit_select COLON bit_sel_def { Bit_select }
  | Case_stmt COLON loc { Case_stmt }
  | Case_item COLON loc { Case_item }
  | Sys_func_call COLON sys_func_call_def { Sys_func_call }
  | Cont_assign COLON loc { Cont_assign }
  | Gen_region COLON loc { Gen_region }
  | Gen_if_else COLON loc { Gen_if_else }
  | Ref_module COLON ref_module_def { Ref_module }
  | Indexed_part_select COLON indexed_part_select_def { Indexed_part_select }
  | Array_net COLON array_net_def { Array_net }
  | Gen_scope_array COLON gen_scope_array_def { Gen_scope_array }
  | Gen_scope COLON gen_scope_array_def { Gen_scope_array }
  | Part_select COLON part_select_def { Part_select }
  | Ref_typespec COLON type_spec { TUPLE2(Ref_typespec, $3) }
  | ref_typespec_actual { $1 }
  
loc: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

ref_module_def:
  | Work AT STRING LPAREN name RPAREN loc { Work }

class_var_def:
  | LPAREN Work AT name type_lst RPAREN loc { Work }

class_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { TUPLE2(Vpiname, $5) }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethod COLON vpi_method_arg { TUPLE2(Vpimethod, $3) }
  | Vpitypedef COLON typespec { TUPLE2(Vpitypedef, $3) }

typespec:
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst { to_tuple  Enum_typespec $5 }

enum_typespec_lst: { [] }
  | enum_typespec_opt enum_typespec_lst { $1 :: $2 }

enum_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpienumconst COLON { Vpienumconst }
  | Enum_const COLON enum_const_decl Indent enum_constant_lst { to_tuple Enum_const $5 }

parameter_lst: { [] }
  | parameter_opt parameter_lst { $1 :: $2 }

parameter_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidecompile COLON VpiNum { Vpidecompile }
  | Vpisize COLON VpiNum { Vpisize }
  | UINT COLON VpiNum { UINT }
  | INT COLON VpiNum { INT }
  | HEX { HEX $1 }
  | BIN { BIN $1 }
  | Vpisigned COLON VpiNum { Vpisigned }
  | Vpilocalparam COLON VpiNum { Vpilocalparam }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }

enum_constant_lst: { [] }
  | enum_constant_opt enum_constant_lst { $1 :: $2 }

enum_constant_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpidecompile COLON VpiNum { Vpidecompile }
  | Vpisize COLON VpiNum { Vpisize }
  | UINT COLON VpiNum { UINT }
  | INT COLON VpiNum { INT }

vpi_method_arg:
  | Function COLON function_def Indent function_lst { Function }
  | Task COLON task_def Indent task_lst { Task }
  | Function COLON function_def { Function }
  | Task COLON task_def { Task }

class_var_lst: { [] }
  | class_var_opt class_var_lst { $1 :: $2 }

class_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }

enum_var_lst: { [] }
  | enum_var_opt enum_var_lst { $1 :: $2 }

enum_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

int_var_lst: { [] }
  | int_var_opt int_var_lst { $1 :: $2 }

int_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON ref_typespec { Vpitypespec }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }

int_typespec_lst: { [] }
  | int_typespec_opt int_typespec_lst { $1 :: $2 }

int_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpitypespec COLON ref_typespec { Vpitypespec }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }

integer_typespec_lst: { [] }
  | integer_typespec_opt integer_typespec_lst { $1 :: $2 }

integer_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpitypespec COLON ref_typespec { Vpitypespec }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }

integer_var_lst: { [] }
  | integer_var_opt integer_var_lst { $1 :: $2 }

integer_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON ref_typespec { Vpitypespec }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }

logic_var_lst: { [] }
  | logic_var_opt logic_var_lst { $1 :: $2 }

logic_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }

ref_typespec:
  | Ref_typespec COLON type_spec Indent ref_typespec_lst { to_tuple Ref_typespec ($3::$5) }

ref_typespec_lst: { [] }
  | ref_typespec_opt ref_typespec_lst { $1 :: $2 }

ref_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_typespec_actual { TUPLE2(Vpiactual, $3) }

ref_typespec_actual:
  | Class_typespec COLON class_typespec { Class_typespec }
  | Int_typespec COLON int_typespec_def { Int_typespec }
  | Integer_typespec COLON integer_typespec_def { Integer_typespec }
  | Enum_typespec COLON enum_typespec_decl { Enum_typespec }
  | Logic_typespec COLON logic_typespec_def { Logic_typespec }
  
function_lst: { [] }
  | function_opt function_lst { $1 :: $2 }

function_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpiclassdefn COLON class_intro { Vpiclassdefn }
  | Vpireturn COLON ret { TUPLE2(Vpireturn, $3) }
  | Vpiiodecl COLON { Vpiiodecl }
  | Io_decl COLON io_decl_def Indent io_decl_lst { to_tuple Io_decl $5 }
  
task_lst: { [] }
  | task_opt task_lst { $1 :: $2 }

task_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiiodecl COLON { Vpiiodecl }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }
  | Io_decl COLON io_decl_def Indent io_decl_lst { to_tuple Io_decl $5 }
  
task_call_lst: { [] }
  | task_call_opt task_call_lst { $1 :: $2 }

task_call_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitask COLON Task COLON task_def { Vpitask }

io_decl_lst: { [] }
  | io_decl_opt io_decl_lst { $1 :: $2 }

io_decl_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpidirection COLON VpiNum { TUPLE2(Vpidirection, direction $3) }
  | Vpilowconn COLON oexpr { TUPLE2(Vpilowconn,$3) }
  | Vpihighconn COLON oexpr { TUPLE2(Vpihighconn,$3) }
  | Vpiexpr COLON oexpr { TUPLE2(Vpiexpr,$3) }
  | Vpitypedef COLON ref_typespec { $3 }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }

bit_sel_lst: { [] }
  | bit_sel_opt bit_sel_lst { $1 :: $2 }

bit_sel_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiindex COLON oexpr { TUPLE2(Vpiindex, $3) }

ref_var_lst: { [] }
  | ref_var_opt ref_var_lst { $1 :: $2 }

ref_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }

ref_obj_lst: { [] }
  | ref_obj_opt ref_obj_lst { $1 :: $2 }

ref_obj_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_obj_actual { TUPLE2(Vpiactual, $3) }

ref_obj_actual:
  | Logic_net COLON logic_net_def { Logic_net }
  | Part_select COLON part_select_def { Part_select }

constant_lst: { [] }
  | constant_opt constant_lst { $1 :: $2 }

constant_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpidecompile COLON const_decomp { TUPLE2(Vpidecompile, $3) }
  | Vpisize COLON VpiNum { Vpisize }
  | UINT COLON VpiNum { UINT }
  | INT COLON VpiNum { INT }
  | STRING_CONST { STRING_CONST $1 }
  | HEX { HEX $1 }
  | BIN { BIN $1 }
  | Vpiconsttype COLON VpiNum { Vpiconsttype }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

const_decomp:
  | VpiNum { VpiNum $1 }
  | STRING { STRING $1 }

package_lst: { [] }
  | package_opt package_lst { $1 :: $2 }

class_lst: { [] }
  | class_opt class_lst { $1 :: $2 }

packlst: { [] }
  | pack packlst { $1 :: $2 }

pack: Package COLON Builtin LPAREN Builtin COLON COLON RPAREN Indent package_lst { to_tuple Package $10 }

module_inst:
  | Module_inst COLON module_inst_def Indent module_inst_lst { to_list Module_inst $5 }

modlst: { [] }
  | module_inst modlst { $1 :: $2 }

elab: { TUPLE2(Vpielaborated, VpiNum "0") }
  | Vpielaborated COLON VpiNum { TUPLE2(Vpielaborated, VpiNum $3) }

modnam:
  | Vpiname COLON Work AT name { Vpiname }

input:
  | path COLON { Work }
  | DESIGN COLON LPAREN Work AT STRING RPAREN elab modnam { TUPLE4(DESIGN, STRING $6, $8, $9) }
  | Uhdmallclasses COLON class_intro { TUPLE2(Uhdmallclasses, $3) }
  | Uhdmallpackages COLON packlst { to_tuple Uhdmallpackages $3 }
  | Uhdmtopmodules COLON modlst { to_tuple Uhdmtopmodules $3 }
  | Uhdmtoppackages COLON packlst { to_tuple Uhdmtoppackages $3 }
  | Uhdmallmodules COLON modlst { to_tuple Uhdmallmodules $3 }
  | Weaklyreferenced COLON weak_lst { to_tuple Weaklyreferenced $3 }

weak_lst: { [] }
  | weak_opt weak_lst { $1 :: $2 }

weak_opt:
  | Class_typespec COLON class_typespec Indent Vpiclassdefn COLON Class_defn COLON class_def { Class_typespec }
  | Int_typespec COLON int_typespec_def Indent int_typespec_lst { Int_typespec }
  | Integer_typespec COLON integer_typespec_def Indent integer_typespec_lst { Integer_typespec }
  | Logic_typespec COLON logic_typespec_def { Logic_typespec }
  | Logic_typespec COLON logic_typespec_def Indent misc_lst { Logic_typespec }
  | Function COLON function_def Indent weak_function_lst { Function }
  | Task COLON task_def Indent task_lst { Task }
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst { Enum_typespec }
  | ref_typespec_actual { $1 }
  
weak_function_lst: { [] }
  | weak_function_opt weak_function_lst { $1 :: $2 }

weak_function_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpiclassdefn COLON class_intro { Vpiclassdefn }
  | Vpireturn COLON parent { Vpireturn }
  | Vpiiodecl COLON { Vpiiodecl }
  | Io_decl COLON io_decl_def Indent io_decl_lst { to_tuple Io_decl $5 }
  
misc_lst: { [] }
  | misc misc_lst { $1 :: $2 }

misc:
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpiparent COLON parent { Vpiparent }
  
range_lst: { [] }
  | range_opt range_lst { $1 :: $2 }

range_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpileftrange COLON oexpr  { Vpileftrange }
  | Vpirightrange COLON oexpr { Vpirightrange }

module_inst_lst: { [] }
  | module_inst_opt module_inst_lst { $1 :: $2 }

module_inst_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpideffile COLON path { Vpideffile }
  | Vpideflineno COLON VpiNum { TUPLE2(Vpideflineno, VpiNum $3) }
  | Vpivariables COLON ret { TUPLE2(Vpivariables,$3) }  
  | Vpinet COLON nettyp { TUPLE2(Vpinet, $3) }
  | Vpiarraynet COLON arraynettyp { TUPLE2(Vpinet, $3) }
  | Vpiport COLON vport { TUPLE2(Vpiport, $3) }
  | Vpiprocess COLON process { TUPLE2(Vpiprocess, $3) }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpitopmodule COLON VpiNum { TUPLE2(Vpitopmodule, VpiNum $3) }
  | Vpiparameter COLON Parameter parameter_def Indent parameter_lst { Vpiparameter }
  | Vpiparamassign COLON Param_assign COLON loc Indent param_assign_lst { Vpiparamassign }
  | Vpicontassign COLON cont_assign { TUPLE2(Vpicontassign, $3) }
  | Vpitaskfunc COLON vpi_method_arg { TUPLE2(Vpitaskfunc, $3) }
  | Vpigenstmt COLON Gen_region COLON Indent gen_region_lst { to_tuple Vpigenstmt $6 }
  | Vpirefmodule COLON stmt { Vpirefmodule }
  | Vpimodule COLON module_inst { TUPLE2(Vpimodule, $3) }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }
  | Vpigenscopearray COLON gen_scope_array { TUPLE2(Vpigenscopearray, $3) }

gen_scope_array:
  | Gen_scope_array COLON gen_scope_array_def Indent gen_scope_array_lst { to_tuple Gen_scope_array $5 }

gen_scope_array_lst: { [] }
  | gen_scope_array_opt gen_scope_array_lst { $1 :: $2 }

gen_scope_array_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpigenscope COLON gen_scope { $3 }

gen_scope:
  | Gen_scope COLON gen_scope_def Indent gen_scope_lst { Gen_scope }

gen_scope_lst: { [] }
  | gen_scope_opt gen_scope_lst { $1 :: $2 }

gen_scope_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpigenscope COLON gen_scope { $3 }
  | Vpisize COLON VpiNum { Vpisize }
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpinettype COLON VpiNum { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

cont_assign:
  | Cont_assign COLON loc Indent cont_assign_lst { to_tuple Cont_assign $5 }
  | Cont_assign COLON loc { Cont_assign }
  
gen_region_lst: { [] }
  | gen_region_opt gen_region_lst { $1 :: $2 }

gen_region_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpistmt COLON stmt { $3 }

parameter_def: COLON LPAREN Work AT pth_lst RPAREN loc { Work }

process:
  | Always COLON always_def Indent always_lst always_typ { to_tuple (TUPLE2(Always,$6)) $5 }
  | Initial COLON initial_def Indent initial_lst { to_tuple Initial $5 }
  | Always COLON always_def { Always }
  | Initial COLON initial_def { Initial }

always_typ:
  | Vpialwaystype COLON VpiNum { TUPLE2(Vpialwaystype, VpiNum $3) }

always_lst: { [] }
  | always_opt always_lst { $1 :: $2 }

always_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

initial_typ:
  | Vpialwaystype COLON VpiNum {  TUPLE2(Vpialwaystype, VpiNum $3) }

initial_lst: { [] }
  | initial_opt initial_lst { $1 :: $2 }

initial_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

attr:
  | Attribute COLON attribute_def Indent attribute_lst { to_tuple Attribute $5 }

attribute_lst: { [] }
  | attribute_opt attribute_lst { $1 :: $2 }

attribute_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }

stmt:
  | Event_control COLON event_control_def Indent event_control_lst { to_tuple Event_control $5 }
  | Begin COLON begin_def Indent begin_lst { to_tuple Begin ($3 :: $5) }
  | Assignment COLON assignment_def Indent assignment_lst { to_tuple Assignment $5 }
  | If_stmt COLON loc Indent if_stmt_lst { to_tuple If_stmt $5 }
  | If_else COLON loc Indent if_else_lst { to_tuple If_else $5 }
  | For_stmt COLON for_stmt_def Indent for_stmt_lst { to_tuple For_stmt $5 }
  | Case_stmt COLON loc Indent case_stmt_lst { to_tuple Case_stmt $5 }
  | Case_item COLON loc Indent case_item_lst { to_tuple Case_item $5 }
  | Task_call COLON task_call_def Indent task_call_lst { to_tuple Case_item $5 }
  | Gen_if_else COLON loc Indent gen_if_else_lst { to_tuple If_else $5 }
  | Ref_module COLON ref_module_def Indent ref_module_lst { to_tuple Ref_module $5 }
  | Cont_assign COLON loc Indent cont_assign_lst { to_tuple Cont_assign $5 }
  | Always COLON always_def Indent always_lst always_typ { to_tuple (TUPLE2(Always,$6)) $5 }
  
ref_module_lst: { [] }
  | ref_module_opt ref_module_lst { $1 :: $2 }

ref_module_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiport COLON vport { TUPLE2(Vpiport, $3) }
  | Vpiparameter COLON Parameter parameter_def Indent parameter_lst { Vpiparameter }
  | Vpiparamassign COLON Param_assign COLON loc Indent param_assign_lst { Vpiparamassign }
  | Vpiactual COLON Module_inst COLON module_inst_def { Vpiactual }

case_item_lst: { [] }
  | case_item_opt case_item_lst { $1 :: $2 }

case_item_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiexpr COLON oexpr { TUPLE2(Vpiexpr,$3) }

case_stmt_lst: { [] }
  | case_stmt_opt case_stmt_lst { $1 :: $2 }

case_stmt_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpicaseitem COLON stmt { TUPLE2(Vpicaseitem, $3) }
  | Vpiattribute COLON attr { TUPLE2(Vpicaseitem, $3) }
  | Vpicasetype COLON VpiNum { TUPLE2(Vpicasetype, VpiNum $3) }

for_stmt_lst: { [] }
  | for_stmt_opt for_stmt_lst { $1 :: $2 }

for_stmt_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpiforinitstmt COLON stmt { TUPLE2(Vpiforinitstmt, $3) }
  | Vpiforincstmt COLON stmt { TUPLE2(Vpiforincstmt, $3) }

if_stmt_lst: { [] }
  | if_stmt_opt if_stmt_lst { $1 :: $2 }

if_stmt_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }

if_else_lst: { [] }
  | if_else_opt if_else_lst { $1 :: $2 }

if_else_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpielsestmt COLON stmt { TUPLE2(Vpielsestmt, $3) }

gen_if_else_lst: { [] }
  | gen_if_else_opt gen_if_else_lst { $1 :: $2 }

gen_if_else_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpielsestmt COLON stmt { TUPLE2(Vpielsestmt, $3) }

begin_lst: { [] }
  | begin_opt begin_lst { $1 :: $2 }

begin_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

assignment_lst: { [] }
  | assignment_opt assignment_lst { $1 :: $2 }

assignment_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, optype $3) }
  | Vpiblocking COLON VpiNum { TUPLE2(Vpiblocking, VpiNum $3) }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON oexpr { TUPLE2(Vpilhs, $3) }

cont_assign_lst: { [] }
  | cont_assign_opt cont_assign_lst { $1 :: $2 }

cont_assign_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }
  | Vpinetdeclassign COLON VpiNum { TUPLE2(Vpinetdeclassign, VpiNum $3) }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, optype $3) }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON oexpr { TUPLE2(Vpilhs, $3) }

param_assign_lst: { [] }
  | param_assign_opt param_assign_lst { $1 :: $2 }

param_assign_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, optype $3) }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON lhs { TUPLE2(Vpilhs, $3) }
  | Vpioverriden COLON VpiNum { TUPLE2(Vpioverriden, VpiNum $3) }

lhs:
  | Parameter parameter_def { Parameter }

rhs:
  | Var_select COLON var_select_def Indent var_select_lst { to_tuple Var_select $5 }
  | Constant COLON constant_def Indent constant_lst { to_tuple Constant $5 }
  | oexpr { $1 }

var_select_lst: { [] }
  | var_select_opt var_select_lst { $1 :: $2 }

var_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpiindex COLON oexpr { TUPLE2(Vpiindex, $3) }
  | Vpiactual COLON parent { TUPLE2(Vpiactual, $3) }

event_control_lst: { [] }
  | event_control_opt event_control_lst { $1 :: $2 }

event_control_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }

oexpr:
  | Operation COLON operation_def Indent operation_lst { to_tuple Operation $5 }
  | Ref_obj COLON ref_obj_def Indent ref_obj_lst { to_tuple Ref_obj $5 }
  | Ref_var COLON ref_var_def Indent ref_var_lst { to_tuple Ref_var $5 }
  | Constant COLON constant_def Indent constant_lst { to_tuple Constant $5 }
  | Part_select COLON part_select_def Indent part_select_lst { to_tuple Part_select $5 }
  | Indexed_part_select COLON indexed_part_select_def Indent indexed_part_select_lst { to_tuple Indexed_part_select $5 }
  | Bit_select COLON bit_sel_def Indent bit_sel_lst { to_tuple Bit_select $5 }
  | Sys_func_call COLON sys_func_call_def Indent sys_func_call_lst { to_tuple Sys_func_call $5 }
  | Logic_net COLON logic_net_def { TUPLE2(Logic_net, $3) }
  | nettyp { $1 }

sys_func_call_lst: { [] }
  | sys_func_call_opt sys_func_call_lst { $1 :: $2 }

sys_func_call_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiargument COLON oexpr { $3 }

part_select_lst: { [] }
  | part_select_opt part_select_lst { $1 :: $2 }

part_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiconstantselect COLON VpiNum { VpiNum $3 }
  | Vpileftrange COLON oexpr { TUPLE2(Vpileftrange, $3) }
  | Vpirightrange COLON oexpr { TUPLE2(Vpirightrange, $3) }

indexed_part_select_lst: { [] }
  | indexed_part_select_opt indexed_part_select_lst { $1 :: $2 }

indexed_part_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiconstantselect COLON VpiNum { VpiNum $3 }
  | Vpiindexedpartselecttype COLON VpiNum { VpiNum $3 }
  | Vpibaseexpr COLON oexpr { TUPLE2(Vpileftrange, $3) }
  | Vpiwidthexpr COLON oexpr { TUPLE2(Vpirightrange, $3) }

operation_lst: { [] }
  | operation_opt operation_lst { $1 :: $2 }

operation_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, optype $3) }
  | Vpioperand COLON oexpr { TUPLE2(Vpioperand, $3) }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }

vport:
  | Port COLON port_def Indent io_decl_lst { to_tuple Port $5 }

nettyp: 
  | Logic_net COLON logic_net_def Indent logic_net_lst { to_tuple Logic_net $5 }

logic_net_lst: { [] }
  | logic_net_opt logic_net_lst { $1 :: $2 }

logic_net_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpinettype COLON VpiNum { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

arraynettyp:
  | Array_net COLON array_net_def Indent array_net_lst { to_tuple Array_net $5 }

array_net_lst: { [] }
  | array_net_opt array_net_lst { $1 :: $2 }

array_net_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpisize COLON VpiNum { Vpisize }
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpinettype COLON VpiNum { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

ret:
  | Int_var COLON int_var_def Indent int_var_lst { to_tuple Int_var $5 }
  | Integer_var COLON integer_var_def Indent integer_var_lst { to_tuple Int_var $5 }
  | Logic_var COLON logic_var_def Indent logic_var_lst { to_tuple Logic_var $5 }
  | Class_var COLON class_var_def Indent class_var_lst { to_tuple Class_var $5 }
  | Enum_var COLON enum_var_def Indent enum_var_lst { to_tuple Enum_var $5 }

def_name: Builtin { Builtin }
  | Work AT STRING { STRING $3 }
  | STRING { STRING $1 }

name: STRING { STRING $1 }
  | Mailbox { Mailbox }
  | Process { Process }
  | Semaphore { Semaphore }

gen_scope_def:
  | LPAREN Work AT pth_lst RPAREN loc { Work }

gen_scope_array_def:
  | LPAREN Work AT pth_lst RPAREN loc { Work }

class_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name RPAREN COMMA File COLON path loc { Work }
  
module_inst_def: { Work }
  | Work AT STRING LPAREN Work AT pth_lst RPAREN COMMA File COLON path loc { STRING $3 }
  
function_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }

logic_net_def: { Work }
  | LPAREN Work AT pth_lst type_lst RPAREN loc { Work }

array_net_def: { Work }
  | LPAREN Work AT pth_lst type_lst RPAREN loc { Work }

var_select_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

part_select_def: { Work }
  | STRING LPAREN Work AT pth_lst RPAREN loc { Work }

indexed_part_select_def: { Work }
  | STRING LPAREN Work AT pth_lst RPAREN loc { Work }

sys_func_call_def: { Work }
  | LPAREN STRING RPAREN loc { Work }

begin_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { TLIST $4 }

for_stmt_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

attribute_def: { Work }
  | LPAREN STRING RPAREN loc { Work }

ref_obj_def: LPAREN STRING RPAREN loc { STRING $2 }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

ref_var_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

bit_sel_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

pth_lst: STRING { [STRING $1] }
  | STRING DOT pth_lst { STRING $1 :: $3 }
  
port_def: { Work }
  | LPAREN STRING RPAREN loc { Work }
  
enum_var_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }

int_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }

integer_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }
  
logic_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }
  
task_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }
  
task_call_def: { Work }
  | LPAREN STRING RPAREN loc { Work }
  
constant_def: { Work }
  | loc { Work }

always_def: { Work }
  | loc { Work }

initial_def: { Work }
  | loc { Work }
  
operation_def: { Work }
  | loc { Work }

event_control_def: { Work }
  | loc { Work }

assignment_def: { Work }
  | loc { Work }

int_typespec_def: { Work }
  | loc { Work }

integer_typespec_def: { Work }
  | loc { Work }
  
range_def: { Work }
  | loc { Work }
  
io_decl_def:
  | LPAREN vnam RPAREN loc { Work }
  
enum_typespec_decl: { Work }
  | LPAREN vnam RPAREN loc { Work }
  
enum_const_decl: { Work }
  | LPAREN vnam RPAREN loc { Work }
  
type_spec: { Work }
  | LPAREN Work AT name type_lst RPAREN { $4 }
  | LPAREN Work AT pth_lst RPAREN { TLIST $4 }

type_lst: { [] }
  | COLON COLON vnam type_lst { $3 :: $4 }
  
class_typespec: { Work }
  | loc { Work }
  
logic_typespec_def: { Work }
  | loc { Work }

path: { [] }
  | DOT path { DOT :: $2}
  | HYPHEN path { DOT :: $2}
  | SLASH path { SLASH :: $2}
  | STRING path { STRING $1 :: $2}
  | Builtin path { Builtin :: $2}
  | Restored path { Builtin :: $2}
  | DESIGN path { Builtin :: $2}
  | Mailbox path { Mailbox :: $2 }
  | Process path { Process :: $2 }
  | Semaphore path { Semaphore :: $2 }
  
vis: { STRING "" }
  | VpiNum { VpiNum $1 }

vnam: { STRING "" }
  | STRING { STRING $1 }
  | New { New }
  | Num { Num }
  | Get { Get }
  | Try_get { Try_get }
  | Put { Put }
  | Peek { Peek }
  | Try_peek { Try_peek }
  | Try_put { Try_put }
  | Bound { Bound }
  | Message { Message }
  | Builtin { Builtin }
  | Array { Array }
  | Queue { Queue }
  | String { String }
  | System { System }
  | Any_sverilog_class { Any_sverilog_class }
  | State { State }
  | Status { Status }
  | Self { Self }
  | Kill { Kill }
  | Await { Await }
  | Suspend { Suspend }
  | Resume { Resume }
  | Keycount { Keycount }
  | FINISHED { FINISHED }
  | RUNNING { RUNNING }
  | WAITING { WAITING }
  | SUSPENDED { SUSPENDED }
  | KILLED { KILLED }
  
fullnam: Builtin COLON COLON vnam { $4 }
  | Work AT name type_lst { TLIST (List.rev $4) }
  | Work AT pth_lst { TLIST (List.rev $3) }
  
input_lst: { [] }
  | input input_lst { $1 :: $2 }
  
unreachable:
| ACCEPT { Work }
| AMPERSAND { Work }
| BACKQUOTE { Work }
| BACKSLASH { Work }
| CARET { Work }
| CONS1 { Work }
| CONS2 { Work }
| CONS3 { Work }
| CONS4 { Work }
| Clk { Work }
| DEFAULT { Work }
| DOLLAR { Work }
| DOUBLEQUOTE { Work }
| ELIST { Work }
| EMPTY_TOKEN { Work }
| END { Work }
| ERROR { Work }
| ERROR_TOKEN { Work }
| GREATER { Work }
| HASH { Work }
| LBRACE { Work }
| LBRACK { Work }
| LESS { Work }
| LINEFEED { Work }
| PERCENT { Work }
| PLING { Work }
| Post_Elab { Work }
| Pre_Elab { Work }
| QUERY { Work }
| QUOTE { Work }
| RBRACE { Work }
| RBRACK { Work }
| Restored { Work }
| SLIST { Work }
| TILDE { Work }
| TLIST { Work }
| TUPLE10 { Work }
| TUPLE2 { Work }
| TUPLE3 { Work }
| TUPLE4 { Work }
| TUPLE5 { Work }
| TUPLE6 { Work }
| TUPLE7 { Work }
| TUPLE8 { Work }
| TUPLE9 { Work }
| UNDERSCORE { Work }
| VBAR { Work }

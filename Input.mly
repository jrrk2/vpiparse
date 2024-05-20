%{
  open Parsing
  let declst = ref []
%}

%token  Indent
%token  ACCEPT
%token  AMPERSAND
%token  AT
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
%token  TILDE
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNDERSCORE
%token  VBAR
%token FINISHED
%token INT
%token KILLED
%token Post_Elab
%token Pre_Elab
%token RUNNING
%token Restored
%token SUSPENDED
%token UINT
%token WAITING
%token Always
%token Assignment
%token Begin
%token Class_defn
%token Class_typespec
%token Class_var
%token Constant
%token DESIGN
%token Design
%token Enum_const
%token Enum_typespec
%token Enum_var
%token Event_control
%token Function
%token Int_typespec
%token Int_var
%token Io_decl
%token Logic_net
%token Logic_typespec
%token Logic_var
%token Module_inst
%token Operation
%token Package
%token Port
%token Range
%token Ref_obj
%token Ref_typespec
%token Task
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
%token Vpiclassdefn
%token Vpicondition
%token Vpiconsttype
%token Vpidecompile
%token Vpidefname
%token Vpidirection
%token Vpielaborated
%token Vpienumconst
%token Vpiexpr
%token Vpifullname
%token Vpiiodecl
%token Vpiindex
%token Vpiinstance
%token Vpileftrange
%token Vpilhs
%token Vpilowconn
%token Vpimethod
%token <int> Vpimethodint
%token Vpiname
%token Vpinet
%token Vpinettype
%token Vpioptype
%token Vpioperand
%token Vpiparent
%token Vpiport
%token Vpiprocess
%token Vpirange
%token Vpireturn
%token Vpirhs
%token Vpirightrange
%token Vpisigned
%token Vpisize
%token Vpistmt
%token Vpitop
%token Vpitopmodule
%token Vpitypedef
%token Vpitypespec
%token Vpivariables
%token Vpivisibility
%token Work
%token <int> VpiNum 
%type <token> ml_start
%start ml_start
%%


ml_start: input_lst EOF_TOKEN { TLIST $1 }

package_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpiclassdefn COLON class_intro { Vpiclassdefn }

class_intro: Class_defn COLON class_def Indent class_lst { Class_defn }

parent:
  | Package COLON Builtin LPAREN Builtin COLON COLON RPAREN { Package }
  | Design COLON LPAREN Work AT STRING RPAREN { Design }
  | Class_defn COLON class_def { Class_defn }
  | Function COLON function_def { Function }
  | Task COLON task_def { Task }
  | Class_var COLON class_var_def { Class_var }
  | Int_var COLON int_var_def { Int_var }
  | Logic_var COLON logic_var_def { Logic_var }
  | Io_decl COLON io_decl_def { Io_decl }
  | Enum_typespec COLON enum_typespec_decl { Enum_typespec }
  | Enum_var COLON enum_var_def { Enum_var }
  | Module_inst COLON module_inst_def { Module_inst }
  | Logic_net COLON logic_net_def { Logic_net }
  | Port COLON port_def { Port }
  | Always COLON always_def { Always }
  | Event_control COLON event_control_def { Event_control }
  | Operation COLON operation_def { Operation }
  | Begin COLON begin_def { Begin }
  | Assignment COLON assignment_def { Assignment }
  | Var_select COLON var_select_def { Var_select }
  | Logic_typespec COLON logic_typespec_def { Logic_typespec }
  | Range COLON range_def { Range }
  | Ref_typespec COLON type_spec { Ref_typespec }

class_var_def:
  | LPAREN Work AT name type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

class_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethod COLON vpi_method_arg { TUPLE2(Vpimethod, $3) }
  | Vpitypedef COLON typespec { TUPLE2(Vpitypedef, $3) }

typespec:
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst { TUPLE2( Enum_typespec, TLIST (List.rev $5)) }

enum_typespec_lst: { [] }
  | enum_typespec_opt enum_typespec_lst { $1 :: $2 }

enum_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpienumconst COLON { Vpienumconst }
  | Enum_const COLON enum_const_decl Indent enum_constant_lst { TUPLE2(Enum_const, TLIST (List.rev $5)) }

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
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }
  | Vpisigned COLON VpiNum { TUPLE2(Vpisigned, VpiNum $3) }

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
  | Ref_typespec COLON type_spec Indent ref_typespec_lst { Ref_typespec }

ref_typespec_lst: { [] }
  | ref_typespec_opt ref_typespec_lst { $1 :: $2 }

ref_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_typespec_actual { TUPLE2(Vpiactual, $3) }

ref_typespec_actual:
  | Class_typespec COLON class_typespec { Class_typespec }
  | Int_typespec COLON int_typespec_def { Int_typespec }
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
  | Io_decl COLON io_decl_def Indent io_decl_lst { TUPLE2(Io_decl, TLIST (List.rev $5)) }
  
task_lst: { [] }
  | task_opt task_lst { $1 :: $2 }

task_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpidefname COLON def_name { $3 }
  | Vpiiodecl COLON { Vpiiodecl }
  | Io_decl COLON io_decl_def Indent io_decl_lst { TUPLE2(Io_decl, TLIST (List.rev $5)) }

io_decl_lst: { [] }
  | io_decl_opt io_decl_lst { $1 :: $2 }

io_decl_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpidirection COLON VpiNum { TUPLE2(Vpidirection,VpiNum $3) }
  | Vpilowconn COLON conn { TUPLE2(Vpilowconn,$3) }
  | Vpiexpr COLON vexpr { TUPLE2(Vpiexpr,$3) }
  | Vpitypedef COLON ref_typespec { $3 }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance,$5) }

conn:
  | Ref_obj COLON ref_obj_def Indent ref_obj_lst { TUPLE2(Ref_obj, TLIST (List.rev $5)) }

ref_obj_lst: { [] }
  | ref_obj_opt ref_obj_lst { $1 :: $2 }

ref_obj_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_obj_actual { TUPLE2(Vpiactual, $3) }

ref_obj_actual:
  | Logic_net COLON logic_net_def { Logic_net }

vexpr:
  | Constant COLON constant_def Indent constant_lst { TUPLE2(Constant, TLIST (List.rev $5)) }

constant_lst: { [] }
  | constant_opt constant_lst { $1 :: $2 }

constant_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpidecompile COLON VpiNum { Vpidecompile }
  | Vpisize COLON VpiNum { Vpisize }
  | UINT COLON VpiNum { UINT }
  | INT COLON VpiNum { INT }
  | Vpiconsttype COLON VpiNum { Vpiconsttype }

package_lst: { [] }
  | package_opt package_lst { $1 :: $2 }

class_lst: { [] }
  | class_opt class_lst { $1 :: $2 }
  
input: DESIGN COLON LPAREN Work AT STRING RPAREN { DESIGN }
  | Vpiname COLON Work AT name { Vpiname }
  | Uhdmallclasses COLON class_intro { Uhdmallclasses }
  | Uhdmallpackages COLON { Uhdmallpackages }
  | Package COLON Builtin LPAREN Builtin COLON COLON RPAREN Indent package_lst { Package }
  | Uhdmtopmodules COLON { Uhdmtopmodules }
  | Uhdmtoppackages COLON { Uhdmtoppackages }
  | Uhdmallmodules COLON { Uhdmallmodules }
  | Module_inst COLON module_inst_def Indent module_inst_lst { TUPLE2(Module_inst, TLIST (List.rev $5)) }
  | Weaklyreferenced COLON weak_lst { Weaklyreferenced }
  | Vpielaborated COLON VpiNum { TUPLE2(Vpielaborated, VpiNum $3) }

weak_lst: { [] }
  | weak_opt weak_lst { $1 :: $2 }

weak_opt:
  | Class_typespec COLON class_typespec Indent Vpiclassdefn COLON Class_defn COLON class_def { Class_typespec }
  | Int_typespec COLON int_typespec_def Indent int_var_lst { Int_typespec }
  | Logic_typespec COLON logic_typespec_def { Logic_typespec }
  | Logic_typespec COLON logic_typespec_def Indent misc_lst { Logic_typespec }
  | Function COLON function_def Indent weak_function_lst { Function }
  | Task COLON task_def Indent task_lst { Task }
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst { Enum_typespec }
  
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
  | Io_decl COLON io_decl_def Indent io_decl_lst { TUPLE2(Io_decl, TLIST (List.rev $5)) }
  
misc_lst: { [] }
  | misc misc_lst { $1 :: $2 }

misc:
  | Vpirange COLON Range COLON range_def Indent range_lst { Vpirange }
  | Vpiparent COLON parent { Vpiparent }
  
range_lst: { [] }
  | range_opt range_lst { $1 :: $2 }

range_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpileftrange COLON vexpr  { Vpileftrange }
  | Vpirightrange COLON vexpr { Vpirightrange }

module_inst_lst: { [] }
  | module_inst_opt module_inst_lst { $1 :: $2 }

module_inst_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpivariables COLON ret { TUPLE2(Vpivariables,$3) }  
  | Vpinet COLON nettyp { TUPLE2(Vpinet, $3) }
  | Vpiport COLON vport { TUPLE2(Vpiport, $3) }
  | Vpiprocess COLON process { TUPLE2(Vpiprocess, $3) }
  | Vpitop COLON VpiNum { TUPLE2(Vpitop, VpiNum $3) }
  | Vpitopmodule COLON VpiNum { TUPLE2(Vpitopmodule, VpiNum $3) }

process:
  | Always COLON always_def Indent always_lst always_typ { TUPLE3(Always, TLIST (List.rev $5), $6) }
  | Always COLON always_def { Always }

always_typ:
  | Vpialwaystype COLON VpiNum {  TUPLE2(Vpialwaystype, VpiNum $3) }

always_lst: { [] }
  | always_opt always_lst { $1 :: $2 }

always_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { TUPLE2(Vpistmt,$3) }

stmt:
  | Event_control COLON event_control_def Indent event_control_lst { TUPLE2(Event_control, TLIST (List.rev $5)) }
  | Begin COLON begin_def Indent begin_lst { TUPLE2(Begin, TLIST (List.rev $5)) }
  | Assignment COLON assignment_def Indent assignment_lst { TUPLE2(Assignment, TLIST (List.rev $5)) }

begin_lst: { [] }
  | begin_opt begin_lst { $1 :: $2 }

begin_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { Vpistmt }

assignment_lst: { [] }
  | assignment_opt assignment_lst { $1 :: $2 }

assignment_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { Vpistmt }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, VpiNum $3) }
  | Vpirhs COLON rhs { Vpirhs }
  | Vpilhs COLON conn { Vpilhs }

rhs:
  | Var_select COLON var_select_def Indent var_select_lst { TUPLE2(Var_select, TLIST (List.rev $5)) }

var_select_lst: { [] }
  | var_select_opt var_select_lst { $1 :: $2 }

var_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpiindex COLON conn { Vpiindex }
  | Vpiactual COLON parent { TUPLE2(Vpiactual, $3) }
  
event_control_lst: { [] }
  | event_control_opt event_control_lst { $1 :: $2 }

event_control_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON cond { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { TUPLE2(Vpistmt, $3) }

cond:
  | Operation COLON operation_def Indent operation_lst { TUPLE2(Operation, TLIST (List.rev $5)) }

operation_lst: { [] }
  | operation_opt operation_lst { $1 :: $2 }

operation_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpioptype COLON VpiNum { TUPLE2(Vpioptype, VpiNum $3) }
  | Vpioperand COLON conn { TUPLE2(Vpioperand, $3) }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON cond { TUPLE2(Vpicondition, $3) }

vport:
  | Port COLON port_def Indent io_decl_lst { TUPLE2(Port, TLIST (List.rev $5)) }

nettyp: 
  | Logic_net COLON logic_net_def Indent logic_net_lst { TUPLE2(Logic_net, TLIST (List.rev $5)) }

logic_net_lst: { [] }
  | logic_net_opt logic_net_lst { $1 :: $2 }

logic_net_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpinettype COLON VpiNum { TUPLE2(Vpinettype, VpiNum $3) }
  | Vpitypespec COLON ref_typespec { Vpitypespec }

ret:
  | Int_var COLON int_var_def Indent int_var_lst { TUPLE2(Int_var, TLIST (List.rev $5)) }
  | Logic_var COLON logic_var_def Indent logic_var_lst { TUPLE2(Logic_var, TLIST (List.rev $5)) }
  | Class_var COLON class_var_def Indent class_var_lst { TUPLE2(Class_var, TLIST (List.rev $5)) }
  | Enum_var COLON enum_var_def Indent enum_var_lst { TUPLE2(Enum_var, TLIST (List.rev $5)) }

def_name: Builtin { Builtin }
  | Work AT STRING { STRING $3 }
  
name: STRING { STRING $1 }
  | Mailbox { Mailbox }
  | Process { Process }
  | Semaphore { Semaphore }
  
class_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name RPAREN COMMA File COLON path COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
module_inst_def: { Work }
  | Work AT STRING LPAREN Work AT STRING RPAREN COMMA File COLON path COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
function_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

logic_net_def: { Work }
  | LPAREN Work AT STRING DOT STRING type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

var_select_def: { Work }
  | LPAREN Work AT pth_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

begin_def: { Work }
  | LPAREN Work AT pth_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
ref_obj_def: { Work }
  | LPAREN Work AT pth_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

pth_lst: STRING { [STRING $1] }
  | STRING DOT pth_lst { STRING $1 :: $3 }
  
port_def: { Work }
  | LPAREN STRING RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
enum_var_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
int_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
logic_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN { Work }
  | LPAREN Work AT pth_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
task_def: { Work }
  | LPAREN Work AT name type_lst RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
constant_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
always_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
operation_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

event_control_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

assignment_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
int_typespec_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
range_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
io_decl_def:
  | LPAREN vnam RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
enum_typespec_decl: { Work }
  | LPAREN vnam RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
enum_const_decl: { Work }
  | LPAREN vnam RPAREN COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
type_spec: { Work }
  | LPAREN Work AT name type_lst RPAREN { $4 }
  | LPAREN Work AT STRING DOT STRING RPAREN { STRING $6 }

type_lst: { [] }
  | COLON COLON vnam type_lst { $3 :: $4 }
  
class_typespec: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }
  
logic_typespec_def: { Work }
  | COMMA Line COLON VpiNum COLON VpiNum COMMA Endln COLON VpiNum COLON VpiNum { Work }

path: { [] }
  | DOT path { DOT :: $2}
  | SLASH path { SLASH :: $2}
  | STRING path { STRING $1 :: $2}
  | Builtin path { Builtin :: $2}

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
  
  
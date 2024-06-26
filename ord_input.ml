open String
open Input

let othinput = ref None

let getstr = function
| FINISHED -> "FINISHED";
| INT -> "INT";
| KILLED -> "KILLED";
| Post_Elab -> "Post-Elab";
| Pre_Elab -> "Pre-Elab";
| RUNNING -> "RUNNING";
| Restored -> "Restored";
| SUSPENDED -> "SUSPENDED";
| UINT -> "UINT";
| WAITING -> "WAITING";
| Always -> "_always";
| Assignment -> "_assignment";
| Begin -> "_begin";
| Class_defn -> "_class_defn";
| Class_typespec -> "_class_typespec";
| Class_var -> "_class_var";
| Constant -> "_constant";
| DESIGN -> "design";
| Design -> "_design";
| Enum_const -> "_enum_const";
| Enum_typespec -> "_enum_typespec";
| Enum_var -> "_enum_var";
| Event_control -> "_event_control";
| Function -> "_function";
| Int_typespec -> "_int_typespec";
| Int_var -> "_int_var";
| Io_decl -> "_io_decl";
| Logic_net -> "_logic_net";
| Logic_typespec -> "_logic_typespec";
| Logic_var -> "_logic_var";
| Module_inst -> "_module_inst";
| Operation -> "_operation";
| Package -> "_package";
| Port -> "_port";
| Range -> "_range";
| Ref_obj -> "_ref_obj";
| Ref_typespec -> "_ref_typespec";
| Task -> "_task";
| Var_select -> "_var_select";
| Weaklyreferenced -> "_weaklyReferenced";
| Any_sverilog_class -> "any_sverilog_class";
| Array -> "array";
| Await -> "await";
| Bound -> "bound";
| Builtin -> "builtin";
| Clk -> "clk";
| Endln -> "endln";
| File -> "file";
| Get -> "get";
| Keycount -> "keyCount";
| Kill -> "kill";
| Line -> "line";
| Mailbox -> "mailbox";
| Message -> "message";
| New -> "new";
| Num -> "num";
| Peek -> "peek";
| Process -> "process";
| Put -> "put";
| Queue -> "queue";
| Resume -> "resume";
| Self -> "self";
| Semaphore -> "semaphore";
| State -> "state";
| Status -> "status";
| String -> "string";
| Suspend -> "suspend";
| System -> "system";
| Try_get -> "try_get";
| Try_peek -> "try_peek";
| Try_put -> "try_put";
| Uhdmallclasses -> "uhdmallClasses";
| Uhdmallmodules -> "uhdmallModules";
| Uhdmallpackages -> "uhdmallPackages";
| Uhdmtopmodules -> "uhdmtopModules";
| Uhdmtoppackages -> "uhdmtopPackages";
| Vpiactual -> "vpiActual";
| Vpialwaystype -> "vpiAlwaysType";
| Vpiclassdefn -> "vpiClassDefn";
| Vpicondition -> "vpiCondition";
| Vpiconsttype -> "vpiConstType";
| Vpidecompile _ -> "vpiDecompile";
| Vpidefname -> "vpiDefName";
| Vpidirection -> "vpiDirection";
| Vpielaborated -> "vpiElaborated";
| Vpienumconst -> "vpiEnumConst";
| Vpiexpr -> "vpiExpr";
| Vpifullname -> "vpiFullName";
| Vpiiodecl -> "vpiIODecl";
| Vpiindex -> "vpiIndex";
| Vpiinstance -> "vpiInstance";
| Vpileftrange -> "vpiLeftRange";
| Vpilhs -> "vpiLhs";
| Vpilowconn -> "vpiLowConn";
| Vpimethod -> "vpiMethod";
| Vpiname -> "vpiName";
| Vpinet -> "vpiNet";
| Vpinettype -> "vpiNetType";
| Vpioptype -> "vpiOpType";
| Vpioperand -> "vpiOperand";
| Vpiparent -> "vpiParent";
| Vpiport -> "vpiPort";
| Vpiprocess -> "vpiProcess";
| Vpirange -> "vpiRange";
| Vpireturn -> "vpiReturn";
| Vpirhs -> "vpiRhs";
| Vpirightrange -> "vpiRightRange";
| Vpisigned -> "vpiSigned";
| Vpisize -> "vpiSize";
| Vpistmt -> "vpiStmt";
| Vpitop -> "vpiTop";
| Vpitopmodule -> "vpiTopModule";
| Vpitypedef -> "vpiTypedef";
| Vpitypespec -> "vpiTypespec";
| Vpivariables -> "vpiVariables";
| Vpivisibility -> "vpiVisibility";
| Work -> "work";
| VpiNum n -> n
| LPAREN -> "("
| RPAREN -> ")"
| oth -> othinput := Some oth; "UNKNOWN"

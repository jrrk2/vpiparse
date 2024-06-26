//*****************************************************************************
//
//    <vscr - Verilog converter to abc format.>
//    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>
//    Derived from verilator/src/verilog.y bison grammar code by Wilson Snyder
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//*****************************************************************************

// Generic void
%token EMPTY
// Generic double
%token<token*token> DOUBLE
// Generic triple
%token<token*token*token> TRIPLE
// Generic quadruple
%token<token*token*token*token> QUADRUPLE
// Generic quintuple
%token<token*token*token*token*token> QUINTUPLE
// Generic sextuple
%token<token*token*token*token*token*token> SEXTUPLE
// Generic septuple
%token<token*token*token*token*token*token*token> SEPTUPLE
// Generic octuple
%token<token*token*token*token*token*token*token*token> OCTUPLE
%token<token*token*token*token*token*token*token*token*token> NONUPLE
%token<token*token*token*token*token*token*token*token*token*token> DECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token> UNDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token> DUODECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token> TREDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token> QUATTUORDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> QUINDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> SEXDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> SEPTENDECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> OCTODECUPLE
%token<token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> NOVEMDECUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> VIGENUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> UNVIGENUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> DUOVIGENUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TREVIGENUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> QUATTUORVIGENUPLE
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> QUINVIGENUPLE

// non-keyword tokens
%token BITSEL
%token PARTSEL
%token IOPORT
%token SUBMODULE
%token SUBCCT
%token MODINST
%token PRIMINST
%token<token * token> RANGE
%token<token list> PLIST
%token<token list> TLIST
%token<(token,unit) Hashtbl.t * (token,unit) Hashtbl.t> THASH
// for undeclared wires
%token IMPLICIT
%token RECEIVER
%token DRIVER
%token BIDIR
// for dotted hierarchical paths (models only)
%token<token list> DOTTED
// pre-proc tokens
%token P_CELLDEFINE
%token P_DEFINE
%token P_DELAY_MODE_PATH
%token P_DISABLE_PORTFAULTS
%token P_ELSE
%token P_ENABLE_PORTFAULTS
%token P_ENDCELLDEFINE
%token P_ENDIF
%token P_IFDEF
%token P_IFNDEF
%token<string> P_INCLUDE
%token P_NOSUPPRESS_FAULTS
%token P_PROTECT
%token P_ENDPROTECT
%token P_RESETALL
%token P_SUPPRESS_FAULTS
%token<string> P_TIMESCALE
%token<string> PREPROC	// some other token, not (yet) recognized
// for named blocks
%token NAMED
// for generate item
%token GENITEM
// for unknown modules/primitives
%token UNKNOWN
// for invalid use
%token NOTCONST
// for scalar nets
%token SCALAR
// for empty types
%token VOID
// for special nets
%token SPECIAL
// for parameters
%token PARAMUSED
// for tasks/funcs/senslists
%token TASKUSED
%token FUNCUSED
%token SENSUSED
%token FUNCASSIGNED
// for memories
%token MEMORY
// for transistor level
%token NMOS
%token PMOS
%token TRAN
%token<string> TRANIF
// for function/task refs
%token TASKREF
%token FUNCREF
// for cell pins
%token CELLPIN
// for case statements
%token CASECOND
%token WILDEQUAL
%token GENCASE
%token GENCASECOND
// for delay specs (models only)
%token MINTYPMAX
// for end labels
%token ENDLABEL
// for {a,b,c}
%token CONCAT
// for tables
%token<char*char> TEDGE
// Generic lexer tokens, for example a number
// IEEE: real_number
%token<float>		FLOATNUM	// "FLOATING-POINT NUMBER"
// IEEE: identifier, class_identifier, class_variable_identifier,
// covergroup_variable_identifier, dynamic_array_variable_identifier,
// enum_identifier, interface_identifier, interface_instance_identifier,
// package_identifier, type_identifier, variable_identifier,
%token<string>		IDSTR	// "IDENTIFIER"
// %token<Idhash.idhash>	ID
// IEEE: integral_number
%token<int>		INT	// "INTEGER NUMBER as int"
%token<int*int*int>	WIDTHNUM // "INTEGER NUMBER as (radix,width,value)"
%token<string>		INTNUM	// "INTEGER NUMBER as string"
%token<string>		BINNUM	// "BINARY NUMBER"
%token<string>		OCTNUM	// "OCTAL NUMBER"
%token<string>		DECNUM	// "DECIMAL NUMBER"
%token<string>		HEXNUM	// "HEXADECIMAL NUMBER"
%token<int64>		INT64	// "INTEGER NUMBER as int64"
// IEEE: string_literal
%token<string>		ASCNUM	// "ASCII NUMBER / STRING"

%token<char> ILLEGAL
%token DOLLAR
%token EOF

// Specific keywords
%token P_AMPERSAND
%token AT
%token P_CARET
%token COLON
%token COMMA
%token DIVIDE
%token EQUALS
%token GREATER
%token HASH
%token LBRACK
%token LCURLY
%token LESS
%token LPAREN
%token MINUS
%token MODULO
%token DOT
%token PLING
%token PLUS
%token QUERY
%token RBRACK
%token RCURLY
%token RPAREN
%token SEMICOLON
%token P_TILDE
%token TIMES
%token P_VBAR
// KEYWORD means match // "keyword"
// Other cases are XX_KEYWORD where XX makes it unique,
// for example P_ for punctuation based operators.
%token ALWAYS		// "always"
%token AND		// "and"
%token ASSERT		// "assert"
%token ASSIGN		// "assign"
%token ASSIGNMENT	// "= assignment"
%token DLYASSIGNMENT	// "<= assignment"
%token AUTOMATIC	// "automatic"
%token BEGIN		// "begin"
%token BUF		// "buf"
%token<string> BUFIF	// "bufif"
%token CASE		// "case"
%token CASEX		// "casex"
%token CASEZ		// "casez"
%token CLOCKING	// "clocking"
%token COVER		// "cover"
%token DEASSIGN		// "deassign"
%token DEFAULT	// "default"
%token DEFPARAM	// "defparam"
%token DISABLE	// "disable"
%token DO		// "do"
%token ELSE		// "else"
%token END		// "end"
%token ENDCASE	// "endcase"
%token ENDCLOCKING	// "endclocking"
%token ENDFUNCTION	// "endfunction"
%token ENDGENERATE	// "endgenerate"
%token ENDMODULE	// "endmodule"
%token ENDPRIMITIVE	// "endprimitive"
%token ENDSPECIFY	// "endspecify"
%token ENDTABLE	// "endtable"
%token ENDTASK	// "endtask"
%token EVENT		// "event"
%token FINAL		// "final"
%token FOR		// "for"
%token FOREVER		// "forever"
%token FUNCTION	// "function"
%token GENERATE	// "generate"
%token GENVAR		// "genvar"
%token IF		// "if"
%token IFF		// "iff"
%token INITIAL	// "initial"
%token INOUT		// "inout"
%token INPUT		// "input"
%token INTEGER	// "integer"
%token TIME	// "time"
%token LOCALPARAM	// "localparam"
%token MODULE		// "module"
%token NAND		// "nand"
%token NEGEDGE	// "negedge"
%token NOR		// "nor"
%token NOT		// "not"
%token<string> NOTIF	// "notif"
%token OR		// "or"
%token OUTPUT		// "output"
%token PARAMETER	// "parameter"
%token POSEDGE	// "posedge"
%token PRIMITIVE	// "primitive"
%token PROPERTY	// "property"
%token PULLUP	// "pullup"
%token REAL	// "real"
%token REG	// "reg"
%token REPEAT	// "repeat"
%token SCALARED	// "scalared"
%token SIGNED	// "signed"
%token SPECIFY	// "specify"
%token STATIC	// "static"
%token SUPPLY0	// "supply0"
%token SUPPLY1	// "supply1"
%token TABLE	// "table"
%token TASK	// "task"
%token TRI	// "tri"
%token TRI0	// "tri0"
%token TRI1	// "tri0"
%token UNSIGNED	// "unsigned"
%token VECTORED	// "vectored"
%token<string> WEAK	// "weak"
%token<string> PWEAK	// "(weak"
%token<string> STRONG	// "strong"
%token<string> PSTRONG	// "(strong"
%token WHILE		// "while"
%token WIRE		// "wire"
%token XNOR		// "xnor"
%token XOR		// "xor"

%token D_ATTRIBUTE	// "$attribute"
%token D_BITS		// "$bits"
%token D_C		// "$c"
%token D_CLOG2		// "$clog2"
%token D_COUNTDRIVERS	// "$countdrivers"
%token D_COUNTONES	// "$countones"
%token D_DISPLAY	// "$display"
%token D_ERROR		// "$error"
%token D_FATAL		// "$fatal"
%token D_FCLOSE		// "$fclose"
%token D_FDISPLAY	// "$fdisplay"
%token D_FEOF		// "$feof"
%token D_FFLUSH		// "$fflush"
%token D_FGETC		// "$fgetc"
%token D_FGETS		// "$fgets"
%token D_FINISH		// "$finish"
%token D_FOPEN		// "$fopen"
%token D_FSCANF		// "$fscanf"
%token D_FWRITE		// "$fwrite"
%token D_FWRITEH	// "$fwriteh"
%token D_INFO		// "$info"
%token D_ISUNKNOWN	// "$isunknown"
%token D_MONITOR	// "$monitor"
%token D_ONEHOT		// "$onehot"
%token D_ONEHOT0	// "$onehot0"
%token D_RANDOM		// "$random"
%token D_READMEMB	// "$readmemb"
%token D_READMEMH	// "$readmemh"
%token D_SIGNED		// "$signed"
%token D_SSCANF		// "$sscanf"
%token D_STIME		// "$stime"
%token D_STOP		// "$stop"
%token D_TEST_PLUSARGS	// "$test$plusargs"
%token D_TIME		// "$time"
%token D_UNSIGNED	// "$unsigned"
%token D_WARNING	// "$warning"
%token D_WRITE		// "$write"

%token NOCHANGE // $nochange
%token D_HOLD // $hold
%token D_PERIOD // $period
%token D_RECOVERY // $recovery
%token D_RECREM // $recrem
%token D_REMOVAL // $removal
%token D_SETUPHOLD // $setuphold
%token D_SETUP // $setup
%token D_SKEW // $skew
%token D_TIMESKEW // $timeskew
%token D_WIDTH // $width
%token SHOWCANCELLED // showcancelled
%token NOSHOWCANCELLED // noshowcancelled
%token SPECPARAM // specparam
%token IF_NONE // ifnone
%token P_TILDE_VBAR // ~|
%token TOKEN_EDGE01 // 01
%token TOKEN_EDGE_10 // 10
%token TOKEN_ZERO // 0
%token TOKEN_ONE // 1
%token PATHPULSE // PATHPULSE$
%token FULLSKEW // $fullskew
%token PULSESTYLE_ONDETECT // pulsestyle_ondetect
%token PULSESTYLE_ONEVENT // pulsestyle_onevent
%token EDGE // edge

%token<string> Z_OR_X // z_or_x

%token P_NXOR // ~^
%token P_OROR		// "||"
%token P_ANDAND		// "&&"
%token P_NOR		// "~|"
%token P_XNOR		// "^~"
%token P_NAND		// "~&"
%token P_EQUAL		// "=="
%token P_NOTEQUAL	// "!="
%token P_CASEEQUAL	// "==="
%token P_CASENOTEQUAL	// "!=="
%token P_WILDEQUAL	// "==?"
%token P_WILDNOTEQUAL	// "!=?"
%token P_GTE		// ">="
%token P_LTE		// "<="
%token P_SLEFT		// "<<"
%token P_SRIGHT		// ">>"
%token P_SSRIGHT	// ">>>"
%token P_POW		// "**"

%token P_PLUSCOLON	// "+:"
%token P_MINUSCOLON	// "-:"
%token P_EQGT		// "=>"
%token P_ASTGT		// "*>"
%token P_ANDANDAND	// "&&&"
%token P_POUNDPOUND	// "##"
%token P_DOTSTAR	// ".*"

%token P_ATAT		// "@@"
%token P_COLONCOLON	// "::"
%token P_COLONEQ	// ":="
%token P_COLONDIV	// ":/"
%token P_ORMINUSGT	// "|->"
%token P_OREQGT		// "|=>"

%token P_PLUSEQ		// "+="
%token P_MINUSEQ	// "-="
%token P_TIMESEQ	// "*="
%token P_DIVEQ		// "/="
%token P_MODEQ		// "%="
%token P_ANDEQ		// "&="
%token P_OREQ		// "|="
%token P_XOREQ		// "^="
%token P_SLEFTEQ	// "<<="
%token P_SRIGHTEQ	// ">>="
%token P_SSRIGHTEQ	// ">>>="

%token P_MINUSGT	// "->"

/*
%token PSL		// "psl"
%token PSL_DEFAULT	// "PSL default"
%token PSL_ALWAYS	// "PSL always"
%token PSL_ASSERT	// "PSL assert"
%token PSL_CLOCK	// "PSL clock"
%token PSL_COVER	// "PSL cover"
%token PSL_REPORT	// "PSL report"
%token PSL_FOR
%token PSL_IF
%token PSL_ABORT
%token PSL_ASSUME_GUARANTEE
%token PSL_BEFORE_PLING
%token PSL_BEFORE_
%token PSL_BEFORE
%token PSL_BOOLEAN
%token PSL_CONST
%token PSL_ENDPOINT
%token PSL_EVENTUALLY_PLING
%token PSL_FAIRNESS
%token PSL_FELL
%token PSL_FORALL
%token PSL_IN
%token PSL_INF
%token PSL_INHERIT
%token PSL_NEVER
%token PSL_NEXT_PLING
%token PSL_NEXT
%token PSL_NEXT_A_PLING
%token PSL_NEXT_A
%token PSL_NEXT_E_PLING
%token PSL_NEXT_E
%token PSL_NEXT_EVENT_PLING
%token PSL_NEXT_EVENT
%token PSL_NEXT_EVENT_A_PLING
%token PSL_NEXT_EVENT_A
%token PSL_NEXT_EVENT_E_PLING
%token PSL_NEXT_EVENT_E
%token PSL_PREV
%token PSL_PROPERTY
%token PSL_RESTRICT_GUARANTEE
%token PSL_ROSE
%token PSL_SEQUENCE
%token PSL_STABLE
%token PSL_UNION
%token PSL_UNTIL
%token PSL_UNTIL_
%token PSL_VMODE
%token PSL_VPROP
%token PSL_VUNIT
%token PSL_WITHIN

%token TokenLPar
%token TokenRPar
%token TokenLBr
%token TokenRBr

%token <string> TokenProp
%token TokenNEQ
%token TokenEQ
%token TokenEEQ

%token TokenGen
%token TokenFin
%token TokenOpO
%token TokenOpH
%token TokenNeg
%token TokenNext
%token TokenYest
%token TokenZest

%token TokenUnt
%token TokenOpW
%token TokenRel
%token TokenOpS
%token TokenOpT
%token TokenAnd
%token TokenOr
%token TokenImp
%token TokenEq

%token TokenFby
%token TokenBFby
%token TokenTrig
%token TokenBTrig

%token TokenCup
%token TokenCap
%token TokenScol
%token TokenCol
%token TokenStar
%token TokenPlus
%token TokenCl
%token<Psl.psl> PSL_CONTAINER
*/
%token RELATION
%token FINITE
%token SIGN_EXT
%token PROPOSITION
%token SDIV
%token SREM
%token SMOD
%token ENDOFFILE

// Verilog op precedence
%left		COLON
%left		QUERY
%left		P_OROR
%left		P_ANDAND
%left		P_VBAR P_NOR
%left		P_CARET
%left		P_XNOR
%left		P_AMPERSAND P_NAND
%left		P_EQUAL P_NOTEQUAL P_CASEEQUAL P_CASENOTEQUAL P_WILDEQUAL P_WILDNOTEQUAL
%left		GREATER LESS P_GTE P_LTE
%left		P_SLEFT P_SRIGHT P_SSRIGHT
%left		PLUS MINUS
%left		TIMES DIVIDE MODULO
%left		P_POW
%left	prUNARYARITH
%left	prREDUCTION
%left	prNEGATION

%nonassoc prLOWER_THAN_ELSE
%nonassoc ELSE

// Types are in same order as declarations.
// Naming:
//	Trailing E indicates this type may have empty match

%start start

%type <char> edge1
%type <char*char> edge2
%type <token list> trowList
%type <token list> tinList
%type <token list> tregoutList
%type <token> Anyrange
%type <token> AssertStmt
%type <token list> AssignList
%type <token> AssignOne
%type <token> caseAttrE
%type <token list> caseCondList
%type <token list> caseList
%type <token> caseListE
%type <token> case
%type <token list> cateList
%type <token> cellpinItem
%type <token list> cellpinItList
%type <token list> cellpinList
%type <token> commaEListE
%type <token> commaVRDListE
%type <token list> concIdList
%type <token> constExpr
%type <token list> cStrList
%type <token list> defpList
%type <token> defpOne
%type <token> delay
%type <token> delayE
%type <token> delayrange
%type <token> delayStrength
%type <token> dlyTerm
%type <token> endLabelE
%type <token> eventControl
%type <token> expr
%type <token list> exprList
%type <token> exprNoStr
%type <token> exprStrText
%type <token> funcDecl
%type <token> funcArgs
%type <token> funcRef
%type <token> funcTypeE
%type <token> funcVar
%type <token list> funcVarList
%type <token> taskVar
%type <token list> taskVarList
%type <token> gateAnd
%type <token list> gateAndList
%type <token list> gateAndPinList
%type <token> gateBuf
%type <token list> gateBufList
%type <token> gateDecl
%type <token> gateIdE
%type <token> gateNand
%type <token list> gateNandList
%type <token> gateNor
%type <token list> gateNorList
%type <token> gateNot
%type <token list> gateNotList
%type <token> gateOr
%type <token list> gateOrList
%type <token list> gateOrPinList
%type <token> gateXnor
%type <token list> gateXnorList
%type <token> gateXor
%type <token list> gateXorList
%type <token list> gateXorPinList
%type <token list> genCaseList
%type <token> genCaseListE
%type <token> generateRegion
%type <token> genItem
%type <token> genItemBegin
%type <token> genItemBlock
%type <token list> genItemList
%type <token> genTopBlock
%type <token> idArrayed
%type <token> idDotted
%type <token> instDecl
%type <token list> instnameList
%type <token> instnameParen
%type <token> instRangeE
%type <token> labeledStmt
%type <token> lifetimeE
%type <token> minTypMax
%type <token> modItem
%type <token list> modItemListE
%type <token> modOrGenItem
%type <token list> modParArgs
%type <token> modParDecl
%type <token list> modParE
%type <token list> modParList
%type <token> modParSecond
%type <token list> modPortsE
%type <token> netSig
%type <token list> netSigList
%type <token> param
%type <token list> paramList
%type <token> parenE
%type <token> Port
%type <token> PortDecl
%type <token>      PortDirection
%type <token list> PortList
%type <token> PortRangeE
%type <token list> RangeList
%type <token> RangeListE
%type <token> regrangeE
%type <token> regsig
%type <token list> regsigList
%type <token> senitem
%type <token> senitemEdge
%type <token> senitemVar
%type <token list> senList
%type <token> sigAndAttr
%type <token> sigAttrListE
%type <token> sigId
%type <token> signingE
%type <token> start
%type <token> stateCaseForIf
%type <token> stmt
%type <token> stmtBlock
%type <token list> stmtList
%type <token> strAsInt
%type <token> strAsText
%type <token> taskDecl
%type <token> taskRef
%type <token> v2kVarDeclE
%type <token> varDecl
%type <token list> varDeclList
%type <token>      varGenVar
%type <token>      varGParam
%type <token>      varLParam
%type <token>      varNet
%type <token> varRefBase
%type <token> varRefDotBit
%type <token> varRefMem
%type <token>      varReg
%type <token list> vrdList
%type <token> identifier
%{
module type Ordered =
  sig
    type t 
    val compare : token -> token -> int
  end

module OrdTok : Ordered with type t = token =
  struct
    type t = token
    let compare a b = compare a b;
  end

type tset = Set.Make(OrdTok).t

type tsigattr = Sigundef |
        Sigarray of tset array |
        Sigparam of token |
        Sigtask of token |
        Sigfunc of token |
	Signamed of token

and symtab = {
  symattr : tset;
  width : token;
  path : string;
  sigattr : tsigattr;
  localsyms : shash;
}

and sentries = (string, symtab) Hashtbl.t

and symrec = { nxt : shash; syms: sentries; stabarch: string; stabnam: string}
 
and shash = EndShash | Shash of symrec

module TokSet = Set.Make (OrdTok)

let enterid str =
  let len = String.length str in
  if len = 0 then failwith "null string unexpected in enterid";
  str

%}

%%

identifier:	IDSTR	{ IDSTR (enterid $1) }

//**********************************************************************
//

start:		ENDOFFILE				{ ENDOFFILE }
	|	modprimDecl				{ $1 }
	;

modprimDecl:	moduleDecl				{ $1 }
	|	primDecl				{ $1 }
	|	preproc					{ $1 }
	;

// Pre-proc

preproc:	P_CELLDEFINE			        { P_CELLDEFINE }
        |       P_DELAY_MODE_PATH        		{ P_DELAY_MODE_PATH }
        |       P_DISABLE_PORTFAULTS        		{ P_DISABLE_PORTFAULTS }
        |       P_ENABLE_PORTFAULTS        		{ P_ENABLE_PORTFAULTS }
        |       P_SUPPRESS_FAULTS        		{ P_SUPPRESS_FAULTS }
        |       P_PROTECT        			{ P_PROTECT }
        |       P_RESETALL        			{ P_RESETALL }
        |       P_TIMESCALE        			{ P_TIMESCALE $1 }
	|	PREPROC					{ PREPROC $1 }
	|	P_ENDCELLDEFINE        			{ P_ENDCELLDEFINE }
        |       P_NOSUPPRESS_FAULTS        		{ P_NOSUPPRESS_FAULTS }
        |       P_ENDPROTECT        			{ P_ENDPROTECT }
			

//**********************************************************************
// Module headers

// IEEE: module_declaration:
moduleDecl:     MODULE identifier modParE modPortsE SEMICOLON modItemListE ENDMODULE
          {
	  let param,noparam = List.partition (function PLIST lst -> true | _ -> false) $6 in
	  QUINTUPLE ( MODULE, $2, TLIST ($3 @ List.flatten (List.map (function PLIST lst -> lst | _ -> []) param)), TLIST $4, TLIST noparam )
          }

// IEEE: primitive_declaration:
primDecl:       PRIMITIVE identifier modParE modPortsE SEMICOLON primItemList ENDPRIMITIVE
                {
		QUINTUPLE ( PRIMITIVE, $2, TLIST $3, TLIST $4, TLIST $6 )
		}
				
primItemList:
		primItem				{ [ $1 ] }
	|	primItem primItemList			{ $1 :: $2 }
	;

primItem:
		PortDecl 				{ $1 }
	|	varDecl 				{ $1 }
	|	tableDecl				{ $1 }
	;

modParE:
		/* empty */				{ [] }
	|	HASH LPAREN RPAREN			{ [] }
	|	HASH LPAREN modParArgs RPAREN		{ $3 }
	;

modParArgs:
		modParDecl				{ [ $1 ] }
	|	modParDecl COMMA modParList		{ $1 :: $3 }
	;

modParList:
		modParSecond				{ [ $1 ] }
	|	modParList COMMA modParSecond 		{ $1 @ [ $3 ] }
	;

// Called only after a comma in a v2k list, to allow parsing "parameter a,b, parameter x"
modParSecond:
		modParDecl				{ $1  }
	|	param					{ $1  }
	;

modPortsE:
		/* empty */				{ [] }
	|	LPAREN RPAREN				{ [] }
	|	LPAREN PortList RPAREN			{ $2 }
	|	LPAREN PortV2kArgs RPAREN		{ $2 }
	;

PortList:
		Port				       	{ [ $1 ] }
	|	Port COMMA PortList	  	   	{ $1 :: $3 }
	;

Port:
		identifier PortRangeE		       	{ $1 }
	|	DOT identifier LPAREN expr RPAREN	{ DOUBLE ( DOT, $2  ) }
	;

PortV2kArgs:
		PortV2kDecl				{ [ $1 ] }
	|	PortV2kDecl COMMA PortV2kList		{ $1 :: $3 }
	;

PortV2kList:
		PortV2kSecond				{ [ $1 ] }
	|	PortV2kSecond COMMA PortV2kList		{ $1 :: $3 }
	;

// Called only after a comma in a v2k list, to allow parsing "input a,b"
PortV2kSecond:
		PortV2kDecl				{ $1 }
	|	PortV2kInit				{ $1 }
	;

PortV2kInit:
		PortV2kSig				{ $1 }
	|	PortV2kSig EQUALS expr			{ TRIPLE (EQUALS, $1, $3 ) }
	;

PortV2kSig:
		sigAndAttr				{ $1 }
	;

//************************************************
// Variable Declarations

varDeclListE:
		/* empty */				{ [ ] }
	|	varDeclList				{ $1 }
	;

varDeclList:
		varDecl					{ [ $1 ] }
	|	varDecl varDeclList			{ $1 :: $2 }
	;

regsigList:
		regsig  				{ [ $1 ] }
	|	regsigList COMMA regsig		       	{ $1 @ [ $3 ] }
	;

PortV2kDecl:
		PortDirection v2kVarDeclE signingE regrangeE PortV2kInit
			{  QUINTUPLE ( $1 , $2 , $3 , $4 , $5 )  }
	;

// IEEE: Port_declaration - plus SEMICOLON
PortDecl:
		PortDirection v2kVarDeclE signingE regrangeE regsigList SEMICOLON
			{  QUINTUPLE ( $1, $2, $3, $4, TLIST $5 )  }
	;

varDecl:
		varReg    signingE regrangeE regsigList SEMICOLON
			{  QUADRUPLE ($1, $2, $3, TLIST $4 )  }
	|	varGParam signingE regrangeE paramList SEMICOLON
			{  PLIST(List.map (function TRIPLE(id,attr,expr) -> SEXTUPLE($1, $2, $3, id, attr, expr) | _ -> failwith "parameter internal error") $4)}
	|	varLParam signingE regrangeE paramList SEMICOLON
			{  QUADRUPLE ($1, $2, $3, TLIST $4 )  }
	|	varNet    signingE delayrange netSigList SEMICOLON
			{  QUADRUPLE ($1, $2, $3, TLIST $4 )  }
	|	varGenVar signingE            regsigList SEMICOLON
			{  TRIPLE ($1, $2, TLIST $3 ) }
	;

modParDecl:
		varGParam  signingE regrangeE param
		 	{ match $4 with TRIPLE(id,attr,expr) -> SEXTUPLE($1, $2, $3, id, attr, expr) | _ -> failwith "parameter internal error" }
	;

varNet:		SUPPLY0					{ SUPPLY0 }
	|	SUPPLY1					{ SUPPLY1 }
	|	WIRE 					{ WIRE }
	|	TRI delayStrength			{ DOUBLE(TRI, $2) }
	|	TRI0 					{ TRI0 }
	|	TRI1 					{ TRI1 }
	;

varGParam:	PARAMETER				{ PARAMETER }
	;

varLParam:	LOCALPARAM				{ LOCALPARAM }
	;

varGenVar:	GENVAR					{ GENVAR }
	;

varReg:		REG					{ REG }
	|	REAL					{ REAL }
	|	INTEGER					{ INTEGER }
	|	EVENT					{ EVENT }
	|	TIME					{ TIME }
	;

readmem:	D_READMEMB				{ D_READMEMB }
	|	D_READMEMH				{ D_READMEMH }
	;

//IEEE: Port_direction
PortDirection:	INPUT					{ INPUT }
	|	OUTPUT					{ OUTPUT }
	|	INOUT					{ INOUT }
//	|	REF					{ REF }
	;

// IEEE: signing - plus empty
signingE:	/*empty*/ 				{ EMPTY }
	|	SIGNED					{ SIGNED }
	|	UNSIGNED				{ UNSIGNED }
	;

v2kVarDeclE:	/*empty*/ 				{ EMPTY }
	|	varNet 					{ $1 }
	|	varReg 					{ $1 }
	;

//************************************************
// Module Items

modItemListE:
		/* empty */				{ [] }
	|	preproc	modItemListE			{ $1 :: $2 }
	|	modItem modItemListE			{ $1 :: $2 }
	;

modItem:
		modOrGenItem 				{ $1 }
	|	generateRegion				{ $1 }
	|	SPECIFY specItemList	{ DOUBLE (SPECIFY, TLIST $2 ) }
	;

specItemList:
		ENDSPECIFY				{ [] }
	|	specItem specItemList			{ $1 :: $2 }
	;

specItem:
		BINNUM		{	BINNUM $1	 }
	|	COLON		{	COLON		 }
	|	COMMA		{	COMMA		 }
	|	D_HOLD	    	{	D_HOLD	    	 }
	|	DOLLAR		{	DOLLAR		 }
	|	D_PERIOD	{	D_PERIOD	 }
	|	D_RECOVERY	{	D_RECOVERY	 }
	|	D_RECREM	{	D_RECREM	 }
	|	D_REMOVAL	{	D_REMOVAL	 }
	|	D_SETUP		{	D_SETUP		 }
	|	D_SETUPHOLD	{	D_SETUPHOLD	 }
	|	D_SKEW		{	D_SKEW		 }
	|	D_TIMESKEW	{	D_TIMESKEW	 }
	|	D_WIDTH		{	D_WIDTH		 }
	|	EDGE		{	EDGE		 }
	|	EQUALS		{	EQUALS		 }
	|	FULLSKEW	{	FULLSKEW	 }
	|	GREATER		{	GREATER 	 }
	|	IDSTR		{	IDSTR $1	 }
	|	IF		{	IF		 }
	|	IF_NONE		{	IF_NONE		 }
	|	INTNUM		{	INTNUM $1	 }
	|	LBRACK		{	LBRACK		 }
	|	LPAREN		{	LPAREN		 }
	|	NEGEDGE		{	NEGEDGE		 }
	|	NOCHANGE	{	NOCHANGE	 }
	|	NOSHOWCANCELLED	{	NOSHOWCANCELLED	 }
	|	P_ANDANDAND	{	P_ANDANDAND	 }
	|	P_ANDAND	{	P_ANDAND	 }
	|	P_ASTGT		{	P_ASTGT		 }
	|	PATHPULSE	{	PATHPULSE	 }
	|	P_EQGT		{	P_EQGT		 }
	|	P_EQUAL		{	P_EQUAL		 }
	|	PLING		{	PLING		 }
	|	P_NOTEQUAL	{	P_NOTEQUAL	 }
	|	P_NXOR		{	P_NXOR		 }
	|	POSEDGE		{	POSEDGE		 }
	|	P_PLUSCOLON	{	P_PLUSCOLON	 }
	|	P_PLUSEQ	{	P_PLUSEQ	 }
	|	P_TILDE_VBAR	{	P_TILDE_VBAR	 }
	|	PULSESTYLE_ONDETECT { PULSESTYLE_ONDETECT}
	|	PULSESTYLE_ONEVENT { PULSESTYLE_ONEVENT  }
	|	RBRACK		{	RBRACK		 }
	|	RPAREN		{	RPAREN		 }
	|	SEMICOLON	{	SEMICOLON	 }
	|	SHOWCANCELLED	{	SHOWCANCELLED	 }
	|	SPECPARAM	{	SPECPARAM	 }
	|	TOKEN_EDGE01	{	TOKEN_EDGE01	 }
	|	TOKEN_EDGE_10	{	TOKEN_EDGE_10	 }
	|	TOKEN_ONE	{	TOKEN_ONE	 }
	|	TOKEN_ZERO	{	TOKEN_ZERO	 }
	|	Z_OR_X		{	Z_OR_X	""	 }

// IEEE: generate_region
generateRegion:
		GENERATE genTopBlock ENDGENERATE	{ DOUBLE (GENERATE, $2 ) }
	;

modOrGenItem:
		ALWAYS stmtBlock			{ DOUBLE (ALWAYS, $2 ) }
	|	FINAL stmtBlock				{ DOUBLE (FINAL, $2 ) }
	|	INITIAL stmtBlock			{ DOUBLE ( INITIAL, $2 ) }
	|	ASSIGN delayStrength AssignList SEMICOLON	{ TRIPLE ( ASSIGN, $2, TLIST $3 ) }
	|	DEFPARAM defpList SEMICOLON		{ DOUBLE (DEFPARAM, TLIST $2 ) }
	|	instDecl 				{ $1 }
	|	taskDecl 				{ $1 }
	|	funcDecl 				{ $1 }
	|	gateDecl 				{ $1 }
	|	attrDecl 				{ $1 }
	|	PortDecl 				{ $1 }
	|	varDecl 				{ $1 }
	|	tableDecl				{ $1 }
	|	concurrent_assertion_item		{ $1 }  // IEEE puts in modItem; seems silly
	|	clocking_declaration			{ $1 }
//	|	pslStmt					{ $1 }
	;

//************************************************
// Generates

// Because genItemList includes variable declarations, we dont need beginNamed
genItemBlock:
		genItem					{ $1 }
	|	genItemBegin				{ $1 }
	;

genTopBlock:
		genItemList				{ TLIST $1 }
	|	genItemBegin				{ $1 }
	;

genItemBegin:
		BEGIN genItemList END			{ TLIST $2 }
	|	BEGIN END				{ EMPTY }
	|	BEGIN COLON identifier genItemList END endLabelE	{ QUADRUPLE(GENITEM, $3, TLIST $4, $6) }
	|	BEGIN COLON identifier 	           END endLabelE	{ QUADRUPLE(GENITEM, $3, EMPTY, $5) }
	;

genItemList:
		genItem					{ [ $1 ] }
	|	genItem genItemList			{ $1 :: $2 }
	;

genItem:
		modOrGenItem 				{ $1 }
	|	CASE  LPAREN expr RPAREN genCaseListE ENDCASE	{ TRIPLE (GENCASE, $3, $5) }
	|	IF LPAREN expr RPAREN genItemBlock	%prec prLOWER_THAN_ELSE
			{ TRIPLE (IF, $3, $5) }
	|	IF LPAREN expr RPAREN genItemBlock ELSE genItemBlock
			{ QUADRUPLE (IF, $3, $5, $7) }
	|	FOR LPAREN varRefBase EQUALS expr SEMICOLON
			expr SEMICOLON varRefBase EQUALS expr RPAREN genItemBlock
			{ QUINTUPLE (FOR, TRIPLE (ASSIGNMENT, $3, $5), $7, TRIPLE (ASSIGNMENT, $9, $11), $13) }
	;

genCaseListE:
		/* empty */				{ EMPTY }
	|	genCaseList				{ TLIST $1 }
	;

genCaseList:
		caseCondList COLON genItemBlock		{ [ TRIPLE (GENCASECOND, TLIST $1, $3 ) ] }
	|	DEFAULT COLON genItemBlock		{ [ DOUBLE (DEFAULT, $3 ) ] }
	|	DEFAULT genItemBlock			{ [ DOUBLE (DEFAULT, $2 ) ] }
	|	genCaseList caseCondList COLON genItemBlock	{ TRIPLE (GENCASECOND, TLIST $2, $4) :: $1 }
	|       genCaseList DEFAULT genItemBlock		{ DOUBLE (DEFAULT, $3 ) :: $1 }
	|	genCaseList DEFAULT COLON genItemBlock		{ DOUBLE (DEFAULT, $4) :: $1 }
	;

//************************************************
// Assignments and register declarations

AssignList:
		AssignOne				{ [ $1 ] }
	|	AssignList COMMA AssignOne		{ $1 @ [ $3 ] }
	;

AssignOne:
		varRefDotBit EQUALS expr			{ TRIPLE (ASSIGNMENT, $1, $3 ) }
	|	LCURLY concIdList RCURLY EQUALS expr		{ TRIPLE (ASSIGNMENT, DOUBLE (CONCAT,TLIST $2), $5) }
	;

delayE:		/* empty */				{ EMPTY }
	|	delay					{ $1 } /* ignored */
	;

strengthList:
		/* empty */				{ [] }
	|	COMMA WEAK strengthList			{ WEAK $2 :: $3 }
	|	COMMA STRONG strengthList		{ STRONG $2 :: $3 }

delay:
		HASH dlyTerm
			{ DOUBLE (HASH, $2 ) } /* ignored */
	|	HASH LPAREN minTypMax RPAREN
			{ DOUBLE (HASH, $3 ) } /* ignored */
	|	HASH LPAREN minTypMax COMMA minTypMax RPAREN
			{ TRIPLE (HASH, $3, $5) } /* ignored */
	|	HASH LPAREN minTypMax COMMA minTypMax COMMA minTypMax RPAREN
			{ QUADRUPLE (HASH, $3, $5, $7) } /* ignored */
	;

dlyTerm:
		identifier 				{ $1 }
	|	floatnum 				{ $1 }
	|	INTNUM					{ FLOATNUM (float_of_int (int_of_string $1)) }
	;

// IEEE: mintypmax_expression and constant_mintypmax_expression
minTypMax:
		dlyTerm					{ $1 } /* ignored */
	|	dlyTerm COLON dlyTerm COLON dlyTerm	{ QUADRUPLE (MINTYPMAX, $1, $3, $5) } /* ignored */
	;

sigAndAttr:
		sigId sigAttrListE			{ DOUBLE ( $1, $2 ) }
	;

netSigList:
		netSig  				{ [ $1 ] }
	|	netSigList COMMA netSig		       	{ $1 @ [ $3 ] }
	;

netSig:
		sigId sigAttrListE			{ DOUBLE ( $1, $2 ) }
	|	sigId sigAttrListE EQUALS expr		{ TRIPLE ( $1, $2, $4 ) }
	|	identifier RangeList sigAttrListE	{ TRIPLE ( $1, TLIST $2, $3 ) }
	;

regsig:
		identifier RangeListE sigAttrListE			{ TRIPLE ($1, $2, $3 ) }
	|	identifier RangeListE EQUALS constExpr sigAttrListE	{ QUADRUPLE ($1, $2, $4, $5 ) }
	;

sigId:	identifier					{ $1 }
	;

sigAttrListE:	/* empty */				{ EMPTY }
	;

RangeListE:
		/* empty */    		               	{ EMPTY }
	|	RangeList 				{ TLIST $1 }
	;

RangeList:
		Anyrange				{ [ $1 ] }
        |	RangeList Anyrange			{ $1 @ [ $2 ] }
	;

regrangeE:
		/* empty */    		               	{ EMPTY }
	|	Anyrange 				{ $1 }
	;

Anyrange:
		LBRACK constExpr COLON constExpr RBRACK	{ RANGE( $2, $4) }
	;

delayrange:
		regrangeE delayE 			{ TRIPLE (EMPTY, $1, $2 ) }
	|	SCALARED regrangeE delayE 		{ TRIPLE (SCALARED, $2, $3 ) }
	|	VECTORED regrangeE delayE 		{ TRIPLE (VECTORED, $2, $3 ) }
	;

PortRangeE:
		/* empty */	                   	{ SCALAR }
	|	LBRACK constExpr RBRACK              	{ RANGE ($2, $2) }
	|	LBRACK constExpr COLON constExpr RBRACK	{ RANGE ($2, $4) }
	;

//************************************************
// Parameters

param:
		sigId sigAttrListE EQUALS expr		{ TRIPLE ( $1, $2, $4 ) }
	|	INTEGER sigId sigAttrListE EQUALS expr	{ TRIPLE ( $2, $3, $5 ) }
	;

paramList:
		param					{ [ $1 ] }
	|	paramList COMMA param			{ $1 @ [ $3 ] }
	;

// IEEE: list_of_defparam_assignments
defpList:
		defpOne					{ [ $1 ] }
	|	defpList COMMA defpOne			{ $1 @ [ $3 ] }
	;

defpOne:
		identifier DOT identifier EQUALS expr { TRIPLE ($1, $3 , $5 ) }
	;

//************************************************
// Instances

instDecl:
		identifier delayStrength instnameList SEMICOLON {
                  (match $1 with IDSTR id -> () | _ -> failwith "inst");
                  QUADRUPLE (MODINST, $1, $2, TLIST $3 ) }
	| 	identifier delayStrength LPAREN varRefDotBit COMMA gateUdpPinList RPAREN SEMICOLON {
                  (match $1 with IDSTR id -> () | _ -> failwith "inst");
                  QUADRUPLE (PRIMINST, $1, $2, TLIST ($4::$6) ) }
        ;

delayStrength:
		/* empty */				{ EMPTY }
	|	HASH dlyTerm				{ DOUBLE (HASH, $2 ) }
	|	HASH LPAREN cellpinList RPAREN		{ DOUBLE (HASH, TLIST $3) }
 	|	PWEAK strengthList RPAREN		{ TLIST ((WEAK $1 ) :: $2 ) }
	|	PWEAK strengthList RPAREN HASH dlyTerm	{ DOUBLE (TLIST ((WEAK $1 ) :: $2 ), DOUBLE (HASH, $5)) }
	|	PSTRONG strengthList RPAREN		{ TLIST ((STRONG $1 ) :: $2 ) }
	|	PSTRONG strengthList RPAREN HASH dlyTerm	{ DOUBLE (TLIST ((STRONG $1 ) :: $2 ), DOUBLE (HASH, $5)) }
	;

instnameList:
		instnameParen				{ [ $1 ] }
	|	instnameList COMMA instnameParen	{ $1 @ [ $3 ] }
	;

instnameParen:
		identifier instRangeE LPAREN cellpinList RPAREN	{ TRIPLE ($1, $2, TLIST $4 ) }
	|	identifier instRangeE 			{ DOUBLE ( $1, $2 ) }
	;

instRangeE:
		/* empty */				{ SCALAR }
	|	LBRACK constExpr COLON constExpr RBRACK	{ RANGE ($2, $4) }
	;

cellpinList:
		cellpinItList				{ $1 }
	;

cellpinItList:
		/* empty: ',,' is legal */		{ [ ] }
	|	cellpinItem				{ [ $1 ] }
	|	cellpinItList COMMA cellpinItem		{ $1 @ [ $3 ] }
	;

cellpinItem:
	|	P_DOTSTAR				{ P_DOTSTAR }
	|	DOT identifier				{ DOUBLE (CELLPIN, $2 ) }
	|	DOT identifier LPAREN RPAREN		{ DOUBLE (CELLPIN, $2 ) }
	|	DOT identifier LPAREN expr RPAREN	{ TRIPLE (CELLPIN, $2, $4) }
	|	expr					{ $1 }
	;

//************************************************
// EventControl lists

// IEEE: event_control
eventControl:
		AT LPAREN senList RPAREN		{ DOUBLE ( AT, (TLIST $3 ) ) }
	|	AT senitemVar				{ DOUBLE ( AT, $2 ) }
	|	AT LPAREN TIMES RPAREN			{ AT }  /* Verilog 2001 */
	|	AT TIMES				{ AT }  /* Verilog 2001 */
	|	delay					{ $1 }
	;

// IEEE: event_expression - split over several
senList:
		senitem					{ [ $1 ] }
	|	senList OR senitem			{ $1 @ [ $3 ] }
	|	senList COMMA senitem			{ $1 @ [ $3 ] }	/* Verilog 2001 */
	;

senitem:
		senitemEdge				{ $1 }
	|	senitemVar				{ $1 }
	|	LPAREN senitemVar RPAREN		{ $2 }
	;

senitemVar:
		varRefDotBit				{ $1 }
	;

senitemEdge:
		POSEDGE varRefDotBit			{ DOUBLE ( POSEDGE, $2 ) }
	|	NEGEDGE varRefDotBit			{ DOUBLE ( NEGEDGE, $2 ) }
	|	POSEDGE lparen varRefDotBit rparen	{ DOUBLE ( POSEDGE, $3 ) }
	|	NEGEDGE lparen varRefDotBit rparen	{ DOUBLE ( NEGEDGE, $3 ) }
	;

lparen:
                LPAREN        { }
//        |       TokenLPar     { }
        ;

rparen:
                RPAREN        { }
//        |       TokenRPar     { }
        ;

//************************************************
// Statements

stmtBlock:
		stmt					{ match $1 with TLIST _ -> $1 | _ -> TLIST [ $1 ] }
	|	BEGIN stmtList END			{ TLIST $2 }
	|	BEGIN END				{ TLIST [] }
	|	BEGIN COLON identifier varDeclListE stmtListE END endLabelE
							{ QUINTUPLE (NAMED, $3, TLIST $4, TLIST $5, $7) }
	;

stmtListE:
		/* empty */				{ [ ] }
	|	stmtList				{ $1 }
	;

stmtList:
		stmtBlock				{ [ $1 ] }
	|	stmtList stmtBlock			{ $1 @ [ $2 ] }
	;

stmt:
		SEMICOLON				{ EMPTY }
	|	labeledStmt				{ $1 }
	|	identifier COLON labeledStmt		{ DOUBLE ($1, $3 ) }

	|	eventControl stmtBlock			{ DOUBLE ($1, $2 ) }

	|	varRefDotBit P_LTE delayE expr SEMICOLON
			{ QUADRUPLE (DLYASSIGNMENT, $1, $3, $4 ) }
	|	varRefDotBit EQUALS delayE expr SEMICOLON
			{ QUADRUPLE ( ASSIGNMENT, $1, $3, $4 ) }
	|	varRefDotBit EQUALS D_FOPEN LPAREN expr RPAREN SEMICOLON
			{ TRIPLE (D_FOPEN, $1, $5) }
	|	varRefDotBit EQUALS D_FOPEN LPAREN expr COMMA expr RPAREN SEMICOLON
			{ QUADRUPLE (D_FOPEN, $1, $5, $7) }
	|	ASSIGN varRefDotBit EQUALS delayStrength expr SEMICOLON
			{ QUADRUPLE (ASSIGN, $2, $4, $5 ) }
	|	ASSIGN LCURLY concIdList RCURLY EQUALS delayE expr SEMICOLON
			{ QUADRUPLE (ASSIGNMENT, DOUBLE (CONCAT,TLIST $3), $6, $7 ) }
	|	DEASSIGN varRefDotBit SEMICOLON
			{ DOUBLE (DEASSIGN, $2 ) }
	|	LCURLY concIdList RCURLY P_LTE delayE expr SEMICOLON
			{ QUADRUPLE (DLYASSIGNMENT, DOUBLE (CONCAT,TLIST $2), $5, $6 ) }
	|	LCURLY concIdList RCURLY EQUALS delayE expr SEMICOLON
			{ QUADRUPLE (ASSIGNMENT, DOUBLE (CONCAT,TLIST $2), $5, $6 ) }
	|	D_C LPAREN cStrList RPAREN SEMICOLON
			{ TLIST $3 }
	|	D_FCLOSE LPAREN varRefDotBit RPAREN SEMICOLON
			{ DOUBLE (D_FCLOSE, $3) }
	|	D_FFLUSH SEMICOLON
			{ D_FFLUSH }
	|	D_FFLUSH LPAREN RPAREN SEMICOLON
			{ D_FFLUSH }
	|	D_FFLUSH LPAREN varRefDotBit RPAREN SEMICOLON
			{ DOUBLE (D_FFLUSH, $3) }
	|	D_FINISH parenE SEMICOLON
			{ DOUBLE (D_FINISH, $2) }
	|	D_FINISH LPAREN expr RPAREN SEMICOLON
			{ DOUBLE (D_FINISH, $3) }
	|	D_STOP parenE SEMICOLON
			{ DOUBLE (D_STOP, $2 ) }
	|	D_STOP LPAREN expr RPAREN SEMICOLON
			{ DOUBLE (D_STOP, $3 ) }
	|	stateCaseForIf
			{ $1 }
	|	taskRef SEMICOLON
			{ $1 }
	|	D_DISPLAY  parenE SEMICOLON
			{ DOUBLE (D_DISPLAY, $2 ) }
	|	D_DISPLAY  LPAREN ASCNUM commaEListE RPAREN SEMICOLON
			{ TRIPLE (D_DISPLAY, ASCNUM $3, $4); }
	|	D_WRITE LPAREN ASCNUM commaEListE RPAREN SEMICOLON
			{ TRIPLE (D_WRITE,ASCNUM $3,$4) }
	|	D_FDISPLAY LPAREN varRefDotBit monListE RPAREN SEMICOLON
		 	{ TRIPLE ( D_FDISPLAY, $3, $4 ) }
	|	D_FWRITE   LPAREN varRefDotBit monListE RPAREN SEMICOLON
			{ TRIPLE ( D_FWRITE, $3, $4 ) }
	|	D_FWRITEH  LPAREN varRefDotBit monListE RPAREN SEMICOLON
			{ TRIPLE ( D_FWRITEH, $3, $4 ) }
	|	D_INFO	    parenE SEMICOLON
			{ DOUBLE (D_INFO, $2) }
	|	D_INFO	    LPAREN ASCNUM commaEListE RPAREN SEMICOLON
			{ DOUBLE (D_INFO, $4) }
	|	D_WARNING  parenE SEMICOLON
			{ DOUBLE (D_WARNING, $2) }
	|	D_WARNING  LPAREN ASCNUM commaEListE RPAREN SEMICOLON
			{ DOUBLE (D_WARNING, $4) }
	|	D_ERROR    parenE SEMICOLON
			{ DOUBLE (D_ERROR, $2) }
	|	D_ERROR    LPAREN ASCNUM commaEListE RPAREN SEMICOLON
			{ DOUBLE (D_ERROR, $4) }
	|	D_FATAL    parenE SEMICOLON
			{ DOUBLE (D_FATAL, $2) }
	|	D_FATAL    LPAREN expr RPAREN SEMICOLON
			{ DOUBLE (D_FATAL, $3) }
	|	D_FATAL    LPAREN expr COMMA ASCNUM commaEListE RPAREN SEMICOLON
			{ TRIPLE ( D_FATAL, $3, $6 ) }
	|	readmem LPAREN expr COMMA varRefMem RPAREN SEMICOLON
			{ TRIPLE ( D_READMEMB, $3, $5 ) }
	|	readmem LPAREN expr COMMA varRefMem COMMA expr RPAREN SEMICOLON
			{ QUADRUPLE ( D_READMEMB, $3, $5, $7 ) }
	|	readmem LPAREN expr COMMA varRefMem COMMA expr COMMA expr RPAREN SEMICOLON
			{ QUINTUPLE ( D_READMEMB, $3, $5, $7, $9 ) }
	|	P_MINUSGT varRefDotBit SEMICOLON
			{ DOUBLE ( P_MINUSGT, $2 ) }	
	|	DISABLE varRefDotBit SEMICOLON
			{ DOUBLE ( DISABLE, $2 ) }	
	|	D_MONITOR  LPAREN monList RPAREN SEMICOLON
			{ DOUBLE (D_MONITOR, TLIST $3); }
	|	REPEAT LPAREN expr RPAREN stmtBlock	{ TRIPLE ( REPEAT, $3, $5 ) }
	|	FOREVER stmtBlock	    		{ DOUBLE ( FOREVER, $2 ) }
	|	preproc					{ $1 }

;

//************************************************
// Case/If

stateCaseForIf:
		case LPAREN expr RPAREN caseAttrE caseListE ENDCASE
			{ QUADRUPLE ($1, $3, $5, $6) }
	|	IF LPAREN expr RPAREN stmtBlock	%prec prLOWER_THAN_ELSE
			{ TRIPLE (IF, $3, $5 ) }
	|	IF LPAREN expr RPAREN stmtBlock ELSE stmtBlock
			{ QUADRUPLE (IF, $3, $5, $7 ) }
	|	FOR LPAREN varRefBase EQUALS expr SEMICOLON
			expr SEMICOLON varRefBase EQUALS expr RPAREN stmtBlock
			{ QUINTUPLE (FOR, TRIPLE (ASSIGNMENT, $3, $5), $7, TRIPLE (ASSIGNMENT, $9, $11), $13 ) }
	|	WHILE LPAREN expr RPAREN stmtBlock
			{ TRIPLE (WHILE, $3, $5 ) }
	|	DO stmtBlock WHILE LPAREN expr RPAREN
			{ TRIPLE (DO, $2, $5 ) }
	;

case:
	 	CASE		{ CASE }
	|	CASEX		{ CASEX }
	|	CASEZ		{ CASEZ }
	;

caseAttrE: 	/*empty*/				{ EMPTY }
	;

caseListE:
		/* empty */				{ EMPTY }
	|	caseList				{ TLIST (List.rev $1) }
	;

caseList:
		caseCondList COLON stmtBlock		{ [ TRIPLE (CASECOND, TLIST $1, $3 ) ] }
	|	DEFAULT COLON stmtBlock			{ [ DOUBLE (DEFAULT, $3 ) ] }
	|	DEFAULT stmtBlock			{ [ DOUBLE (DEFAULT, $2 ) ] }
	|	caseList caseCondList COLON stmtBlock	{ TRIPLE (CASECOND, TLIST $2, $4) :: $1 }
	|       caseList DEFAULT stmtBlock		{ DOUBLE (DEFAULT, $3 ) :: $1 }
	|	caseList DEFAULT COLON stmtBlock	{ DOUBLE (DEFAULT, $4) :: $1 }
	;

caseCondList:
		expr 					{ [ $1 ] }
	|	caseCondList COMMA expr			{ $1 @ [ $3 ] }
	;

//************************************************
// Functions/tasks

taskRef:
		idDotted		 		{ TRIPLE (TASKREF, $1, EMPTY ) }
	|	idDotted LPAREN exprList RPAREN		{ TRIPLE (TASKREF, $1, TLIST $3 ) }
	;

funcRef:
		idDotted LPAREN exprList RPAREN		{ TRIPLE (FUNCREF, $1, TLIST $3 ) }
	;

taskDecl:
		TASK lifetimeE identifier taskArgs SEMICOLON taskVarList stmtBlock ENDTASK endLabelE
			{ SEPTUPLE ( TASK, $2, $3, $4, TLIST $6, $7, $9 ) }
	;

funcDecl:
		FUNCTION lifetimeE signedE funcTypeE identifier funcArgs SEMICOLON funcVarList stmtBlock ENDFUNCTION endLabelE
			{ OCTUPLE (FUNCTION, $2, $4, $5, $6, TLIST $8, $9, $11 ) }
	;

signedE:	SIGNED					{ SIGNED }
	|	/* empty */				{ EMPTY }
	;

// IEEE: lifetime - plus empty
lifetimeE:	/* empty */		 		{ EMPTY }
	|	STATIC			 		{ STATIC }
	|	AUTOMATIC		 		{ AUTOMATIC }
	;

funcArgs:
		LPAREN PortV2kArgs RPAREN		{ TLIST $2 }
	|	/* empty */				{ EMPTY }
	;

taskArgs:
		LPAREN PortV2kArgs RPAREN		{ TLIST $2 }
	|	/* empty */				{ EMPTY }
	;

funcTypeE:
		/* empty */				{ SCALAR }
	|	REAL					{ REAL }
	|	INTEGER					{ INTEGER }
	|	LBRACK constExpr COLON constExpr RBRACK	{ RANGE ( $2, $4 ) }
	;

funcVarList:
		/* empty */				{ [] }
	|	funcVar funcVarList			{ $1 :: $2 }
	;

taskVarList:
		/* empty */				{ [] }
	|	taskVar taskVarList			{ $1 :: $2 }
	;

funcVar:
		PortDecl				{ $1 }
	|	varDecl 				{ $1 }
	;

taskVar:
		PortDecl				{ $1 }
	|	varDecl 				{ $1 }
	;

parenE:		/* empty */				{ EMPTY }
	|	LPAREN RPAREN				{ EMPTY }
	;

//************************************************
// Expressions

constExpr:
		expr					{ $1 }
	;

exprNoStr:
		expr P_OROR expr			{ TRIPLE ( P_OROR, $1, $3 ) }
	|	expr P_ANDAND expr			{ TRIPLE ( P_ANDAND, $1, $3 ) }
	|	expr P_AMPERSAND expr			{ TRIPLE ( AND, $1, $3 ) }
	|	expr P_VBAR expr			{ TRIPLE ( OR, $1, $3 ) }
	|	expr P_NAND expr			{ TRIPLE ( P_NAND, $1, $3 ) }
	|	expr P_NOR expr				{ TRIPLE ( P_NOR, $1, $3 ) }
	|	expr P_CARET expr			{ TRIPLE ( XOR, $1, $3 ) }
	|	expr P_XNOR expr			{ TRIPLE ( P_XNOR, $1, $3 ) }
	|	expr P_EQUAL expr			{ TRIPLE ( P_EQUAL, $1, $3 ) }
	|	expr P_NOTEQUAL expr			{ TRIPLE ( P_NOTEQUAL, $1, $3 ) }
	|	expr P_CASEEQUAL expr			{ TRIPLE ( P_CASEEQUAL, $1, $3 ) }
	|	expr P_CASENOTEQUAL expr		{ TRIPLE ( P_CASENOTEQUAL, $1, $3 ) }
	|	expr P_WILDEQUAL expr			{ TRIPLE ( P_WILDEQUAL, $1, $3 ) }
	|	expr P_WILDNOTEQUAL expr		{ TRIPLE ( P_WILDNOTEQUAL, $1, $3 ) }
	|	expr GREATER expr			{ TRIPLE ( GREATER, $1, $3 ) }
	|	expr LESS expr				{ TRIPLE ( LESS, $1, $3 ) }
	|	expr P_GTE expr				{ TRIPLE ( P_GTE, $1, $3 ) }
	|	expr P_LTE expr				{ TRIPLE ( P_LTE, $1, $3 ) }
	|	expr P_SLEFT expr			{ TRIPLE ( P_SLEFT, $1, $3 ) }
	|	expr P_SRIGHT expr			{ TRIPLE ( P_SRIGHT, $1, $3 ) }
	|	expr P_SSRIGHT expr			{ TRIPLE ( P_SSRIGHT, $1, $3 ) }
	|	expr PLUS expr				{ TRIPLE ( PLUS, $1, $3 ) }
	|	expr MINUS expr				{ TRIPLE ( MINUS, $1, $3 ) }
	|	expr TIMES expr				{ TRIPLE ( TIMES, $1, $3 ) }
	|	expr DIVIDE expr			{ TRIPLE ( DIVIDE, $1, $3 ) }
	|	expr MODULO expr			{ TRIPLE ( MODULO, $1, $3 ) }
	|	expr P_POW expr				{ TRIPLE ( P_POW, $1, $3 ) }
	|	MINUS expr	%prec prUNARYARITH	{ match $2 with
		      		      			  | FLOATNUM f -> FLOATNUM(-. f)
							  | INTNUM i -> INTNUM(string_of_int(-(int_of_string i)))
							  | INT i -> INT(-i)
		      		      			  | _ -> DOUBLE (MINUS, $2 )
							  }
	|	PLUS expr        %prec prUNARYARITH	{ DOUBLE (PLUS, $2 ) }
	|	P_AMPERSAND expr %prec prREDUCTION	{ DOUBLE (AND, $2 ) }
	|	P_VBAR expr      %prec prREDUCTION	{ DOUBLE (OR, $2 ) }
	|	P_CARET expr     %prec prREDUCTION	{ DOUBLE (XOR, $2 ) }
	|	P_XNOR expr      %prec prREDUCTION	{ DOUBLE (P_XNOR, $2 ) }
	|	P_NAND expr      %prec prREDUCTION	{ DOUBLE (P_NAND, $2 ) }
	|	P_NOR expr       %prec prREDUCTION	{ DOUBLE (P_NOR, $2 ) }
	|	PLING expr       %prec prNEGATION	{ DOUBLE (PLING, $2 ) }
	|	P_TILDE expr     %prec prNEGATION	{ DOUBLE (NOT, $2 ) }

	|	expr QUERY expr COLON expr		{ QUADRUPLE (QUERY, $1, $3, $5 ) }
	|	LPAREN expr RPAREN			{ DOUBLE (LPAREN, $2) }
	|	LCURLY cateList RCURLY			{ DOUBLE (CONCAT, TLIST $2) }
	|	LCURLY constExpr LCURLY cateList RCURLY RCURLY
							{ TRIPLE (CONCAT, $2, TLIST $4) }
	|	D_BITS LPAREN expr RPAREN		{ DOUBLE (D_BITS, $3 ) }
	|	D_C LPAREN cStrList RPAREN		{ DOUBLE (D_C, TLIST $3 ) }
	|	D_CLOG2 LPAREN expr RPAREN		{ DOUBLE (D_CLOG2, $3 ) }
	|	D_COUNTDRIVERS LPAREN expr RPAREN	{ DOUBLE (D_COUNTDRIVERS, $3 ) }
	|	D_COUNTONES LPAREN expr RPAREN		{ DOUBLE (D_COUNTONES, $3 ) }
	|	D_FEOF LPAREN expr RPAREN		{ DOUBLE (D_FEOF, $3 ) }
	|	D_FGETC LPAREN expr RPAREN		{ DOUBLE (D_FGETC, $3 ) }
	|	D_FGETS LPAREN varRefDotBit COMMA expr RPAREN
							{ TRIPLE (D_FGETS, $3, $5) }
	|	D_FSCANF LPAREN expr COMMA ASCNUM commaVRDListE RPAREN
							{ TRIPLE (D_FSCANF, $3, $6) }
	|	D_SSCANF LPAREN expr COMMA ASCNUM commaVRDListE RPAREN
							{ TRIPLE (D_SSCANF, $3, $6) }
	|	D_ISUNKNOWN LPAREN expr RPAREN		{ DOUBLE (D_ISUNKNOWN, $3 ) }
	|	D_ONEHOT LPAREN expr RPAREN		{ DOUBLE (D_ONEHOT, $3 ) }
	|	D_ONEHOT0 LPAREN expr RPAREN		{ DOUBLE (D_ONEHOT0, $3 ) }
	|	D_RANDOM LPAREN expr RPAREN		{ DOUBLE (D_RANDOM, $3 ) }
	|	D_RANDOM LPAREN RPAREN			{ D_RANDOM }
	|	D_RANDOM				{ D_RANDOM }
	|	D_SIGNED LPAREN expr RPAREN		{ DOUBLE (D_SIGNED, $3 ) }
	|	D_STIME					{ D_STIME }
	|	D_TIME					{ D_TIME }
	|	D_TEST_PLUSARGS LPAREN expr RPAREN	{ DOUBLE (D_TEST_PLUSARGS, $3 ) }
	|	D_UNSIGNED LPAREN expr RPAREN		{ DOUBLE (D_UNSIGNED, $3 ) }
	|	funcRef					{ $1 }
	|	INTNUM					{ INT (int_of_string $1) }
	|	BINNUM					{ BINNUM $1 }
	|	OCTNUM					{ OCTNUM $1 }
	|	DECNUM					{ DECNUM $1 }
	|	HEXNUM					{ HEXNUM $1 }
	|	varRefDotBit	  			{ $1 }
	;

// Generic expressions
expr:
		exprNoStr				{ $1 }
	|	strAsInt				{ $1 }
	|	floatnum				{ $1 }
	|	preproc					{ $1 }
	;

// PLI calls exclude "" as integers, they're strings
// For $c("foo","bar") we want "bar" as a string, not a Verilog integer.
exprStrText:
		exprNoStr				{ $1 }
	|	strAsText				{ $1 }
	;

cStrList:
		exprStrText				{ [ $1 ] }
	|	exprStrText COMMA cStrList		{ $1 :: $3 }
	;

cateList:
		expr					{ [ $1 ] }
	|	cateList COMMA expr			{ $1 @ [ $3 ] }
	;

exprList:
		/* empty */				{ [] }
	|	expr					{ [ $1 ] }
	|	exprList COMMA expr			{ $1 @ [ $3 ] }
	;

commaEListE:
		/* empty */				{ EMPTY }
	|	COMMA exprList				{ TLIST $2 }
	;

vrdList:
		varRefDotBit				{ [ $1 ] }
	|	vrdList COMMA varRefDotBit		{ $1 @ [ $3 ] }
	;

commaVRDListE:
		/* empty */				{ EMPTY }
	|	COMMA vrdList				{ DOUBLE (COMMA, TLIST $2 ) }
	;

monListE:
		/* empty */				{ EMPTY }
	|	COMMA monList				{ TLIST $2 }

monList:
		monText					{ [ $1 ] }
	|	monText COMMA monList			{ $1 :: $3 }
	;

monText:
		/* empty */				{ ASCNUM "" }
	|	exprNoStr				{ $1 }
	|	strAsText				{ $1 }
	;

attrDecl:
		D_ATTRIBUTE LPAREN exprList RPAREN SEMICOLON	{ DOUBLE (D_ATTRIBUTE, TLIST $3 ) }

//************************************************
// Gate declarations

gateDecl:
		BUF  delayStrength gateBufList SEMICOLON		{ TRIPLE (BUF, $2, TLIST $3 ) }
	|	BUFIF delayStrength gateBufIfList SEMICOLON		{ TRIPLE (BUFIF $1, $2, TLIST $3 ) }
	|	NOTIF delayStrength gateBufIfList SEMICOLON		{ TRIPLE (NOTIF $1, $2, TLIST $3 ) }
	|	NOT  delayStrength gateNotList SEMICOLON		{ TRIPLE (NOT, $2, TLIST $3 ) }
	|	AND  delayStrength gateAndList SEMICOLON		{ TRIPLE (AND, $2, TLIST $3 ) }
	|	NAND delayStrength gateNandList SEMICOLON		{ TRIPLE (NAND, $2, TLIST $3 ) }
	|	OR   delayStrength gateOrList SEMICOLON			{ TRIPLE (OR, $2, TLIST $3 ) }
	|	NOR  delayStrength gateNorList SEMICOLON		{ TRIPLE (NOR, $2, TLIST $3 ) }
	|	XOR  delayStrength gateXorList SEMICOLON		{ TRIPLE (XOR, $2, TLIST $3 ) }
	|	XNOR delayStrength gateXnorList SEMICOLON		{ TRIPLE (XNOR, $2, TLIST $3 ) }
	|	PULLUP delayStrength gatePullupList SEMICOLON		{ TRIPLE (PULLUP, $2, TLIST $3 ) }
	|	NMOS delayStrength gateMosList SEMICOLON		{ TRIPLE (NMOS, $2, TLIST $3 ) }
	|	PMOS delayStrength gateMosList SEMICOLON		{ TRIPLE (PMOS, $2, TLIST $3 ) }
	|	TRAN delayStrength gateTranList SEMICOLON		{ TRIPLE (TRAN, $2, TLIST $3 ) }
	|	TRANIF delayStrength gateTranIfList SEMICOLON		{ TRIPLE (TRANIF $1, $2, TLIST $3 ) }
	;

gateMosList:
		gateMos 				{ [ $1 ] }
	|	gateMosList COMMA gateMos		{ $1 @ [ $3 ] }
	;

gatePullupList:
		gatePullup 				{ [ $1 ] }
	|	gatePullupList COMMA gatePullup		{ $1 @ [ $3 ] }
	;

gateBufList:
		gateBuf 				{ [ $1 ] }
	|	gateBufList COMMA gateBuf		{ $1 @ [ $3 ] }
	;

gateBufIfList:
		gateBufIf 				{ [ $1 ] }
	|	gateBufIfList COMMA gateBufIf		{ $1 @ [ $3 ] }
	;

gateTranIfList:
		gateTranIf 				{ [ $1 ] }
	|	gateTranIfList COMMA gateTranIf		{ $1 @ [ $3 ] }
	;

gateTranList:
		gateTran 				{ [ $1 ] }
	|	gateTranIfList COMMA gateTran		{ $1 @ [ $3 ] }
	;

gateNotList:
		gateNot 				{ [ $1 ] }
	|	gateNotList COMMA gateNot		{ $1 @ [ $3 ] }
	;

gateAndList:
		gateAnd 				{ [ $1 ] }
	|	gateAndList COMMA gateAnd		{ $1 @ [ $3 ] }
	;
gateNandList:
		gateNand 				{ [ $1 ] }
	|	gateNandList COMMA gateNand		{ $1 @ [ $3 ] }
	;
gateOrList:
		gateOr 					{ [ $1 ] }
	|	gateOrList COMMA gateOr			{ $1 @ [ $3 ] }
	;
gateNorList:
		gateNor 				{ [ $1 ] }
	|	gateNorList COMMA gateNor		{ $1 @ [ $3 ] }
	;
gateXorList:
		gateXor 				{ [ $1 ] }
	|	gateXorList COMMA gateXor		{ $1 @ [ $3 ] }
	;
gateXnorList:
		gateXnor 				{ [ $1 ] }
	|	gateXnorList COMMA gateXnor		{ $1 @ [ $3 ] }
	;

gateMos:	gateIdE instRangeE LPAREN varRefDotBit COMMA varRefDotBit COMMA expr RPAREN
							{ QUINTUPLE ($1, $2, $4, $6, $8 ) }
	;

gateTran:	gateIdE instRangeE LPAREN varRefDotBit COMMA varRefDotBit RPAREN
							{ QUADRUPLE ($1, $2, $4, $6 ) }
	;

gateTranIf:	gateIdE instRangeE LPAREN varRefDotBit COMMA varRefDotBit COMMA expr RPAREN
							{ QUINTUPLE ($1, $2, $4, $6, $8 ) }
	;

gatePullup:	gateIdE instRangeE LPAREN varRefDotBit RPAREN
							{ TRIPLE ($1, $2, $4 ) }
	;

gateBuf:	gateIdE instRangeE LPAREN varRefDotBit COMMA expr RPAREN
							{ QUADRUPLE ($1, $2, $4, $6 ) }
	;

gateBufIf:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateBufIfPinList RPAREN
							{ QUADRUPLE ($1, $2, $4, TLIST $6 ) }
	;

gateNot:	gateIdE instRangeE LPAREN varRefDotBit COMMA expr RPAREN
							{ QUADRUPLE ($1 , $2, $4 , $6 ) }
	;

gateAnd:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateAndPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateNand:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateAndPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateOr:		gateIdE instRangeE LPAREN varRefDotBit COMMA gateOrPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateNor:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateOrPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateXor:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateXorPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateXnor:	gateIdE instRangeE LPAREN varRefDotBit COMMA gateXorPinList RPAREN
							{ QUADRUPLE ($1 , $2, $4 , TLIST $6 ) }
	;

gateIdE:	/*empty*/				{ EMPTY }
	|	identifier				{ $1 }
	;

gateBufIfPinList:
		expr 					{ [ $1 ] }
	|	gateBufIfPinList COMMA expr		{ $1 @ [ $3 ] }
	;

gateAndPinList:
		expr 					{ [ $1 ] }
	|	gateAndPinList COMMA expr		{ $1 @ [ $3 ] }
	;

gateOrPinList:
		expr 					{ [ $1 ] }
	|	gateOrPinList COMMA expr		{ $1 @ [ $3 ] }
	;

gateXorPinList:
		expr 					{ [ $1 ] }
	|	gateXorPinList COMMA expr		{ $1 @ [ $3 ] }
	;

gateUdpPinList:
		expr 					{ [ $1 ] }
	|	gateUdpPinList COMMA expr		{ $1 @ [ $3 ] }
	;

//************************************************
// Tables
// parsed but not supported

tableDecl:	TABLE trowList ENDTABLE 		{ DOUBLE (TABLE, TLIST $2) }
	;

trowList:
		/* empty */				{ [ ] }
	|	trow					{ [ $1 ] }
	|	trow SEMICOLON trowList			{ $1 :: $3 }
	;

trow:		tinList COLON tregoutList			{ DOUBLE (TLIST $1,TLIST $3) }
	|	tinList COLON tregoutList COLON tregoutList	{ TRIPLE (TLIST $1,TLIST $3,TLIST $5) }

tinList:	tin					{ [ $1 ] }
	|	tin tinList				{ $1 :: $2 }
	;

tin:		INTNUM					{ BINNUM $1 }
	|	TIMES					{ TIMES }
	|	QUERY					{ QUERY }
	|	LPAREN edge2 RPAREN			{ TEDGE((fst $2),(snd $2)) }
	|	LPAREN edge1 edge1 RPAREN		{ TEDGE(($2),($2)) }
	|	IDSTR					{ BINNUM $1 }
	;

edge1:		INTNUM					{ $1.[0] }
	|	IDSTR					{ $1.[0] }
	|	QUERY					{ '?' }
	|	ILLEGAL					{ $1 }
	;

edge2:		INTNUM					{ ($1.[0],$1.[1]) }
	|	IDSTR					{ ($1.[0],$1.[1]) }
	;

tregoutList:	tregout					{ [ $1 ] }
	|	tregout tregoutList			{ $1 :: $2 }
	;

// merged to prevent conflicts

tregout:	INTNUM					{ BINNUM $1 }
	|	MINUS					{ MINUS }
	|	QUERY					{ QUERY }
	|	IDSTR					{ BINNUM $1 }
	;

//************************************************
// IDs

// VarRef to a Memory
varRefMem:
		idDotted				{ $1 }
	;

// VarRef to dotted, and/or arrayed, and/or bit-ranged variable
varRefDotBit:
		idDotted				{ $1 }
//	|	TokenProp                               { DOTTED [IDSTR $1] }
	;

idDotted:
		idArrayed 				{ $1 }
	|	idDotted DOT idArrayed	 		
		{ match $1 with DOTTED items -> DOTTED (items @ [$3]) | _ -> DOTTED [$1;$3] }
	;

// Single component of dotted path, maybe [#].
// Due to lookahead constraints, we cant know if [:] or [+:] are valid (last dotted part),
// well assume so and cleanup later.
idArrayed:
		identifier				{ $1 }
	|	idArrayed LBRACK expr RBRACK		{ TRIPLE (BITSEL, $1, $3 ) }  // Or AstArraySel, dont know et.
	|	idArrayed LBRACK constExpr COLON constExpr RBRACK
							{ QUADRUPLE (PARTSEL, $1 , $3 , $5 ) }
	|	idArrayed LBRACK expr P_PLUSCOLON  constExpr RBRACK
							{ QUADRUPLE (P_PLUSCOLON, $1 , $3 , $5 ) }
	|	idArrayed LBRACK expr P_MINUSCOLON constExpr RBRACK
							{ QUADRUPLE (P_MINUSCOLON, $1, $3, $5 ) }
	;

// VarRef without any dots or vectorization
varRefBase:
		identifier				{ $1 }
	;

strAsInt:
		ASCNUM					{ ASCNUM $1 }
	;

strAsText:
		ASCNUM					{ ASCNUM $1 }
	;

floatnum:
	|	FLOATNUM				{ FLOATNUM ($1) }
	;

concIdList:
		varRefDotBit				{ [ $1 ] }
	|	concIdList COMMA varRefDotBit		{ $1 @ [ $3 ] }
	;

endLabelE:	/* empty */				{ EMPTY }
	|	COLON identifier			{ DOUBLE (ENDLABEL, $2 ) }
	|	ENDOFFILE				{ ENDOFFILE }
	;

//************************************************
// Asserts

labeledStmt:
		AssertStmt				{ $1 }
	;

clocking_declaration:		// IEEE: clocking_declaration  (INCOMPLETE)
		DEFAULT CLOCKING AT LPAREN senitemEdge RPAREN SEMICOLON ENDCLOCKING
							{ CLOCKING }
	;

concurrent_assertion_item:	// IEEE: concurrent_assertion_item  (complete)
		concurrent_assertion_statement		{ $1 }
	|	identifier COLON concurrent_assertion_statement
							{ $3 }
	;

concurrent_assertion_statement:	// IEEE: concurrent_assertion_statement  (INCOMPLETE)
		cover_property_statement		{ COVER }
	;

cover_property_statement:	// IEEE: cover_property_statement (complete)
		COVER PROPERTY LPAREN property_spec RPAREN stmtBlock
							{ COVER }
	;

property_spec:			// IEEE: property_spec
		AT LPAREN senitemEdge RPAREN property_spec_disable expr
							{ DISABLE }
	|	property_spec_disable expr	 	{ DISABLE }
	;

property_spec_disable:
		/* empty */				{ EMPTY }
	|	DISABLE IFF LPAREN expr RPAREN		{ DISABLE }
	;

AssertStmt:
		ASSERT LPAREN expr RPAREN stmtBlock %prec prLOWER_THAN_ELSE
							{ ASSERT }
	|	ASSERT LPAREN expr RPAREN           ELSE stmtBlock
							{ ASSERT }
	|	ASSERT LPAREN expr RPAREN stmtBlock ELSE stmtBlock
							{ ASSERT }
	;

/*
//************************************************
// PSL Statements

pslStmt:
		PSL pslDir 		{ DOUBLE(PSL, $2) }
	|	PSL pslDecl 		{ DOUBLE(PSL, $2) }
	;

pslDir:
		PSL_PROPERTY TokenProp TokenCol pslProp	{ TRIPLE(PSL_PROPERTY, IDSTR $2, $4) }
	|	TokenProp TokenCol pslDirOne		{ DOUBLE(IDSTR $1, $3) }
	|	pslDirOne		       		{ $1 }
	;

pslDirOne:
		PSL_ASSERT pslProp TokenScol				{ DOUBLE(PSL_ASSERT,$2) }
	|	PSL_ASSERT pslProp PSL_REPORT yaSTRING TokenScol	{ TRIPLE(PSL_ASSERT,$2,$4) }
	|	PSL_COVER  pslProp TokenScol				{ DOUBLE(PSL_COVER,$2) }
	|	PSL_COVER  pslProp PSL_REPORT yaSTRING TokenScol	{ TRIPLE(PSL_COVER,$2,$4) }
	;

pslDecl:
		PSL_DEFAULT PSL_CLOCK TokenEQ senitemEdge TokenScol		{ DOUBLE(PSL_CLOCK, $4) }
	|	PSL_DEFAULT PSL_CLOCK TokenEQ lparen senitemEdge rparen TokenScol	{ DOUBLE(PSL_CLOCK, DOUBLE(LPAREN, $5)) }
	;

yaSTRING:
		ASCNUM					{ ASCNUM $1 }
	;

//************************************************
// PSL Properties, Sequences and SEREs
// Don't use '{' or '}'; in PSL they're TokenLBr and TokenRBr to avoid expr concatenates

pslProp:
		pslSequence				{ $1 }
	|	PSL_NEVER pslSequence			{ DOUBLE(PSL_NEVER, $2) }
	|	PSL_ALWAYS pslSequence			{ DOUBLE(PSL_ALWAYS, $2) }
	|	PSL_ALWAYS lparen pslSequence rparen	{ DOUBLE(PSL_ALWAYS, $3) }
	|	pslSequence AT lparen senitemEdge rparen { DOUBLE($1,DOUBLE(LPAREN,$4)) }  // or pslSequence @ ...?
	|	PSL_ALWAYS pslSequence AT lparen senitemEdge rparen { TRIPLE(PSL_ALWAYS,$2,DOUBLE(LPAREN,$5)) }  // or pslSequence @ ...?
	;

pslSequence:
		TokenLBr pslSere TokenRBr		{ $2 }
	;

pslSere:
		pslExpr					{ $1 }
	|	pslSequence				{ $1 }  // Sequence containing sequence
	;

// Undocumented PSL rule is that {} is always a SERE; concatenation is not allowed.
// This can be bypassed with the _(...) embedding of any arbitrary expression.
pslExpr:
//		exprPsl					{ PSL_CONTAINER $1 }
		pslSequence				{ $1 }
	|	P_TRUE					{ P_TRUE }
	;

exprPsl:
  | TokenLPar exprPsl TokenRPar   { $2 }

	| TokenProp                { Prop $1 }
	| TokenProp TokenEQ TokenProp  { Prop ("("^$1^"="^$3^")") }
	| TokenProp TokenEEQ TokenProp  { Prop ("("^$1^"=="^$3^")") }
	| TokenProp TokenNEQ TokenProp  { Prop ("("^$1^"!="^$3^")") }

  | TokenFin exprPsl            { OpF $2 }
	| TokenGen exprPsl            { OpG $2 }
  | TokenOpO exprPsl            { OpO $2 }
  | TokenOpH exprPsl            { OpH $2 }
  | TokenNeg exprPsl            { OpNeg $2 }
  | TokenNext exprPsl           { OpX $2 }
  | TokenYest exprPsl           { OpY $2 }
  | TokenZest exprPsl           { OpZ $2 }

  | exprPsl TokenUnt exprPsl       { OpU ($1, $3) }
  | exprPsl TokenOpW exprPsl       { OpOr (OpU ($1, $3), OpG $1) }
  | exprPsl TokenRel exprPsl       { OpR ($1, $3) }
  | exprPsl TokenOpS exprPsl       { OpS ($1, $3) }
  | exprPsl TokenOpT exprPsl       { OpT ($1, $3) }
  | exprPsl TokenAnd exprPsl       { OpAnd ($1, $3) }
  | exprPsl TokenOr exprPsl        { OpOr ($1, $3) }
  | exprPsl TokenImp exprPsl       { OpOr (OpNeg $1, $3) }
  | exprPsl TokenEq exprPsl        { OpAnd (OpOr (OpNeg $1, $3), OpOr ($1, OpNeg $3)) }

	| TokenLBr sere TokenRBr TokenFby exprPsl       { Fby ($2, $5) }
  | TokenLBr sere TokenRBr TokenBFby exprPsl      { BFby ($2, $5) }
  | TokenLBr sere TokenRBr TokenTrig exprPsl      { Trig ($2, $5) }
  | TokenLBr sere TokenRBr TokenBTrig exprPsl     { BTrig ($2, $5) }
  | TokenCl TokenLBr sere TokenRBr             { Cl $3 }
;

sere:
  TokenLBr sere TokenRBr     { $2 }
  | bexp                     { BExp $1 }
	| sere TokenStar           { Star $1 }
	| sere TokenPlus           { Conc($1, Star $1) }
  | sere TokenCol sere       { Col ($1, $3) } 
  | sere TokenScol sere      { Conc ($1, $3) }
  | sere TokenCap sere       { Cap ($1, $3) }
  | sere TokenCup sere       { Cup ($1, $3) }
;

bexp:
  TokenLPar bexp TokenRPar   { $2 }
	| TokenProp                { Atom $1 }
	| TokenProp TokenEQ TokenProp  { Atom ($1^"="^$3) }
	| TokenProp TokenNEQ TokenProp  { Atom ($1^"!="^$3) }
  | TokenNeg bexp            { Neg $2 }
  | bexp TokenAnd bexp       { And ($1, $3) }
  | bexp TokenOr bexp        { Or ($1, $3) }
;
*/
unused_tokens:			{	}
	|	ASSIGNMENT	{	}
	|	BIDIR		{	}
	|	BITSEL		{	}
	|	CASECOND	{	}
	|	CELLPIN		{	}
	|	CONCAT		{	}
	|	DECUPLE		{	}
	|	DLYASSIGNMENT	{	}
	|	DOTTED		{	}
	|	DOUBLE		{	}
	|	DRIVER		{	}
	|	DUODECUPLE	{	}
	|	DUOVIGENUPLE	{	}
	|	EMPTY		{	}
	|	ENDLABEL	{	}
	|	EOF		{	}
	|	FINITE		{	}
	|	FUNCASSIGNED	{	}
	|	FUNCREF		{	}
	|	FUNCUSED	{	}
	|	GENCASE		{	}
	|	GENCASECOND	{	}
	|	GENITEM		{	}
	|	IMPLICIT	{	}
	|	INT		{	}
	|	INT64		{	}
	|	IOPORT		{	}
	|	MEMORY		{	}
	|	MINTYPMAX	{	}
	|	MODINST		{	}
	|	NAMED		{	}
	|	NONUPLE		{	}
	|	NOTCONST	{	}
	|	NOVEMDECUPLE	{	}
	|	OCTODECUPLE	{	}
	|	OCTUPLE		{	}
	|	PARAMUSED	{	}
	|	PARTSEL		{	}
	|	PRIMINST	{	}
	|	PROPOSITION	{	}
/*
	|	PSL_ABORT	{	}
	|	PSL_ASSUME_GUARANTEE	{	}
	|	PSL_BEFORE	{	}
	|	PSL_BEFORE_PLING	{	}
	|	PSL_BEFORE_	{	}
	|	PSL_BOOLEAN	{	}
	|	PSL_CONST	{	}
//	|	PSL_CONTAINER	{	}
	|	PSL_ENDPOINT	{	}
	|	PSL_EVENTUALLY_PLING	{	}
	|	PSL_FAIRNESS	{	}
	|	PSL_FELL	{	}
	|	PSL_FOR	{	}
	|	PSL_FORALL	{	}
	|	PSL_IF	{	}
	|	PSL_IN	{	}
	|	PSL_INF	{	}
	|	PSL_INHERIT	{	}
	|	PSL_NEVER	{	}
	|	PSL_NEXT	{	}
	|	PSL_NEXT_A	{	}
	|	PSL_NEXT_A_PLING	{	}
	|	PSL_NEXT_E	{	}
	|	PSL_NEXT_EVENT	{	}
	|	PSL_NEXT_EVENT_A	{	}
	|	PSL_NEXT_EVENT_A_PLING	{	}
	|	PSL_NEXT_EVENT_E	{	}
	|	PSL_NEXT_EVENT_E_PLING	{	}
	|	PSL_NEXT_EVENT_PLING	{	}
	|	PSL_NEXT_E_PLING	{	}
	|	PSL_NEXT_PLING	{	}
	|	PSL_PREV	{	}
	|	PSL_PROPERTY	{	}
	|	PSL_RESTRICT_GUARANTEE	{	}
	|	PSL_ROSE	{	}
	|	PSL_SEQUENCE	{	}
	|	PSL_STABLE	{	}
	|	PSL_UNION	{	}
	|	PSL_UNTIL	{	}
	|	PSL_UNTIL_	{	}
	|	PSL_VMODE	{	}
	|	PSL_VPROP	{	}
	|	PSL_VUNIT	{	}
	|	PSL_WITHIN	{	}
*/
	|	P_ANDEQ		{	}
	|	P_ATAT		{	}
	|	P_COLONCOLON	{	}
	|	P_COLONDIV	{	}
	|	P_COLONEQ	{	}
	|	P_DEFINE	{	}
	|	P_DIVEQ		{	}
	|	P_ELSE		{	}
	|	P_ENDIF		{	}
	|	P_IFDEF		{	}
	|	P_IFNDEF	{	}
	|	P_INCLUDE	{	}
	|	P_MINUSEQ	{	}
	|	P_MODEQ		{	}
	|	P_OREQ		{	}
	|	P_OREQGT	{	}
	|	P_ORMINUSGT	{	}
	|	P_PLUSEQ	{	}
	|	P_POUNDPOUND	{	}
	|	P_SLEFTEQ	{	}
	|	P_SRIGHTEQ	{	}
	|	P_SSRIGHTEQ	{	}
	|	P_TIMESEQ	{	}
	|	P_XOREQ		{	}
	|	QUADRUPLE	{	}
	|	QUATTUORDECUPLE	{	}
	|	QUATTUORVIGENUPLE	{	}
	|	QUINDECUPLE	{	}
	|	QUINTUPLE	{	}
	|	QUINVIGENUPLE	{	}
	|	RANGE		{	}
	|	RECEIVER	{	}
	|	RELATION	{	}
	|	SCALAR		{	}
	|	SDIV		{	}
	|	SENSUSED	{	}
	|	SEPTENDECUPLE	{	}
	|	SEPTUPLE	{	}
	|	SEXDECUPLE	{	}
	|	SEXTUPLE	{	}
	|	SIGN_EXT	{	}
	|	SMOD		{	}
	|	SPECIAL		{	}
	|	SREM		{	}
	|	SUBCCT		{	}
	|	SUBMODULE	{	}
	|	TASKREF		{	}
	|	TASKUSED	{	}
	|	TEDGE		{	}
	|	THASH		{	}
	|	TLIST		{	}
	|	TREDECUPLE	{	}
	|	TREVIGENUPLE	{	}
	|	TRIPLE		{	}
	|	UNDECUPLE	{	}
	|	UNKNOWN		{	}
	|	UNVIGENUPLE	{	}
	|	VIGENUPLE	{	}
	|	VOID		{	}
	|	WIDTHNUM	{	}
	|	 PLIST	{	}
/*
	|	 TokenAnd	{	}
	|	 TokenBFby	{	}
	|	 TokenBTrig	{	}
	|	 TokenCap	{	}
	|	 TokenCl	{	}
	|	 TokenCup	{	}
	|	 TokenEEQ	{	}
	|	 TokenEq	{	}
	|	 TokenFby	{	}
	|	 TokenFin	{	}
	|	 TokenGen	{	}
	|	 TokenImp	{	}
	|	 TokenNEQ	{	}
	|	 TokenNeg	{	}
	|	 TokenNext	{	}
	|	 TokenOpH	{	}
	|	 TokenOpO	{	}
	|	 TokenOpS	{	}
	|	 TokenOpT	{	}
	|	 TokenOpW	{	}
	|	 TokenOr	{	}
	|	 TokenPlus	{	}
	|	 TokenRel	{	}
	|	 TokenStar	{	}
	|	 TokenTrig	{	}
	|	 TokenUnt	{	}
	|	 TokenYest	{	}
	|	 TokenZest	{	}
*/
	|	 WILDEQUAL	{	}
	

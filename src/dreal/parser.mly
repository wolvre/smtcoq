/*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 */

%{
%}

%token AFTER BEFORE PRUNING BRANCHED IS IN ON CONFLICT DETECTED
%token PRECISION
%token LB RB COMMA COLON SEMICOLON CARET NOT
%token LP RP PLUS MINUS AST SLASH EQ GE LE GT LT
%token INFTY
%token SIN COS TAN SQRT SAFESQRT
%token ASIN ACOS ATAN ATAN2 MATAN
%token SINH COSH TANH
%token LOG EXP
%token UNSAT HOLE TIME_PRECISION
%token EOF
%token <float> FNUM
%token <Num.num> NUM
%token <string> ID
%start main

%type <float * Basic.formula list * Ptree.t> main
%type <Ptree.t> ptree
%type <Basic.formula> con
%type <Basic.exp> func
%type <string> branched_on
%%

main:
   precision con_list init_list ptree EOF
     { ($1, $2, $4) }
 | precision con_list init_list EOF
     { ($1, $2, Ptree.NAxiom (Env.nmake $3)) }
 | precision con_list init_list conflict_detected EOF
     { ($1, $2, Ptree.NAxiom (Env.nmake $3)) }

precision: /* nothing */ { 0.001 } /* default value */
 | PRECISION COLON FNUM  { $3 }
 | PRECISION COLON NUM { BatNum.to_float $3 }

con_list: con          { [$1] }
        | con con_list { $1::$2 }
;

con: LP EQ func func RP { (Basic.Eq ($3, $4)) }
  |  LP LE func func RP { (Basic.Le ($3, $4)) }
  |  LP LT func func RP { (Basic.Le ($3, $4)) } /* ALWAYS TREAT IT AS LE */
  |  LP GE func func RP { (Basic.Ge ($3, $4)) }
  |  LP GT func func RP { (Basic.Ge ($3, $4)) } /* ALWAYS TREAT IT AS GE */
  |  LP NOT LP LE func func RP RP { (Basic.Ge ($5, $6)) }
  |  LP NOT LP LT func func RP RP { (Basic.Ge ($5, $6)) } /* ALWAYS TREAT IT AS GE */
  |  LP NOT LP GE func func RP RP { (Basic.Le ($5, $6)) }
  |  LP NOT LP GT func func RP RP { (Basic.Le ($5, $6)) } /* ALWAYS TREAT IT AS LE */
;

func_list: func           { [$1] }
         | func func_list { $1::$2 }
;

func:  FNUM                  { Basic.Num $1 }
     | NUM                  { Basic.NNum $1 }
     | ID                    { Basic.Var $1 }
     | LP PLUS  func_list RP { Basic.Add $3 }
     | LP MINUS func RP      { Basic.Neg $3 }
     | LP MINUS func func_list RP { Basic.Sub ($3::$4) }
     | LP AST   func_list RP { Basic.Mul $3 }
     | LP SLASH func func RP { Basic.Div ($3, $4) }
     | LP SQRT func RP       { Basic.Sqrt $3 }
     | LP SAFESQRT func RP   { Basic.Safesqrt $3 }
     | LP SIN func RP        { Basic.Sin $3 }
     | LP COS func RP        { Basic.Cos $3 }
     | LP TAN func RP        { Basic.Tan $3 }
     | LP ASIN func RP       { Basic.Asin $3 }
     | LP ACOS func RP       { Basic.Acos $3 }
     | LP ATAN func RP       { Basic.Atan $3 }
     | LP ATAN2 func func RP { Basic.Atan2 ($3, $4) }
     | LP MATAN func RP      { Basic.Matan $3 }
     | LP SINH func RP       { Basic.Sinh $3 }
     | LP COSH func RP       { Basic.Cosh $3 }
     | LP TANH func RP       { Basic.Tanh $3 }
     | LP LOG func RP        { Basic.Log $3 }
     | LP EXP func RP        { Basic.Exp $3 }
     | LP CARET func FNUM RP { Basic.Pow ($3, Basic.Num $4 ) }
     | LP CARET func NUM RP { Basic.Pow ( $3, Basic.NNum $4 ) }
;

ptree: /* Axiom */
       before_pruning nentry_list conflict_detected
         { Ptree.NAxiom (Env.nmake $2) }
       /* Hole */
     | before_pruning nentry_list after_pruning nentry_list HOLE
         { Ptree.NPrune (Env.nmake $2, Env.nmake $4, Ptree.Hole) }
       /* Branching */
     | branched_on nentry_list ptree ptree
         { Ptree.NBranch (Env.nmake $2, $3, $4) }
       /* Pruning */
     | before_pruning nentry_list after_pruning nentry_list ptree
         { Ptree.NPrune (Env.nmake $2, Env.nmake $4, $5) }
;

before_pruning: LB BEFORE PRUNING RB { }
;

after_pruning: LB AFTER PRUNING RB { }
;

branched_on: LB BRANCHED ON ID RB { $4 }
;

conflict_detected: LB CONFLICT DETECTED RB { }
;

init: nentry SEMICOLON { $1 }
;

init_list: init { [$1] }
         | init init_list { $1::$2 }
;

nentry:     
     | ID COLON LB NUM COMMA NUM RB { ($1, Intv.nmake $4 $6) }
     | ID COLON LP MINUS INFTY COMMA NUM RB { ($1, Intv.nmake Intv.nneg_infinity $7) }
     | ID COLON LB NUM COMMA PLUS INFTY RP { ($1, Intv.nmake $4 Intv.ninfinity) }
     | ID COLON LB NUM COMMA INFTY RP { ($1, Intv.nmake $4 Intv.ninfinity) }
     | ID COLON NUM { ($1, Intv.nmake $3 $3) }
;

nentry_list: nentry { [$1] }
     | nentry SEMICOLON nentry_list { $1::$3 }
;



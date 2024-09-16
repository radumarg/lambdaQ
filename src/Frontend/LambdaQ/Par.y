-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Frontend.LambdaQ.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Frontend.LambdaQ.Abs
import Frontend.LambdaQ.Lex

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'             { PT _ (TS _ 1)      }
  '!!'            { PT _ (TS _ 2)      }
  '$'             { PT _ (TS _ 3)      }
  '&&'            { PT _ (TS _ 4)      }
  '('             { PT _ (TS _ 5)      }
  '()'            { PT _ (TS _ 6)      }
  ')'             { PT _ (TS _ 7)      }
  '*'             { PT _ (TS _ 8)      }
  '**'            { PT _ (TS _ 9)      }
  '+'             { PT _ (TS _ 10)     }
  '++'            { PT _ (TS _ 11)     }
  ','             { PT _ (TS _ 12)     }
  '-'             { PT _ (TS _ 13)     }
  '->'            { PT _ (TS _ 14)     }
  '.'             { PT _ (TS _ 15)     }
  '/'             { PT _ (TS _ 16)     }
  '/='            { PT _ (TS _ 17)     }
  ':'             { PT _ (TS _ 18)     }
  '::'            { PT _ (TS _ 19)     }
  ';'             { PT _ (TS _ 20)     }
  '<'             { PT _ (TS _ 21)     }
  '<-'            { PT _ (TS _ 22)     }
  '<='            { PT _ (TS _ 23)     }
  '='             { PT _ (TS _ 24)     }
  '=='            { PT _ (TS _ 25)     }
  '>'             { PT _ (TS _ 26)     }
  '>='            { PT _ (TS _ 27)     }
  '@+'            { PT _ (TS _ 28)     }
  '@+i'           { PT _ (TS _ 29)     }
  '@-'            { PT _ (TS _ 30)     }
  '@-i'           { PT _ (TS _ 31)     }
  '@0'            { PT _ (TS _ 32)     }
  '@1'            { PT _ (TS _ 33)     }
  'BasisState'    { PT _ (TS _ 34)     }
  'Bit'           { PT _ (TS _ 35)     }
  'Bool'          { PT _ (TS _ 36)     }
  'FSWAP'         { PT _ (TS _ 37)     }
  'False'         { PT _ (TS _ 38)     }
  'H'             { PT _ (TS _ 39)     }
  'ID'            { PT _ (TS _ 40)     }
  'ISWAP'         { PT _ (TS _ 41)     }
  'Int'           { PT _ (TS _ 42)     }
  'QFT'           { PT _ (TS _ 43)     }
  'QFT_DAG'       { PT _ (TS _ 44)     }
  'Qbit'          { PT _ (TS _ 45)     }
  'ROOT_SWAP'     { PT _ (TS _ 46)     }
  'ROOT_SWAP_DAG' { PT _ (TS _ 47)     }
  'ROOT_X'        { PT _ (TS _ 48)     }
  'ROOT_X_DAG'    { PT _ (TS _ 49)     }
  'ROOT_Y'        { PT _ (TS _ 50)     }
  'ROOT_Y_DAG'    { PT _ (TS _ 51)     }
  'ROOT_Z'        { PT _ (TS _ 52)     }
  'ROOT_Z_DAG'    { PT _ (TS _ 53)     }
  'RX'            { PT _ (TS _ 54)     }
  'RY'            { PT _ (TS _ 55)     }
  'RZ'            { PT _ (TS _ 56)     }
  'S'             { PT _ (TS _ 57)     }
  'SQRT_SWAP'     { PT _ (TS _ 58)     }
  'SQRT_SWAP_DAG' { PT _ (TS _ 59)     }
  'SQRT_X'        { PT _ (TS _ 60)     }
  'SQRT_X_DAG'    { PT _ (TS _ 61)     }
  'SQRT_Y'        { PT _ (TS _ 62)     }
  'SQRT_Y_DAG'    { PT _ (TS _ 63)     }
  'SWAP'          { PT _ (TS _ 64)     }
  'SWAP_THETA'    { PT _ (TS _ 65)     }
  'S_DAG'         { PT _ (TS _ 66)     }
  'T'             { PT _ (TS _ 67)     }
  'T_DAG'         { PT _ (TS _ 68)     }
  'True'          { PT _ (TS _ 69)     }
  'U1'            { PT _ (TS _ 70)     }
  'U2'            { PT _ (TS _ 71)     }
  'U3'            { PT _ (TS _ 72)     }
  'X'             { PT _ (TS _ 73)     }
  'Y'             { PT _ (TS _ 74)     }
  'Z'             { PT _ (TS _ 75)     }
  '['             { PT _ (TS _ 76)     }
  '[]'            { PT _ (TS _ 77)     }
  ']'             { PT _ (TS _ 78)     }
  'case'          { PT _ (TS _ 79)     }
  'ctrl'          { PT _ (TS _ 80)     }
  'else'          { PT _ (TS _ 81)     }
  'gate'          { PT _ (TS _ 82)     }
  'if'            { PT _ (TS _ 83)     }
  'in'            { PT _ (TS _ 84)     }
  'let'           { PT _ (TS _ 85)     }
  'not'           { PT _ (TS _ 86)     }
  'of'            { PT _ (TS _ 87)     }
  'then'          { PT _ (TS _ 88)     }
  'with'          { PT _ (TS _ 89)     }
  '{'             { PT _ (TS _ 90)     }
  '|'             { PT _ (TS _ 91)     }
  '||'            { PT _ (TS _ 92)     }
  '}'             { PT _ (TS _ 93)     }
  L_doubl         { PT _ (TD $$)       }
  L_integ         { PT _ (TI $$)       }
  L_GateVar       { PT _ (T_GateVar _) }
  L_Var           { PT _ (T_Var _)     }
  L_Bit           { PT _ (T_Bit $$)    }
  L_Lambda        { PT _ (T_Lambda $$) }

%%

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

GateVar :: { Frontend.LambdaQ.Abs.GateVar }
GateVar  : L_GateVar { Frontend.LambdaQ.Abs.GateVar (mkPosToken $1) }

Var :: { Frontend.LambdaQ.Abs.Var }
Var  : L_Var { Frontend.LambdaQ.Abs.Var (mkPosToken $1) }

Bit :: { Frontend.LambdaQ.Abs.Bit }
Bit  : L_Bit { Frontend.LambdaQ.Abs.Bit $1 }

Lambda :: { Frontend.LambdaQ.Abs.Lambda }
Lambda  : L_Lambda { Frontend.LambdaQ.Abs.Lambda $1 }

Program :: { Frontend.LambdaQ.Abs.Program }
Program
  : ListFunctionDeclaration { Frontend.LambdaQ.Abs.ProgDef $1 }

IntegerExpression1 :: { Frontend.LambdaQ.Abs.IntegerExpression }
IntegerExpression1
  : '-' IntegerExpression { Frontend.LambdaQ.Abs.ArithmExprMinus $2 }
  | IntegerExpression1 '+' IntegerExpression2 { Frontend.LambdaQ.Abs.ArithmExprAdd $1 $3 }
  | IntegerExpression1 '-' IntegerExpression2 { Frontend.LambdaQ.Abs.ArithmExprSub $1 $3 }
  | IntegerExpression2 { $1 }

IntegerExpression2 :: { Frontend.LambdaQ.Abs.IntegerExpression }
IntegerExpression2
  : IntegerExpression2 '*' IntegerExpression3 { Frontend.LambdaQ.Abs.ArithmExprMul $1 $3 }
  | IntegerExpression2 '/' IntegerExpression3 { Frontend.LambdaQ.Abs.ArithmExprDiv $1 $3 }
  | IntegerExpression3 { $1 }

IntegerExpression3 :: { Frontend.LambdaQ.Abs.IntegerExpression }
IntegerExpression3
  : Integer { Frontend.LambdaQ.Abs.ArithmExprInt $1 }
  | '(' IntegerExpression ')' { $2 }

IntegerExpression :: { Frontend.LambdaQ.Abs.IntegerExpression }
IntegerExpression : IntegerExpression1 { $1 }

BoolValue :: { Frontend.LambdaQ.Abs.BoolValue }
BoolValue
  : 'True' { Frontend.LambdaQ.Abs.BoolValueTrue }
  | 'False' { Frontend.LambdaQ.Abs.BoolValueFalse }

BoolExpression :: { Frontend.LambdaQ.Abs.BoolExpression }
BoolExpression
  : BoolExpression '&&' BoolExpression1 { Frontend.LambdaQ.Abs.BoolExpressionAnd $1 $3 }
  | BoolExpression '||' BoolExpression1 { Frontend.LambdaQ.Abs.BoolExpressionOr $1 $3 }
  | 'not' BoolExpression1 { Frontend.LambdaQ.Abs.BoolExpressionNot $2 }
  | BoolExpression1 { $1 }

BoolExpression1 :: { Frontend.LambdaQ.Abs.BoolExpression }
BoolExpression1
  : BoolExpression1 '==' BoolExpression2 { Frontend.LambdaQ.Abs.BoolExpressionEq $1 $3 }
  | BoolExpression1 '/=' BoolExpression2 { Frontend.LambdaQ.Abs.BoolExpressionDif $1 $3 }
  | BoolExpression2 { $1 }

BoolExpression2 :: { Frontend.LambdaQ.Abs.BoolExpression }
BoolExpression2
  : IntegerExpression2 '==' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionEqInt $1 $3 }
  | IntegerExpression2 '/=' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionDifInt $1 $3 }
  | IntegerExpression2 '>' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionGt $1 $3 }
  | IntegerExpression2 '>=' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionGe $1 $3 }
  | IntegerExpression2 '<' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionLt $1 $3 }
  | IntegerExpression2 '<=' IntegerExpression2 { Frontend.LambdaQ.Abs.BoolExpressionLe $1 $3 }
  | BoolValue { Frontend.LambdaQ.Abs.BoolExpressionVal $1 }
  | '(' BoolExpression ')' { $2 }

Type :: { Frontend.LambdaQ.Abs.Type }
Type
  : Type1 '->' Type { Frontend.LambdaQ.Abs.TypeFunction $1 $3 }
  | Type1 { $1 }

Type2 :: { Frontend.LambdaQ.Abs.Type }
Type2
  : Type2 '*' Type3 { Frontend.LambdaQ.Abs.TypeTensorProd $1 $3 }
  | Type3 { $1 }

Type3 :: { Frontend.LambdaQ.Abs.Type }
Type3
  : Type4 '**' Integer { Frontend.LambdaQ.Abs.TypeExp $1 $3 }
  | '!' Type4 { Frontend.LambdaQ.Abs.TypeNonLinear $2 }
  | Type4 { $1 }

Type5 :: { Frontend.LambdaQ.Abs.Type }
Type5
  : 'Bool' { Frontend.LambdaQ.Abs.TypeBool }
  | 'Bit' { Frontend.LambdaQ.Abs.TypeBit }
  | 'Int' { Frontend.LambdaQ.Abs.TypeInteger }
  | 'Qbit' { Frontend.LambdaQ.Abs.TypeQbit }
  | 'BasisState' { Frontend.LambdaQ.Abs.TypeBasisState }
  | '()' { Frontend.LambdaQ.Abs.TypeUnit }
  | '[' Type ']' { Frontend.LambdaQ.Abs.TypeList $2 }
  | '(' Type ')' { $2 }

Type1 :: { Frontend.LambdaQ.Abs.Type }
Type1 : Type2 { $1 }

Type4 :: { Frontend.LambdaQ.Abs.Type }
Type4 : Type5 { $1 }

Angle :: { Frontend.LambdaQ.Abs.Angle }
Angle : Double { Frontend.LambdaQ.Abs.AngleValue $1 }

BasisState :: { Frontend.LambdaQ.Abs.BasisState }
BasisState
  : '@0' { Frontend.LambdaQ.Abs.BasisStateZero }
  | '@1' { Frontend.LambdaQ.Abs.BasisStateOne }
  | '@+' { Frontend.LambdaQ.Abs.BasisStatePlus }
  | '@-' { Frontend.LambdaQ.Abs.BasisStateMinus }
  | '@+i' { Frontend.LambdaQ.Abs.BasisStatePlusI }
  | '@-i' { Frontend.LambdaQ.Abs.BasisStateMinusI }

Gate :: { Frontend.LambdaQ.Abs.Gate }
Gate
  : 'H' { Frontend.LambdaQ.Abs.GateH }
  | 'X' { Frontend.LambdaQ.Abs.GateX }
  | 'Y' { Frontend.LambdaQ.Abs.GateY }
  | 'Z' { Frontend.LambdaQ.Abs.GateZ }
  | 'ID' { Frontend.LambdaQ.Abs.GateID }
  | 'ROOT_X' '[' Integer ']' { Frontend.LambdaQ.Abs.GateXRootInt $3 }
  | 'ROOT_X' '[' Var ']' { Frontend.LambdaQ.Abs.GateXRootVar $3 }
  | 'ROOT_X_DAG' '[' Integer ']' { Frontend.LambdaQ.Abs.GateXRootDagInt $3 }
  | 'ROOT_X_DAG' '[' Var ']' { Frontend.LambdaQ.Abs.GateXRootDagVar $3 }
  | 'ROOT_Y' '[' Integer ']' { Frontend.LambdaQ.Abs.GateYRootInt $3 }
  | 'ROOT_Y' '[' Var ']' { Frontend.LambdaQ.Abs.GateYRootVar $3 }
  | 'ROOT_Y_DAG' '[' Integer ']' { Frontend.LambdaQ.Abs.GateYRootDagInt $3 }
  | 'ROOT_Y_DAG' '[' Var ']' { Frontend.LambdaQ.Abs.GateYRootDagVar $3 }
  | 'ROOT_Z' '[' Integer ']' { Frontend.LambdaQ.Abs.GateZRootInt $3 }
  | 'ROOT_Z' '[' Var ']' { Frontend.LambdaQ.Abs.GateZRootVar $3 }
  | 'ROOT_Z_DAG' '[' Integer ']' { Frontend.LambdaQ.Abs.GateZRootDagInt $3 }
  | 'ROOT_Z_DAG' '[' Var ']' { Frontend.LambdaQ.Abs.GateZRootDagVar $3 }
  | 'S' { Frontend.LambdaQ.Abs.GateS }
  | 'S_DAG' { Frontend.LambdaQ.Abs.GateSDag }
  | 'T' { Frontend.LambdaQ.Abs.GateT }
  | 'T_DAG' { Frontend.LambdaQ.Abs.GateTDag }
  | 'SQRT_X' { Frontend.LambdaQ.Abs.GateSqrtX }
  | 'SQRT_X_DAG' { Frontend.LambdaQ.Abs.GateSqrtXDag }
  | 'SQRT_Y' { Frontend.LambdaQ.Abs.GateSqrtY }
  | 'SQRT_Y_DAG' { Frontend.LambdaQ.Abs.GateSqrtYDag }
  | 'RX' '[' Angle ']' { Frontend.LambdaQ.Abs.GateRxTheta $3 }
  | 'RY' '[' Angle ']' { Frontend.LambdaQ.Abs.GateRyTheta $3 }
  | 'RZ' '[' Angle ']' { Frontend.LambdaQ.Abs.GateRzTheta $3 }
  | 'U1' '[' Angle ']' { Frontend.LambdaQ.Abs.GateU1 $3 }
  | 'U2' '[' Angle ',' Angle ']' { Frontend.LambdaQ.Abs.GateU2 $3 $5 }
  | 'U3' '[' Angle ',' Angle ',' Angle ']' { Frontend.LambdaQ.Abs.GateU3 $3 $5 $7 }
  | 'SWAP' { Frontend.LambdaQ.Abs.GateSwp }
  | 'SQRT_SWAP' { Frontend.LambdaQ.Abs.GateSqrtSwp }
  | 'SQRT_SWAP_DAG' { Frontend.LambdaQ.Abs.GateSqrtSwpDag }
  | 'ISWAP' { Frontend.LambdaQ.Abs.GateISwp }
  | 'FSWAP' { Frontend.LambdaQ.Abs.GateFSwp }
  | 'SWAP_THETA' '[' Angle ']' { Frontend.LambdaQ.Abs.GateSwpTheta $3 }
  | 'ROOT_SWAP' '[' Integer ']' { Frontend.LambdaQ.Abs.GateSwpRtInt $3 }
  | 'ROOT_SWAP' '[' Var ']' { Frontend.LambdaQ.Abs.GateSwpRtVar $3 }
  | 'ROOT_SWAP_DAG' '[' Integer ']' { Frontend.LambdaQ.Abs.GateSwpRtDagInt $3 }
  | 'ROOT_SWAP_DAG' '[' Var ']' { Frontend.LambdaQ.Abs.GateSwpRtDagVar $3 }
  | 'QFT' '[' Integer ']' { Frontend.LambdaQ.Abs.GateQftInt $3 }
  | 'QFT' '[' Var ']' { Frontend.LambdaQ.Abs.GateQftVar $3 }
  | 'QFT_DAG' '[' Integer ']' { Frontend.LambdaQ.Abs.GateQftDagInt $3 }
  | 'QFT_DAG' '[' Var ']' { Frontend.LambdaQ.Abs.GateQftDagVar $3 }
  | GateVar '[' Angle ',' Angle ',' Angle ']' { Frontend.LambdaQ.Abs.GateUnknown3Angle $1 $3 $5 $7 }
  | GateVar '[' Angle ',' Angle ']' { Frontend.LambdaQ.Abs.GateUnknown2Angle $1 $3 $5 }
  | GateVar '[' Angle ']' { Frontend.LambdaQ.Abs.GateUnknown1Angle $1 $3 }
  | GateVar '[' Integer ']' { Frontend.LambdaQ.Abs.GateUnknownInt $1 $3 }
  | GateVar '[' Var ']' { Frontend.LambdaQ.Abs.GateUnknownVar $1 $3 }
  | GateVar { Frontend.LambdaQ.Abs.GateUnknownSimple $1 }

ListVar :: { [Frontend.LambdaQ.Abs.Var] }
ListVar : Var { (:[]) $1 } | Var ',' ListVar { (:) $1 $3 }

ListBit :: { [Frontend.LambdaQ.Abs.Bit] }
ListBit : Bit { (:[]) $1 } | Bit ',' ListBit { (:) $1 $3 }

ControlBasisState :: { Frontend.LambdaQ.Abs.ControlBasisState }
ControlBasisState
  : '[' BasisState ']' { Frontend.LambdaQ.Abs.CtrlBasisState $2 }

ControlBasisStates :: { Frontend.LambdaQ.Abs.ControlBasisStates }
ControlBasisStates
  : '[' BasisState ',' ListBasisState ']' { Frontend.LambdaQ.Abs.CtrlBasisStates $2 $4 }

ListBasisState :: { [Frontend.LambdaQ.Abs.BasisState] }
ListBasisState
  : BasisState { (:[]) $1 }
  | BasisState ',' ListBasisState { (:) $1 $3 }

ControlBit :: { Frontend.LambdaQ.Abs.ControlBit }
ControlBit : '[' Bit ']' { Frontend.LambdaQ.Abs.CtrlBit $2 }

ControlBits :: { Frontend.LambdaQ.Abs.ControlBits }
ControlBits
  : '[' Bit ',' ListBit ']' { Frontend.LambdaQ.Abs.CtrlBits $2 $4 }

ListInteger :: { [Integer] }
ListInteger
  : Integer { (:[]) $1 } | Integer ',' ListInteger { (:) $1 $3 }

ControlTerm :: { Frontend.LambdaQ.Abs.ControlTerm }
ControlTerm : '[' Term ']' { Frontend.LambdaQ.Abs.CtrlTerm $2 }

ControlTerms :: { Frontend.LambdaQ.Abs.ControlTerms }
ControlTerms
  : '[' Term ',' ListTerm ']' { Frontend.LambdaQ.Abs.CtrlTerms $2 $4 }

ControlVar :: { Frontend.LambdaQ.Abs.ControlVar }
ControlVar : '[' Var ']' { Frontend.LambdaQ.Abs.CtrlVar $2 }

ControlVars :: { Frontend.LambdaQ.Abs.ControlVars }
ControlVars
  : '[' Var ',' ListVar ']' { Frontend.LambdaQ.Abs.CtrlVars $2 $4 }

ListTerm :: { [Frontend.LambdaQ.Abs.Term] }
ListTerm : Term { (:[]) $1 } | Term ',' ListTerm { (:) $1 $3 }

Term4 :: { Frontend.LambdaQ.Abs.Term }
Term4
  : List '!!' Integer { Frontend.LambdaQ.Abs.TermListElement $1 $3 }
  | '(' Term ')' { $2 }

Term3 :: { Frontend.LambdaQ.Abs.Term }
Term3
  : '()' { Frontend.LambdaQ.Abs.TermUnit }
  | Bit { Frontend.LambdaQ.Abs.TermBit $1 }
  | BasisState { Frontend.LambdaQ.Abs.TermBasisState $1 }
  | BoolExpression { Frontend.LambdaQ.Abs.TermBoolExpression $1 }
  | IntegerExpression { Frontend.LambdaQ.Abs.TermIntegerExpression $1 }
  | 'gate' Gate { Frontend.LambdaQ.Abs.TermGate $2 }
  | List { Frontend.LambdaQ.Abs.TermList $1 }
  | Var { Frontend.LambdaQ.Abs.TermVariable $1 }
  | '(' Term ',' ListTerm ')' { Frontend.LambdaQ.Abs.TermTuple $2 $4 }
  | Term4 { $1 }

Term2 :: { Frontend.LambdaQ.Abs.Term }
Term2
  : 'with' ControlTerm 'ctrl' ControlBasisState { Frontend.LambdaQ.Abs.TermGateQuantumCtrl $2 $4 }
  | 'with' ControlTerms 'ctrl' ControlBasisStates { Frontend.LambdaQ.Abs.TermGateQuantumTCtrls $2 $4 }
  | 'with' ControlVars 'ctrl' ControlBasisStates { Frontend.LambdaQ.Abs.TermGateQuantumVCtrls $2 $4 }
  | 'with' ControlTerm 'ctrl' ControlBit { Frontend.LambdaQ.Abs.TermGateClassicCtrl $2 $4 }
  | 'with' ControlTerms 'ctrl' ControlBits { Frontend.LambdaQ.Abs.TermGateClassicTCtrls $2 $4 }
  | 'with' ControlVars 'ctrl' ControlBits { Frontend.LambdaQ.Abs.TermGateClassicVCtrls $2 $4 }
  | Term2 Term3 { Frontend.LambdaQ.Abs.TermApply $1 $2 }
  | Term2 '.' Term3 { Frontend.LambdaQ.Abs.TermCompose $1 $3 }
  | Term2 '*' Term3 { Frontend.LambdaQ.Abs.TermTensorProduct $1 $3 }
  | Term3 { $1 }

Term1 :: { Frontend.LambdaQ.Abs.Term }
Term1
  : 'if' Term 'then' Term 'else' Term { Frontend.LambdaQ.Abs.TermIfElse $2 $4 $6 }
  | 'let' '{' Var '=' Term '}' 'in' Term { Frontend.LambdaQ.Abs.TermLetSingle $3 $5 $8 }
  | 'let' '{' '(' Var ',' ListVar ')' '=' Term '}' 'in' Term { Frontend.LambdaQ.Abs.TermLetMultiple $4 $6 $9 $12 }
  | Var '<-' Term ';' Term { Frontend.LambdaQ.Abs.TermLetSugarSingle $1 $3 $5 }
  | '|' Var ',' ListVar '|' '<-' Term ';' Term { Frontend.LambdaQ.Abs.TermLetSugarMultiple $2 $4 $7 $9 }
  | 'case' Term 'of' '{' ListCaseExpression '}' { Frontend.LambdaQ.Abs.TermCase $2 $5 }
  | Lambda Var Type '.' Term { Frontend.LambdaQ.Abs.TermLambda $1 $2 $3 $5 }
  | Term2 { $1 }

Term :: { Frontend.LambdaQ.Abs.Term }
Term
  : Term1 '$' Term { Frontend.LambdaQ.Abs.TermDollar $1 $3 }
  | Term1 { $1 }

List1 :: { Frontend.LambdaQ.Abs.List }
List1
  : '[]' { Frontend.LambdaQ.Abs.ListNil }
  | '[' Term ']' { Frontend.LambdaQ.Abs.ListSingle $2 }
  | '[' Term ',' ListTerm ']' { Frontend.LambdaQ.Abs.ListMultiple $2 $4 }
  | '(' List ')' { $2 }

List :: { Frontend.LambdaQ.Abs.List }
List
  : List '++' List1 { Frontend.LambdaQ.Abs.ListExpressionAdd $1 $3 }
  | Term3 ':' List1 { Frontend.LambdaQ.Abs.ListCons $1 $3 }
  | List1 { $1 }

CaseExpression :: { Frontend.LambdaQ.Abs.CaseExpression }
CaseExpression
  : Term '->' Term { Frontend.LambdaQ.Abs.CaseExpr $1 $3 }
  | CaseExpression ';' { $1 }

ListCaseExpression :: { [Frontend.LambdaQ.Abs.CaseExpression] }
ListCaseExpression
  : CaseExpression { (:[]) $1 }
  | CaseExpression ListCaseExpression { (:) $1 $2 }

Arg :: { Frontend.LambdaQ.Abs.Arg }
Arg : Var { Frontend.LambdaQ.Abs.FunArg $1 }

ListArg :: { [Frontend.LambdaQ.Abs.Arg] }
ListArg : {- empty -} { [] } | Arg ListArg { (:) $1 $2 }

FunctionDefinition :: { Frontend.LambdaQ.Abs.FunctionDefinition }
FunctionDefinition
  : Var ListArg '=' Term { Frontend.LambdaQ.Abs.FunDef $1 $2 $4 }
  | FunctionDefinition ';' { $1 }

FunctionType :: { Frontend.LambdaQ.Abs.FunctionType }
FunctionType
  : Var '::' Type { Frontend.LambdaQ.Abs.FunType $1 $3 }
  | FunctionType ';' { $1 }

FunctionDeclaration :: { Frontend.LambdaQ.Abs.FunctionDeclaration }
FunctionDeclaration
  : FunctionType ';' FunctionDefinition ';' { Frontend.LambdaQ.Abs.FunDecl $1 $3 }

ListFunctionDeclaration :: { [Frontend.LambdaQ.Abs.FunctionDeclaration] }
ListFunctionDeclaration
  : {- empty -} { [] }
  | FunctionDeclaration ListFunctionDeclaration { (:) $1 $2 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}


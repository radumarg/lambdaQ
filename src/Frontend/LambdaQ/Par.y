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
  '$'             { PT _ (TS _ 2)      }
  '('             { PT _ (TS _ 3)      }
  '()'            { PT _ (TS _ 4)      }
  ')'             { PT _ (TS _ 5)      }
  '*'             { PT _ (TS _ 6)      }
  '**'            { PT _ (TS _ 7)      }
  '+'             { PT _ (TS _ 8)      }
  ','             { PT _ (TS _ 9)      }
  '-'             { PT _ (TS _ 10)     }
  '->'            { PT _ (TS _ 11)     }
  '.'             { PT _ (TS _ 12)     }
  '/'             { PT _ (TS _ 13)     }
  '::'            { PT _ (TS _ 14)     }
  ';'             { PT _ (TS _ 15)     }
  '<-'            { PT _ (TS _ 16)     }
  '='             { PT _ (TS _ 17)     }
  '@+'            { PT _ (TS _ 18)     }
  '@+i'           { PT _ (TS _ 19)     }
  '@-'            { PT _ (TS _ 20)     }
  '@-i'           { PT _ (TS _ 21)     }
  '@0'            { PT _ (TS _ 22)     }
  '@1'            { PT _ (TS _ 23)     }
  'Bit'           { PT _ (TS _ 24)     }
  'FSWAP'         { PT _ (TS _ 25)     }
  'H'             { PT _ (TS _ 26)     }
  'ID'            { PT _ (TS _ 27)     }
  'ISWAP'         { PT _ (TS _ 28)     }
  'QFT'           { PT _ (TS _ 29)     }
  'QFT_DAG'       { PT _ (TS _ 30)     }
  'Qbit'          { PT _ (TS _ 31)     }
  'ROOT_SWAP'     { PT _ (TS _ 32)     }
  'ROOT_SWAP_DAG' { PT _ (TS _ 33)     }
  'ROOT_X'        { PT _ (TS _ 34)     }
  'ROOT_X_DAG'    { PT _ (TS _ 35)     }
  'ROOT_Y'        { PT _ (TS _ 36)     }
  'ROOT_Y_DAG'    { PT _ (TS _ 37)     }
  'ROOT_Z'        { PT _ (TS _ 38)     }
  'ROOT_Z_DAG'    { PT _ (TS _ 39)     }
  'RX'            { PT _ (TS _ 40)     }
  'RY'            { PT _ (TS _ 41)     }
  'RZ'            { PT _ (TS _ 42)     }
  'S'             { PT _ (TS _ 43)     }
  'SQRT_SWAP'     { PT _ (TS _ 44)     }
  'SQRT_SWAP_DAG' { PT _ (TS _ 45)     }
  'SQRT_X'        { PT _ (TS _ 46)     }
  'SQRT_X_DAG'    { PT _ (TS _ 47)     }
  'SQRT_Y'        { PT _ (TS _ 48)     }
  'SQRT_Y_DAG'    { PT _ (TS _ 49)     }
  'SWAP'          { PT _ (TS _ 50)     }
  'SWAP_THETA'    { PT _ (TS _ 51)     }
  'S_DAG'         { PT _ (TS _ 52)     }
  'State'         { PT _ (TS _ 53)     }
  'T'             { PT _ (TS _ 54)     }
  'T_DAG'         { PT _ (TS _ 55)     }
  'U1'            { PT _ (TS _ 56)     }
  'U2'            { PT _ (TS _ 57)     }
  'U3'            { PT _ (TS _ 58)     }
  'Unitary'       { PT _ (TS _ 59)     }
  'X'             { PT _ (TS _ 60)     }
  'Y'             { PT _ (TS _ 61)     }
  'Z'             { PT _ (TS _ 62)     }
  '['             { PT _ (TS _ 63)     }
  ']'             { PT _ (TS _ 64)     }
  'case'          { PT _ (TS _ 65)     }
  'ctrl'          { PT _ (TS _ 66)     }
  'else'          { PT _ (TS _ 67)     }
  'gate'          { PT _ (TS _ 68)     }
  'if'            { PT _ (TS _ 69)     }
  'in'            { PT _ (TS _ 70)     }
  'let'           { PT _ (TS _ 71)     }
  'of'            { PT _ (TS _ 72)     }
  'then'          { PT _ (TS _ 73)     }
  'with'          { PT _ (TS _ 74)     }
  '{'             { PT _ (TS _ 75)     }
  '}'             { PT _ (TS _ 76)     }
  L_doubl         { PT _ (TD $$)       }
  L_integ         { PT _ (TI $$)       }
  L_Var           { PT _ (T_Var _)     }
  L_Lambda        { PT _ (T_Lambda $$) }

%%

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Var :: { Frontend.LambdaQ.Abs.Var }
Var  : L_Var { Frontend.LambdaQ.Abs.Var (mkPosToken $1) }

Lambda :: { Frontend.LambdaQ.Abs.Lambda }
Lambda  : L_Lambda { Frontend.LambdaQ.Abs.Lambda $1 }

Program :: { Frontend.LambdaQ.Abs.Program }
Program
  : ListFunctionDeclaration { Frontend.LambdaQ.Abs.ProgDef $1 }

Type5 :: { Frontend.LambdaQ.Abs.Type }
Type5
  : 'Bit' { Frontend.LambdaQ.Abs.TypeBit }
  | 'Qbit' { Frontend.LambdaQ.Abs.TypeQbit }
  | 'State' { Frontend.LambdaQ.Abs.TypeState }
  | 'Unitary' { Frontend.LambdaQ.Abs.TypeUnitary }
  | '()' { Frontend.LambdaQ.Abs.TypeUnit }
  | '(' Type ')' { $2 }

Type4 :: { Frontend.LambdaQ.Abs.Type }
Type4
  : '!' Type5 { Frontend.LambdaQ.Abs.TypeNonLinear $2 }
  | Type5 { $1 }

Type3 :: { Frontend.LambdaQ.Abs.Type }
Type3
  : Type4 '**' Integer { Frontend.LambdaQ.Abs.TypeExp $1 $3 }
  | Type4 '*' Type3 { Frontend.LambdaQ.Abs.TypeTensorProd $1 $3 }
  | Type4 { $1 }

Type2 :: { Frontend.LambdaQ.Abs.Type }
Type2
  : Type3 '+' Type2 { Frontend.LambdaQ.Abs.TypeSum $1 $3 }
  | Type3 { $1 }

Type1 :: { Frontend.LambdaQ.Abs.Type }
Type1
  : Type2 '->' Type1 { Frontend.LambdaQ.Abs.TypeFunction $1 $3 }
  | Type2 { $1 }

Type :: { Frontend.LambdaQ.Abs.Type }
Type : Type1 { $1 }

Angle :: { Frontend.LambdaQ.Abs.Angle }
Angle : Double { Frontend.LambdaQ.Abs.Angle $1 }

BasisState :: { Frontend.LambdaQ.Abs.BasisState }
BasisState
  : '@0' { Frontend.LambdaQ.Abs.BasisStateZero }
  | '@1' { Frontend.LambdaQ.Abs.BasisStateOne }
  | '@+' { Frontend.LambdaQ.Abs.BasisStatePlus }
  | '@-' { Frontend.LambdaQ.Abs.BasisStateMinus }
  | '@+i' { Frontend.LambdaQ.Abs.BasisStatePlusI }
  | '@-i' { Frontend.LambdaQ.Abs.BasisStateMinusI }

Bit :: { Frontend.LambdaQ.Abs.Bit }
Bit : Integer { Frontend.LambdaQ.Abs.BitValue $1 }

Gate :: { Frontend.LambdaQ.Abs.Gate }
Gate
  : 'H' { Frontend.LambdaQ.Abs.GateH }
  | 'X' { Frontend.LambdaQ.Abs.GateX }
  | 'Y' { Frontend.LambdaQ.Abs.GateY }
  | 'Z' { Frontend.LambdaQ.Abs.GateZ }
  | 'ID' { Frontend.LambdaQ.Abs.GateID }
  | 'ROOT_X' Integer { Frontend.LambdaQ.Abs.GateXRoot $2 }
  | 'ROOT_X_DAG' Integer { Frontend.LambdaQ.Abs.GateXRootDag $2 }
  | 'ROOT_Y' Integer { Frontend.LambdaQ.Abs.GateYRoot $2 }
  | 'ROOT_Y_DAG' Integer { Frontend.LambdaQ.Abs.GateYRootDag $2 }
  | 'ROOT_Z' Integer { Frontend.LambdaQ.Abs.GateZRoot $2 }
  | 'ROOT_Z_DAG' Integer { Frontend.LambdaQ.Abs.GateZRootDag $2 }
  | 'S' { Frontend.LambdaQ.Abs.GateS }
  | 'S_DAG' { Frontend.LambdaQ.Abs.GateSDag }
  | 'T' { Frontend.LambdaQ.Abs.GateT }
  | 'T_DAG' { Frontend.LambdaQ.Abs.GateTDag }
  | 'SQRT_X' { Frontend.LambdaQ.Abs.GateSqrtX }
  | 'SQRT_X_DAG' { Frontend.LambdaQ.Abs.GateSqrtXDag }
  | 'SQRT_Y' { Frontend.LambdaQ.Abs.GateSqrtY }
  | 'SQRT_Y_DAG' { Frontend.LambdaQ.Abs.GateSqrtYDag }
  | 'RX' Angle { Frontend.LambdaQ.Abs.GateRxTheta $2 }
  | 'RY' Angle { Frontend.LambdaQ.Abs.GateRyTheta $2 }
  | 'RZ' Angle { Frontend.LambdaQ.Abs.GateRzTheta $2 }
  | 'U1' Angle { Frontend.LambdaQ.Abs.GateU1 $2 }
  | 'U2' '(' Angle ',' Angle ')' { Frontend.LambdaQ.Abs.GateU2 $3 $5 }
  | 'U3' '(' Angle ',' Angle ',' Angle ')' { Frontend.LambdaQ.Abs.GateU3 $3 $5 $7 }
  | 'SWAP' { Frontend.LambdaQ.Abs.GateSwp }
  | 'SQRT_SWAP' { Frontend.LambdaQ.Abs.GateSqrtSwp }
  | 'SQRT_SWAP_DAG' { Frontend.LambdaQ.Abs.GateSqrtSwpDag }
  | 'ISWAP' { Frontend.LambdaQ.Abs.GateISwp }
  | 'FSWAP' { Frontend.LambdaQ.Abs.GateFSwp }
  | 'SWAP_THETA' Angle { Frontend.LambdaQ.Abs.GateSwpTheta $2 }
  | 'ROOT_SWAP' Integer { Frontend.LambdaQ.Abs.GateSwpRt $2 }
  | 'ROOT_SWAP_DAG' Integer { Frontend.LambdaQ.Abs.GateSwpRtDag $2 }
  | 'QFT' Integer { Frontend.LambdaQ.Abs.GateQft $2 }
  | 'QFT_DAG' Integer { Frontend.LambdaQ.Abs.GateQftDag $2 }

ListVar :: { [Frontend.LambdaQ.Abs.Var] }
ListVar : Var { (:[]) $1 } | Var ',' ListVar { (:) $1 $3 }

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
ControlBit : '[' Integer ']' { Frontend.LambdaQ.Abs.CtrlBit $2 }

ControlBits :: { Frontend.LambdaQ.Abs.ControlBits }
ControlBits
  : '[' Integer ',' ListInteger ']' { Frontend.LambdaQ.Abs.CtrlBits $2 $4 }

ListInteger :: { [Integer] }
ListInteger
  : Integer { (:[]) $1 } | Integer ',' ListInteger { (:) $1 $3 }

Tuple :: { Frontend.LambdaQ.Abs.Tuple }
Tuple
  : '(' Term ',' ListTerm ')' { Frontend.LambdaQ.Abs.Tupl $2 $4 }

ControlTerm :: { Frontend.LambdaQ.Abs.ControlTerm }
ControlTerm : '[' Term ']' { Frontend.LambdaQ.Abs.CtrlTerm $2 }

ControlTerms :: { Frontend.LambdaQ.Abs.ControlTerms }
ControlTerms
  : '[' Term ',' ListTerm ']' { Frontend.LambdaQ.Abs.CtrlTerms $2 $4 }

ListTerm :: { [Frontend.LambdaQ.Abs.Term] }
ListTerm : Term { (:[]) $1 } | Term ',' ListTerm { (:) $1 $3 }

Term1 :: { Frontend.LambdaQ.Abs.Term }
Term1
  : 'if' Term 'then' Term 'else' Term { Frontend.LambdaQ.Abs.TermIfElse $2 $4 $6 }
  | 'let' '{' Var '=' Term '}' 'in' Term { Frontend.LambdaQ.Abs.TermLetSingle $3 $5 $8 }
  | 'let' '{' '(' Var ',' ListVar ')' '=' Term '}' 'in' Term { Frontend.LambdaQ.Abs.TermLetMultiple $4 $6 $9 $12 }
  | Var '<-' Term ';' Term { Frontend.LambdaQ.Abs.TermLetSugarSingle $1 $3 $5 }
  | Var ',' ListVar '<-' Term ';' Term { Frontend.LambdaQ.Abs.TermLetSugarMultiple $1 $3 $5 $7 }
  | 'case' Term 'of' CaseExpression ListCaseExpression { Frontend.LambdaQ.Abs.TermCase $2 $4 $5 }
  | Lambda Var Type '.' Term { Frontend.LambdaQ.Abs.TermLambda $1 $2 $3 $5 }
  | Term2 '$' Term1 { Frontend.LambdaQ.Abs.TermDollar $1 $3 }
  | Term2 { $1 }

Term2 :: { Frontend.LambdaQ.Abs.Term }
Term2
  : 'with' ControlTerm 'ctrl' ControlBasisState { Frontend.LambdaQ.Abs.TermQuantumCtrlGate $2 $4 }
  | 'with' ControlTerms 'ctrl' ControlBasisStates { Frontend.LambdaQ.Abs.TermQuantumCtrlsGate $2 $4 }
  | 'with' ControlTerm 'ctrl' ControlBit { Frontend.LambdaQ.Abs.TermClassicCtrlGate $2 $4 }
  | 'with' ControlTerms 'ctrl' ControlBits { Frontend.LambdaQ.Abs.TermClassicCtrlsGate $2 $4 }
  | Term2 Term3 { Frontend.LambdaQ.Abs.TermApply $1 $2 }
  | Term2 '.' Term3 { Frontend.LambdaQ.Abs.TermCompose $1 $3 }
  | Term3 { $1 }

Term3 :: { Frontend.LambdaQ.Abs.Term }
Term3
  : Var { Frontend.LambdaQ.Abs.TermVariable $1 }
  | BasisState { Frontend.LambdaQ.Abs.TermBasisState $1 }
  | 'gate' Gate { Frontend.LambdaQ.Abs.TermGate $2 }
  | Tuple { Frontend.LambdaQ.Abs.TermTuple $1 }
  | Bit { Frontend.LambdaQ.Abs.TermBit $1 }
  | '()' { Frontend.LambdaQ.Abs.TermUnit }
  | '(' Term ')' { $2 }

Term :: { Frontend.LambdaQ.Abs.Term }
Term : Term1 { $1 }

CaseExpression :: { Frontend.LambdaQ.Abs.CaseExpression }
CaseExpression
  : Term '->' Term { Frontend.LambdaQ.Abs.CaseExp $1 $3 }

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

ArithmExpr :: { Frontend.LambdaQ.Abs.ArithmExpr }
ArithmExpr
  : ArithmExpr '+' ArithmTerm { Frontend.LambdaQ.Abs.ArithmExprAdd $1 $3 }
  | ArithmExpr '-' ArithmTerm { Frontend.LambdaQ.Abs.ArithmExprSub $1 $3 }
  | ArithmTerm { Frontend.LambdaQ.Abs.ArithmExprTerm $1 }

ArithmTerm :: { Frontend.LambdaQ.Abs.ArithmTerm }
ArithmTerm
  : ArithmTerm '*' ArithmFactor { Frontend.LambdaQ.Abs.ArithmTermMul $1 $3 }
  | ArithmTerm '/' ArithmFactor { Frontend.LambdaQ.Abs.ArithmTermDiv $1 $3 }
  | ArithmFactor { Frontend.LambdaQ.Abs.ArithmTermFactor $1 }

ArithmFactor :: { Frontend.LambdaQ.Abs.ArithmFactor }
ArithmFactor
  : Integer { Frontend.LambdaQ.Abs.ArithmFactorInt $1 }
  | '(' ArithmExpr ')' { Frontend.LambdaQ.Abs.ArithmFactorExpr $2 }
  | ArithmFactor1 { $1 }

ArithmFactor1 :: { Frontend.LambdaQ.Abs.ArithmFactor }
ArithmFactor1 : ArithmFactor2 { $1 }

ArithmFactor2 :: { Frontend.LambdaQ.Abs.ArithmFactor }
ArithmFactor2 : '(' ArithmFactor ')' { $2 }

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


-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language LambdaQ.

module Frontend.LambdaQ.Abs where

import Prelude (Double, Integer, String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Int, Maybe(..)
  )
import qualified Data.String

data Program = ProgDef [FunctionDeclaration]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IntegerExpression
    = ArithmExprMinus IntegerExpression
    | ArithmExprAdd IntegerExpression IntegerExpression
    | ArithmExprSub IntegerExpression IntegerExpression
    | ArithmExprMul IntegerExpression IntegerExpression
    | ArithmExprDiv IntegerExpression IntegerExpression
    | ArithmExprInt Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BoolValue = BoolValueTrue | BoolValueFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BoolExpression
    = BoolExpressionAnd BoolExpression BoolExpression
    | BoolExpressionOr BoolExpression BoolExpression
    | BoolExpressionNot BoolExpression
    | BoolExpressionEq BoolExpression BoolExpression
    | BoolExpressionDif BoolExpression BoolExpression
    | BoolExpressionEqInt IntegerExpression IntegerExpression
    | BoolExpressionDifInt IntegerExpression IntegerExpression
    | BoolExpressionGt IntegerExpression IntegerExpression
    | BoolExpressionGe IntegerExpression IntegerExpression
    | BoolExpressionLt IntegerExpression IntegerExpression
    | BoolExpressionLe IntegerExpression IntegerExpression
    | BoolExpressionVal BoolValue
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = TypeFunction Type Type
    | TypeTensorProd Type Type
    | TypeExp Type Integer
    | TypeNonLinear Type
    | TypeBool
    | TypeBit
    | TypeInteger
    | TypeQbit
    | TypeBasisState
    | TypeUnit
    | TypeList Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Angle = AngleValue Double
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BasisState
    = BasisStateZero
    | BasisStateOne
    | BasisStatePlus
    | BasisStateMinus
    | BasisStatePlusI
    | BasisStateMinusI
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Gate
    = GateH
    | GateX
    | GateY
    | GateZ
    | GateID
    | GateXRootInt Integer
    | GateXRootVar Var
    | GateXRootDagInt Integer
    | GateXRootDagVar Var
    | GateYRootInt Integer
    | GateYRootVar Var
    | GateYRootDagInt Integer
    | GateYRootDagVar Var
    | GateZRootInt Integer
    | GateZRootVar Var
    | GateZRootDagInt Integer
    | GateZRootDagVar Var
    | GateS
    | GateSDag
    | GateT
    | GateTDag
    | GateSqrtX
    | GateSqrtXDag
    | GateSqrtY
    | GateSqrtYDag
    | GateRxTheta Angle
    | GateRyTheta Angle
    | GateRzTheta Angle
    | GateU1 Angle
    | GateU2 Angle Angle
    | GateU3 Angle Angle Angle
    | GateSwp
    | GateSqrtSwp
    | GateSqrtSwpDag
    | GateISwp
    | GateFSwp
    | GateSwpTheta Angle
    | GateSwpRtInt Integer
    | GateSwpRtVar Var
    | GateSwpRtDagInt Integer
    | GateSwpRtDagVar Var
    | GateQftInt Integer
    | GateQftVar Var
    | GateQftDagInt Integer
    | GateQftDagVar Var
    | GateUnknown3Angle GateVar Angle Angle Angle
    | GateUnknown2Angle GateVar Angle Angle
    | GateUnknown1Angle GateVar Angle
    | GateUnknownInt GateVar Integer
    | GateUnknownVar GateVar Var
    | GateUnknownSimple GateVar
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlBasisState = CtrlBasisState BasisState
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlBasisStates = CtrlBasisStates BasisState [BasisState]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlBit = CtrlBit Bit
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlBits = CtrlBits Bit [Bit]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlTerm = CtrlTerm Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlTerms = CtrlTerms Term [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlVar = CtrlVar Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlVars = CtrlVars Var [Var]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Term
    = TermListElement List Integer
    | TermUnit
    | TermBit Bit
    | TermBasisState BasisState
    | TermBoolExpression BoolExpression
    | TermIntegerExpression IntegerExpression
    | TermGate Gate
    | TermList List
    | TermVariable Var
    | TermTuple Term [Term]
    | TermGateQuantumCtrl ControlTerm ControlBasisState
    | TermGateQuantumTCtrls ControlTerms ControlBasisStates
    | TermGateQuantumVCtrls ControlVars ControlBasisStates
    | TermGateClassicCtrl ControlTerm ControlBit
    | TermGateClassicTCtrls ControlTerms ControlBits
    | TermGateClassicVCtrls ControlVars ControlBits
    | TermApply Term Term
    | TermCompose Term Term
    | TermTensorProduct Term Term
    | TermIfElse Term Term Term
    | TermLetSingle Var Term Term
    | TermLetMultiple Var [Var] Term Term
    | TermLetSugarSingle Var Term Term
    | TermLetSugarMultiple Var [Var] Term Term
    | TermCase Term [CaseExpression]
    | TermLambda Lambda Var Type Term
    | TermDollar Term Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data List
    = ListNil
    | ListSingle Term
    | ListMultiple Term [Term]
    | ListExpressionAdd List List
    | ListCons Term List
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CaseExpression = CaseExpr Term Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = FunArg Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionDefinition = FunDef Var [Arg] Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionType = FunType Var Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionDeclaration = FunDecl FunctionType FunctionDefinition
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype GateVar = GateVar ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Var = Var ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Bit = Bit String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Lambda = Lambda String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition GateVar where
  hasPosition (GateVar (p, _)) = C.Just p

instance HasPosition Var where
  hasPosition (Var (p, _)) = C.Just p


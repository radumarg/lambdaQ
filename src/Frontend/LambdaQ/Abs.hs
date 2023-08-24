-- File generated by the BNF Converter (bnfc 2.9.4.1).

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

data Type
    = TypeBit
    | TypeQbit
    | TypeUnit
    | TypeNonLin Type
    | TypeExp Type Integer
    | TypeTensr Type Type
    | TypeFunc Type Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Angle = Angle Double
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
    | GateXRt Integer
    | GateXRtDag Integer
    | GateYRt Integer
    | GateYRtDag Integer
    | GateZRt Integer
    | GateZRtDag Integer
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
    | GateSwpRt Integer
    | GateSwpRtDag Integer
    | GateGeneric GateIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LetVariable = LetVar Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Tuple = Tup Term [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Controls = Ctrls Term [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlBasisStates = CtrlStates BasisState [BasisState]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Term
    = TermQubit BasisState
    | TermVar Var
    | TermTupl Tuple
    | TermUnit
    | TermIfElse Term Term Term
    | TermLet LetVariable [LetVariable] Term Term
    | TermLetSugar LetVariable [LetVariable] Term Term
    | TermCase Term CaseExpression [CaseExpression]
    | TermLambda Lambda FunctionType Term
    | TermGate Gate
    | TermCtrlGate Controls ControlBasisStates Gate
    | TermApp Term Term
    | TermDollar Term Term
    | TermCompose Term Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CaseExpression = CaseExp Term Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = FunArg Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionDefinition = FunDef Var [Arg] Term
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionType = FunType Var Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionDeclaration = FunDecl FunctionType FunctionDefinition
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype GateIdent = GateIdent ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Var = Var ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

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

instance HasPosition GateIdent where
  hasPosition (GateIdent (p, _)) = C.Just p

instance HasPosition Var where
  hasPosition (Var (p, _)) = C.Just p


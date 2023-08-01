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

data Angle = AAngl Double
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ControlState
    = CStateZero
    | CStateOne
    | CStatePlus
    | CStateMinus
    | CStatePlusI
    | CStateMinusI
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Control = CCtrl Term ControlState
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Gate
    = GateH Term
    | GateHC Term [Control]
    | GateX Term
    | GateXC Term [Control]
    | GateY Term
    | GateYC Term [Control]
    | GateZ Term
    | GateZC Term [Control]
    | GateI Term
    | GateXRt Integer Term
    | GateXRtC Integer Term [Control]
    | GateXRtDag Integer Term
    | GateXRtDagC Integer Term [Control]
    | GateYRt Integer Term
    | GateYRtC Integer Term [Control]
    | GateYRtDag Integer Term
    | GateYRtDagC Integer Term [Control]
    | GateZRt Integer Term
    | GateZRtC Integer Term [Control]
    | GateZRtDag Integer Term
    | GateZRtDagC Integer Term [Control]
    | GateS Term
    | GateSC Term [Control]
    | GateSDag Term
    | GateSDagC Term [Control]
    | GateT Term
    | GateTC Term [Control]
    | GateTDag Term
    | GateTDagC Term [Control]
    | GateSqrtX Term
    | GateSqrtXC Term [Control]
    | GateSqrtXDag Term
    | GateSqrtXDagC Term [Control]
    | GateSqrtY Term
    | GateSqrtYC Term [Control]
    | GateSqrtYDag Term
    | GateSqrtYDagC Term [Control]
    | GateRxTheta Angle Term
    | GateRxThetaC Angle Term [Control]
    | GateRyTheta Angle Term
    | GateRyThetaC Angle Term [Control]
    | GateRzTheta Angle Term
    | GateRzThetaC Angle Term [Control]
    | GateU1 Angle Term
    | GateU1C Angle Term [Control]
    | GateU2 Angle Angle Term
    | GateU2C Angle Angle Term [Control]
    | GateU3 Angle Angle Angle Term
    | GateU3C Angle Angle Angle Term [Control]
    | GateSwp Term
    | GateSwpC Term [Control]
    | GateSqrtSwp Term
    | GateSqrtSwpC Term [Control]
    | GateSqrtSwpDag Term
    | GateSqrtSwpDagC Term [Control]
    | GateISwp Term
    | GateISwpC Term [Control]
    | GateFSwp Term
    | GateFSwpC Term [Control]
    | GateSwpTheta Term Angle
    | GateSwpThetaC Angle Term [Control]
    | GateSwpRt Term Integer
    | GateSwpRtC Integer Term [Control]
    | GateSwpRtDag Integer Term
    | GateSwpRtDagC Integer Term [Control]
    | GateGeneric GateIdent Term
    | GateGenericC GateIdent Term [Control]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LetVariable = LetVar Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LambdaVariable = LambdaVar Var
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Tuple = Tup Term [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Term
    = TVar Var
    | TBit Bit
    | TGate Gate
    | TTup Tuple
    | TUnit
    | TIfEl Term Term Term
    | TLet1 LetVariable Term Term
    | TLet2 LetVariable [LetVariable] Term Term
    | TCase Term CaseExpression [CaseExpression]
    | TLmbd Lambda LambdaVariable [LambdaVariable] Term
    | TApp Term Term
    | TDollr Term Term
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

newtype Bit = Bit ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

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

instance HasPosition Bit where
  hasPosition (Bit (p, _)) = C.Just p


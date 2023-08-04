-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms 
--   *  introduce De Bruijn indices for bound variables
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}

module Backend.IAST where

import qualified Frontend.LambdaQ.Abs as GenAbSyntax
import qualified Data.Map as Map

--- Types will be declared first below ---

data Type = 
    TypeBit           |
    TypeQbit          |
    TypeUnit          |
    TypeNonLin Type   |
    Type :*: Type     |
    Type :**: Integer |
    Type :->: Type 
  deriving (Eq, Ord, Show, Read)

infixr 1 :->:
infixr 2 :**:
infixr 3 :*:

data ControlState =
    CStateZero   |
    CStateOne    |
    CStatePlus   |
    CStateMinus  |
    CStatePlusI  |
    CStateMinusI
  deriving (Eq, Ord, Show, Read)

data Control = CCtrl ControlState Term
  deriving (Eq, Ord, Show, Read)

data Angle where
  AAngl :: Double -> Angle
  deriving (Eq, Ord, Show, Read)

data BitValue = BitZero | BitOne
  deriving (Eq, Ord, Show, Read)

newtype Bit = Bit ((Int, Int), BitValue)
  deriving (Eq, Ord, Show, Read)

-- Integer arguments correspond to line and column occurence in
-- the source code, used for providing more relevant error messages.
newtype GateIdent = GateIdent ((Int, Int), String)
  deriving (Eq, Ord, Show, Read)

data Gate =
    GateH                               |
    GateHC [Control]                    |
    GateX                               |
    GateXC [Control]                    |
    GateY                               |
    GateYC [Control]                    |
    GateZ                               |
    GateZC [Control]                    |
    GateI                               |
    GateXRt Integer                     |
    GateXRtC Integer [Control]          |
    GateXRtDag Integer                  |
    GateXRtDagC Integer [Control]       |
    GateYRt Integer                     |
    GateYRtC Integer [Control]          |
    GateYRtDag Integer                  |
    GateYRtDagC Integer [Control]       |
    GateZRt Integer                     |
    GateZRtC Integer [Control]          |
    GateZRtDag Integer                  |
    GateZRtDagC Integer [Control]       |
    GateS                               |
    GateSC [Control]                    |
    GateSDag                            |
    GateSDagC [Control]                 |
    GateT                               |
    GateTC [Control]                    |
    GateTDag                            |
    GateTDagC [Control]                 |
    GateSqrtX                           |
    GateSqrtXC [Control]                |
    GateSqrtXDag                        |
    GateSqrtXDagC [Control]             |
    GateSqrtY                           |
    GateSqrtYC [Control]                |
    GateSqrtYDag                        |
    GateSqrtYDagC [Control]             |
    GateRxTheta Angle                   |
    GateRxThetaC Angle [Control]        |
    GateRyTheta Angle                   |
    GateRyThetaC Angle [Control]        |
    GateRzTheta Angle                   |
    GateRzThetaC Angle [Control]        |
    GateU1 Angle                        |
    GateU1C Angle [Control]             |
    GateU2 Angle Angle                  |
    GateU2C Angle Angle [Control]       |
    GateU3 Angle Angle Angle            |
    GateU3C Angle Angle Angle [Control] |
    GateSwp                             |
    GateSwpC [Control]                  |
    GateSqrtSwp                         |
    GateSqrtSwpC [Control]              |
    GateSqrtSwpDag                      |
    GateSqrtSwpDagC [Control]           |
    GateISwp                            |
    GateISwpC [Control]                 |
    GateFSwp                            |
    GateSwpTheta Angle                  |
    GateFSwpC [Control]                 |
    GateSwpRt Integer                   |
    GateSwpRtC Integer [Control]        |
    GateSwpRtDag Integer                |
    GateSwpRtDagC Integer [Control]     |
    GateGeneric GateIdent               |
    GateGenericC GateIdent [Control]
  deriving (Eq, Ord, Show, Read)

data Term =
    TermFunction String                 |
    TermBit Bit                         |
    TermGate Gate                       |
    TermTuple Term Term                 |
    TermApp Term Term                   |
    TermIfEl Term Term Term             |
    TermLet Term Term                   |
    TermLambda Type Term                |
    TermNew                             |
    TermMeasure                         |
    TermUnit
  deriving (Eq, Ord, Show, Read)

data Function = Func String Type Term
type Program = [Function]


--- Functions will be declared below ---

mapType :: GenAbSyntax.Type -> Type
mapType GenAbSyntax.TypeBit   = TypeBit
mapType GenAbSyntax.TypeQbit  = TypeQbit
mapType GenAbSyntax.TypeUnit  = TypeUnit
mapType (GenAbSyntax.TypeNonLin t) = TypeNonLin (mapType t)
mapType (GenAbSyntax.TypeTensr l r) = mapType l :*: mapType r
mapType (GenAbSyntax.TypeExp t i) = mapType t :**: i
mapType (GenAbSyntax.TypeFunc l r) = mapType l :->: mapType r

mapControlState :: GenAbSyntax.ControlState -> ControlState
mapControlState GenAbSyntax.CStateZero = CStateZero
mapControlState GenAbSyntax.CStateOne = CStateOne
mapControlState GenAbSyntax.CStatePlus = CStatePlus
mapControlState GenAbSyntax.CStateMinus = CStateMinus
mapControlState GenAbSyntax.CStatePlusI = CStatePlusI
mapControlState GenAbSyntax.CStateMinusI = CStateMinusI

-- mapControl :: GenAbSyntax.Control -> Control
-- mapControl (GenAbSyntax.CCtrl ctrlState term) = CCtrl (mapControlState ctrlState) (mapTerm Map.empty term)

mapAngle :: GenAbSyntax.Angle -> Angle
mapAngle (GenAbSyntax.AAngl angle) = AAngl angle

reverseMapAngle :: Angle -> GenAbSyntax.Angle
reverseMapAngle (AAngl angle) = GenAbSyntax.AAngl angle

--mapBit :: GenAbSyntax.Bit -> Bit
--mapBit (GenAbSyntax.Bit ((l, c) 0)) = undefined
--mapBit (GenAbSyntax.Bit ((l, c) 1)) = undefined

mapGate :: GenAbSyntax.Gate -> Gate
mapGate g = undefined
-- mapGate g = case g of 
--     GenAbSyntax.GateH ->  GateH
--     GenAbSyntax.GateHC ctrls -> GateHC (map mapControl ctrls)
--     GenAbSyntax.GateX -> GateX
--     GenAbSyntax.GateXC ctrls -> GateXC (map mapControl ctrls)
--     GenAbSyntax.GateY -> GateY
--     GenAbSyntax.GateYC ctrls -> GateYC (map mapControl ctrls)
--     GenAbSyntax.GateZ -> GateZ
--     GenAbSyntax.GateZC ctrls -> GateZC (map mapControl ctrls)
--     GenAbSyntax.GateI -> GateI
--     GenAbSyntax.GateXRt rt -> GateXRt rt
--     GenAbSyntax.GateXRtC rt ctrls -> GateXRtC rt (map mapControl ctrls)
--     GenAbSyntax.GateXRtDag rt -> GateXRtDag rt
--     GenAbSyntax.GateXRtDagC rt ctrls -> GateXRtDagC rt (map mapControl ctrls)
--     GenAbSyntax.GateYRt rt -> GateYRt rt
--     GenAbSyntax.GateYRtC rt ctrls -> GateYRtC rt (map mapControl ctrls)
--     GenAbSyntax.GateYRtDag rt -> GateYRtDag rt
--     GenAbSyntax.GateYRtDagC rt ctrls -> GateYRtDagC rt (map mapControl ctrls)
--     GenAbSyntax.GateZRt rt -> GateZRt rt
--     GenAbSyntax.GateZRtC rt ctrls -> GateZRtC rt (map mapControl ctrls)
--     GenAbSyntax.GateZRtDag rt -> GateZRtDag rt
--     GenAbSyntax.GateZRtDagC rt ctrls -> GateZRtDagC rt (map mapControl ctrls)
--     GenAbSyntax.GateS -> GateS
--     GenAbSyntax.GateSC ctrls -> GateSC (map mapControl ctrls)
--     GenAbSyntax.GateSDag -> GateSDag
--     GenAbSyntax.GateSDagC ctrls -> GateSDagC (map mapControl ctrls)
--     GenAbSyntax.GateT -> GateT
--     GenAbSyntax.GateTC ctrls -> GateTC (map mapControl ctrls)
--     GenAbSyntax.GateTDag -> GateTDag
--     GenAbSyntax.GateTDagC ctrls -> GateTDagC (map mapControl ctrls)
--     GenAbSyntax.GateSqrtX -> GateSqrtX
--     GenAbSyntax.GateSqrtXC ctrls -> GateSqrtXC (map mapControl ctrls)
--     GenAbSyntax.GateSqrtXDag -> GateSqrtXDag
--     GenAbSyntax.GateSqrtXDagC ctrls -> GateSqrtXDagC (map mapControl ctrls)
--     GenAbSyntax.GateSqrtY -> GateSqrtY
--     GenAbSyntax.GateSqrtYC ctrls -> GateSqrtYC (map mapControl ctrls)
--     GenAbSyntax.GateSqrtYDag -> GateSqrtYDag 
--     GenAbSyntax.GateSqrtYDagC ctrls -> GateSqrtYDagC (map mapControl ctrls)
--     GenAbSyntax.GateRxTheta angle -> GateRxTheta (mapAngle angle)
--     GenAbSyntax.GateRxThetaC angle ctrls -> GateRxThetaC (mapAngle angle) (map mapControl ctrls)
--     GenAbSyntax.GateRyTheta angle -> GateRyTheta (mapAngle angle)
--     GenAbSyntax.GateRyThetaC angle ctrls -> GateRyThetaC (mapAngle angle) (map mapControl ctrls)
--     GenAbSyntax.GateRzTheta angle -> GateRzTheta (mapAngle angle)
--     GenAbSyntax.GateRzThetaC angle ctrls -> GateRzThetaC (mapAngle angle) (map mapControl ctrls)
--     GenAbSyntax.GateU1 angle -> GateU1 (mapAngle angle)
--     GenAbSyntax.GateU1C angle ctrls -> GateU1C (mapAngle angle) (map mapControl ctrls)
--     GenAbSyntax.GateU2 angle1 angle2 -> GateU2 (mapAngle angle1) (mapAngle angle2)
--     GenAbSyntax.GateU2C angle1 angle2 ctrls -> GateU2C (mapAngle angle1) (mapAngle angle2) (map mapControl ctrls)
--     GenAbSyntax.GateU3 angle1 angle2 angle3 -> GateU3 (mapAngle angle1) (mapAngle angle2) (mapAngle angle3)
--     GenAbSyntax.GateU3C angle1 angle2 angle3 ctrls -> GateU3C (mapAngle angle1) (mapAngle angle2) (mapAngle angle3) (map mapControl ctrls) 
--     GenAbSyntax.GateSwp -> GateSwp 
--     GenAbSyntax.GateSwpC ctrls -> GateSwpC (map mapControl ctrls) 
--     GenAbSyntax.GateSqrtSwp -> GateSqrtSwp
--     GenAbSyntax.GateSqrtSwpC ctrls -> GateSqrtSwpC (map mapControl ctrls)
--     GenAbSyntax.GateSqrtSwpDag -> GateSqrtSwpDag 
--     GenAbSyntax.GateSqrtSwpDagC ctrls -> GateSqrtSwpDagC (map mapControl ctrls) 
--     GenAbSyntax.GateISwp -> GateISwp
--     GenAbSyntax.GateISwpC ctrls -> GateISwpC (map mapControl ctrls)
--     GenAbSyntax.GateFSwp -> GateFSwp 
--     GenAbSyntax.GateFSwpC ctrls -> GateFSwpC (map mapControl ctrls)
--     GenAbSyntax.GateSwpTheta angle -> GateSwpTheta (mapAngle angle)
--     GenAbSyntax.GateSwpRt rt -> GateSwpRt rt 
--     GenAbSyntax.GateSwpRtC rt ctrls -> GateSwpRtC rt (map mapControl ctrls)
--     GenAbSyntax.GateSwpRtDag rt -> GateSwpRtDag rt 
--     GenAbSyntax.GateSwpRtDagC rt ctrls -> GateSwpRtDagC rt (map mapControl ctrls)
--     GenAbSyntax.GateGeneric name -> undefined             -- TODO: map generic gates -
--     GenAbSyntax.GateGenericC name ctrls -> undefined      -- TODO: map generic gates -

type Env = Map.Map String Integer

mapTerm :: Env -> GenAbSyntax.Term -> Term
mapTerm = undefined


-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms 
--   *  introduce De Bruijn indices for bound variables
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Backend.IAST where

import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

--- Types will be declared below ---

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

data Angle = AAngl Double
  deriving (Eq, Ord, Show, Read)

type GateIdent = String

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
    GateGate GateIdent                  |
    GateGateC GateIdent [Control]
  deriving (Eq, Ord, Show, Read)

data Term =
    Fun String                      |
    Bit GeneratedAbstractSyntax.Bit |
    Gate Gate                       |
    Tup Term Term                   |
    App  Term Term                  |
    IfEl Term Term Term             |
    Let Term Term                   |
    Lamb Type Term                  |
    New                             |
    Measure                         |
    Unit
  deriving (Eq, Ord, Show, Read)

data Function = Func String Type Term
type Program = [Function]


--- Functions will be declared below ---

mapType :: GeneratedAbstractSyntax.Type -> Type
mapType GeneratedAbstractSyntax.TypeBit   = TypeBit
mapType GeneratedAbstractSyntax.TypeQbit  = TypeQbit
mapType GeneratedAbstractSyntax.TypeUnit  = TypeUnit
mapType (GeneratedAbstractSyntax.TypeNonLin t) = TypeNonLin (mapType t)
mapType (GeneratedAbstractSyntax.TypeTensr l r) = mapType l :*: mapType r
mapType (GeneratedAbstractSyntax.TypeExp t i) = mapType t :**: i
mapType (GeneratedAbstractSyntax.TypeFunc l r) = mapType l :->: mapType r

reverseMapType :: Type -> GeneratedAbstractSyntax.Type
reverseMapType TypeBit  = GeneratedAbstractSyntax.TypeBit
reverseMapType TypeQbit = GeneratedAbstractSyntax.TypeQbit
reverseMapType TypeUnit = GeneratedAbstractSyntax.TypeUnit
reverseMapType (TypeNonLin t) = GeneratedAbstractSyntax.TypeNonLin (reverseMapType t)
reverseMapType (l :*: r) = GeneratedAbstractSyntax.TypeTensr (reverseMapType l) (reverseMapType r)
reverseMapType (t :**: i) = GeneratedAbstractSyntax.TypeExp (reverseMapType t) i
reverseMapType (l :->: r) = GeneratedAbstractSyntax.TypeFunc (reverseMapType l) (reverseMapType r)

mapControlState :: GeneratedAbstractSyntax.ControlState -> ControlState
mapControlState GeneratedAbstractSyntax.CStateZero = CStateZero
mapControlState GeneratedAbstractSyntax.CStateOne = CStateOne
mapControlState GeneratedAbstractSyntax.CStatePlus = CStatePlus
mapControlState GeneratedAbstractSyntax.CStateMinus = CStateMinus
mapControlState GeneratedAbstractSyntax.CStatePlusI = CStatePlusI
mapControlState GeneratedAbstractSyntax.CStateMinusI = CStateMinusI

reverseMapControlState :: ControlState -> GeneratedAbstractSyntax.ControlState
reverseMapControlState CStateZero = GeneratedAbstractSyntax.CStateZero
reverseMapControlState CStateOne = GeneratedAbstractSyntax.CStateOne
reverseMapControlState CStatePlus = GeneratedAbstractSyntax.CStatePlus
reverseMapControlState CStateMinus = GeneratedAbstractSyntax.CStateMinus
reverseMapControlState CStatePlusI = GeneratedAbstractSyntax.CStatePlusI
reverseMapControlState CStateMinusI = GeneratedAbstractSyntax.CStateMinusI

mapControl :: GeneratedAbstractSyntax.Control -> Control
mapControl (GeneratedAbstractSyntax.CCtrl ctrlState term) = CCtrl (mapControlState ctrlState) (mapTerm "" term)

reverseMapControl :: Control -> GeneratedAbstractSyntax.Control
reverseMapControl (CCtrl ctrlState term) = GeneratedAbstractSyntax.CCtrl (reverseMapControlState ctrlState) (reverseMapTerm "" term)

mapAngle:: GeneratedAbstractSyntax.Angle -> Angle
mapAngle (GeneratedAbstractSyntax.AAngl angle) = AAngl angle

reverseMapAngle:: Angle -> GeneratedAbstractSyntax.Angle
reverseMapAngle (AAngl angle) = GeneratedAbstractSyntax.AAngl angle

mapGate :: GeneratedAbstractSyntax.Gate -> Gate
mapGate g = case g of 
    GeneratedAbstractSyntax.GateH ->  GateH
    GeneratedAbstractSyntax.GateHC ctrls -> GateHC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateX -> GateX
    GeneratedAbstractSyntax.GateXC ctrls -> GateXC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateY -> GateY
    GeneratedAbstractSyntax.GateYC ctrls -> GateYC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateZ -> GateZ
    GeneratedAbstractSyntax.GateZC ctrls -> GateZC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateI -> GateI
    GeneratedAbstractSyntax.GateXRt rt -> GateXRt rt
    GeneratedAbstractSyntax.GateXRtC rt ctrls -> GateXRtC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateXRtDag rt -> GateXRtDag rt
    GeneratedAbstractSyntax.GateXRtDagC rt ctrls -> GateXRtDagC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateYRt rt -> GateYRt rt
    GeneratedAbstractSyntax.GateYRtC rt ctrls -> GateYRtC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateYRtDag rt -> GateYRtDag rt
    GeneratedAbstractSyntax.GateYRtDagC rt ctrls -> GateYRtDagC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateZRt rt -> GateZRt rt
    GeneratedAbstractSyntax.GateZRtC rt ctrls -> GateZRtC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateZRtDag rt -> GateZRtDag rt
    GeneratedAbstractSyntax.GateZRtDagC rt ctrls -> GateZRtDagC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateS -> GateS
    GeneratedAbstractSyntax.GateSC ctrls -> GateSC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSDag -> GateSDag
    GeneratedAbstractSyntax.GateSDagC ctrls -> GateSDagC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateT -> GateT
    GeneratedAbstractSyntax.GateTC ctrls -> GateTC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateTDag -> GateTDag
    GeneratedAbstractSyntax.GateTDagC ctrls -> GateTDagC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSqrtX -> GateSqrtX
    GeneratedAbstractSyntax.GateSqrtXC ctrls -> GateSqrtXC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSqrtXDag -> GateSqrtXDag
    GeneratedAbstractSyntax.GateSqrtXDagC ctrls -> GateSqrtXDagC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSqrtY -> GateSqrtY
    GeneratedAbstractSyntax.GateSqrtYC ctrls -> GateSqrtYC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSqrtYDag -> GateSqrtYDag 
    GeneratedAbstractSyntax.GateSqrtYDagC ctrls -> GateSqrtYDagC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateRxTheta angle -> GateRxTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateRxThetaC angle ctrls -> GateRxThetaC (mapAngle angle) (map mapControl ctrls)
    GeneratedAbstractSyntax.GateRyTheta angle -> GateRyTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateRyThetaC angle ctrls -> GateRyThetaC (mapAngle angle) (map mapControl ctrls)
    GeneratedAbstractSyntax.GateRzTheta angle -> GateRzTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateRzThetaC angle ctrls -> GateRzThetaC (mapAngle angle) (map mapControl ctrls)
    GeneratedAbstractSyntax.GateU1 angle -> GateU1 (mapAngle angle)
    GeneratedAbstractSyntax.GateU1C angle ctrls -> GateU1C (mapAngle angle) (map mapControl ctrls)
    GeneratedAbstractSyntax.GateU2 angle1 angle2 -> GateU2 (mapAngle angle1) (mapAngle angle2)
    GeneratedAbstractSyntax.GateU2C angle1 angle2 ctrls -> GateU2C (mapAngle angle1) (mapAngle angle2) (map mapControl ctrls)
    GeneratedAbstractSyntax.GateU3 angle1 angle2 angle3 -> GateU3 (mapAngle angle1) (mapAngle angle2) (mapAngle angle3)
    GeneratedAbstractSyntax.GateU3C angle1 angle2 angle3 ctrls -> GateU3C (mapAngle angle1) (mapAngle angle2) (mapAngle angle3) (map mapControl ctrls) 
    GeneratedAbstractSyntax.GateSwp -> GateSwp 
    GeneratedAbstractSyntax.GateSwpC ctrls -> GateSwpC (map mapControl ctrls) 
    GeneratedAbstractSyntax.GateSqrtSwp -> GateSqrtSwp
    GeneratedAbstractSyntax.GateSqrtSwpC ctrls -> GateSqrtSwpC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSqrtSwpDag -> GateSqrtSwpDag 
    GeneratedAbstractSyntax.GateSqrtSwpDagC ctrls -> GateSqrtSwpDagC (map mapControl ctrls) 
    GeneratedAbstractSyntax.GateISwp -> GateISwp
    GeneratedAbstractSyntax.GateISwpC ctrls -> GateISwpC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateFSwp -> GateFSwp 
    GeneratedAbstractSyntax.GateFSwpC ctrls -> GateFSwpC (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSwpTheta angle -> GateSwpTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateSwpRt rt -> GateSwpRt rt 
    GeneratedAbstractSyntax.GateSwpRtC rt ctrls -> GateSwpRtC rt (map mapControl ctrls)
    GeneratedAbstractSyntax.GateSwpRtDag rt -> GateSwpRtDag rt 
    GeneratedAbstractSyntax.GateSwpRtDagC rt ctrls -> GateSwpRtDagC rt (map mapControl ctrls)

reverseMapGate :: Gate -> GeneratedAbstractSyntax.Gate
reverseMapGate g = case g of 
    GateH ->  GeneratedAbstractSyntax.GateH
    GateHC ctrls -> GeneratedAbstractSyntax.GateHC (map reverseMapControl ctrls)
    GateX -> GeneratedAbstractSyntax.GateX
    GateXC ctrls -> GeneratedAbstractSyntax.GateXC (map reverseMapControl ctrls)
    GateY -> GeneratedAbstractSyntax.GateY
    GateYC ctrls -> GeneratedAbstractSyntax.GateYC (map reverseMapControl ctrls)
    GateZ -> GeneratedAbstractSyntax.GateZ
    GateZC ctrls -> GeneratedAbstractSyntax.GateZC (map reverseMapControl ctrls)
    GateI -> GeneratedAbstractSyntax.GateI
    GateXRt rt -> GeneratedAbstractSyntax.GateXRt rt
    GateXRtC rt ctrls -> GeneratedAbstractSyntax.GateXRtC rt (map reverseMapControl ctrls)
    GateXRtDag rt -> GeneratedAbstractSyntax.GateXRtDag rt
    GateXRtDagC rt ctrls -> GeneratedAbstractSyntax.GateXRtDagC rt (map reverseMapControl ctrls)
    GateYRt rt -> GeneratedAbstractSyntax.GateYRt rt
    GateYRtC rt ctrls -> GeneratedAbstractSyntax.GateYRtC rt (map reverseMapControl ctrls)
    GateYRtDag rt -> GeneratedAbstractSyntax.GateYRtDag rt
    GateYRtDagC rt ctrls -> GeneratedAbstractSyntax.GateYRtDagC rt (map reverseMapControl ctrls)
    GateZRt rt -> GeneratedAbstractSyntax.GateZRt rt
    GateZRtC rt ctrls -> GeneratedAbstractSyntax.GateZRtC rt (map reverseMapControl ctrls)
    GateZRtDag rt -> GeneratedAbstractSyntax.GateZRtDag rt
    GateZRtDagC rt ctrls -> GeneratedAbstractSyntax.GateZRtDagC rt (map reverseMapControl ctrls)
    GateS -> GeneratedAbstractSyntax.GateS
    GateSC ctrls -> GeneratedAbstractSyntax.GateSC (map reverseMapControl ctrls)
    GateSDag -> GeneratedAbstractSyntax.GateSDag
    GateSDagC ctrls -> GeneratedAbstractSyntax.GateSDagC (map reverseMapControl ctrls)
    GateT -> GeneratedAbstractSyntax.GateT
    GateTC ctrls -> GeneratedAbstractSyntax.GateTC (map reverseMapControl ctrls)
    GateTDag -> GeneratedAbstractSyntax.GateTDag
    GateTDagC ctrls -> GeneratedAbstractSyntax.GateTDagC (map reverseMapControl ctrls)
    GateSqrtX -> GeneratedAbstractSyntax.GateSqrtX
    GateSqrtXC ctrls -> GeneratedAbstractSyntax.GateSqrtXC (map reverseMapControl ctrls)
    GateSqrtXDag -> GeneratedAbstractSyntax.GateSqrtXDag
    GateSqrtXDagC ctrls -> GeneratedAbstractSyntax.GateSqrtXDagC (map reverseMapControl ctrls)
    GateSqrtY -> GeneratedAbstractSyntax.GateSqrtY
    GateSqrtYC ctrls -> GeneratedAbstractSyntax.GateSqrtYC (map reverseMapControl ctrls)
    GateSqrtYDag -> GeneratedAbstractSyntax.GateSqrtYDag
    GateSqrtYDagC ctrls -> GeneratedAbstractSyntax.GateSqrtYDagC (map reverseMapControl ctrls)
    GateRxTheta angle -> GeneratedAbstractSyntax.GateRxTheta (reverseMapAngle angle)
    GateRxThetaC angle ctrls -> GeneratedAbstractSyntax.GateRxThetaC (reverseMapAngle angle) (map reverseMapControl ctrls)
    GateRyTheta angle -> GeneratedAbstractSyntax.GateRyTheta (reverseMapAngle angle)
    GateRyThetaC angle ctrls -> GeneratedAbstractSyntax.GateRyThetaC (reverseMapAngle angle) (map reverseMapControl ctrls)
    GateRzTheta angle -> GeneratedAbstractSyntax.GateRzTheta (reverseMapAngle angle)
    GateRzThetaC angle ctrls -> GeneratedAbstractSyntax.GateRzThetaC (reverseMapAngle angle) (map reverseMapControl ctrls)
    GateU1 angle -> GeneratedAbstractSyntax.GateU1 (reverseMapAngle angle)
    GateU1C angle ctrls -> GeneratedAbstractSyntax.GateU1C (reverseMapAngle angle) (map reverseMapControl ctrls)
    GateU2 angle1 angle2 -> GeneratedAbstractSyntax.GateU2 (reverseMapAngle angle1) (reverseMapAngle angle2)
    GateU2C angle1 angle2 ctrls -> GeneratedAbstractSyntax.GateU2C (reverseMapAngle angle1) (reverseMapAngle angle2) (map reverseMapControl ctrls)
    GateU3 angle1 angle2 angle3 -> GeneratedAbstractSyntax.GateU3 (reverseMapAngle angle1) (reverseMapAngle angle2) (reverseMapAngle angle3)
    GateU3C angle1 angle2 angle3 ctrls -> GeneratedAbstractSyntax.GateU3C (reverseMapAngle angle1) (reverseMapAngle angle2) (reverseMapAngle angle3) (map reverseMapControl ctrls)
    GateSwp -> GeneratedAbstractSyntax.GateSwp
    GateSwpC ctrls -> GeneratedAbstractSyntax.GateSwpC (map reverseMapControl ctrls) 
    GateSqrtSwp -> GeneratedAbstractSyntax.GateSqrtSwp
    GateSqrtSwpC ctrls -> GeneratedAbstractSyntax.GateSqrtSwpC (map reverseMapControl ctrls)
    GateSqrtSwpDag -> GeneratedAbstractSyntax.GateSqrtSwpDag
    GateSqrtSwpDagC ctrls -> GeneratedAbstractSyntax.GateSqrtSwpDagC (map reverseMapControl ctrls) 
    GateISwp -> GeneratedAbstractSyntax.GateISwp
    GateISwpC ctrls -> GeneratedAbstractSyntax.GateISwpC (map reverseMapControl ctrls)
    GateFSwp -> GeneratedAbstractSyntax.GateFSwp
    GateFSwpC ctrls -> GeneratedAbstractSyntax.GateFSwpC (map reverseMapControl ctrls)
    GateSwpTheta angle -> GeneratedAbstractSyntax.GateSwpTheta (reverseMapAngle angle)
    GateSwpRt rt -> GeneratedAbstractSyntax.GateSwpRt rt
    GateSwpRtC rt ctrls -> GeneratedAbstractSyntax.GateSwpRtC rt (map reverseMapControl ctrls)
    GateSwpRtDag rt -> GeneratedAbstractSyntax.GateSwpRtDag rt
    GateSwpRtDagC rt ctrls -> GeneratedAbstractSyntax.GateSwpRtDagC rt (map reverseMapControl ctrls)

type Env = String
mapTerm :: Env -> GeneratedAbstractSyntax.Term -> Term
mapTerm = undefined

reverseMapTerm :: Env -> Term -> GeneratedAbstractSyntax.Term
reverseMapTerm = undefined
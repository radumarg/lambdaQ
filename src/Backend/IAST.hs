-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree 
--   *  introduce De Bruijn indices for bound variables

module Backend.ASTConverter where

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

type Angle = Double

type GateIdent = String

data Gate =
    GH                               |
    GHC [Control]                    |
    GX                               |
    GXC [Control]                    |
    GY                               |
    GYC [Control]                    |
    GZ                               |
    GZC [Control]                    |
    GI                               |
    GXRt Integer                     |
    GXRtC Integer [Control]          |
    GXRtDag Integer                  |
    GXRtDagC Integer [Control]       |
    GYRt Integer                     |
    GYRtC Integer [Control]          |
    GYRtDag Integer                  |
    GYRtDagC Integer [Control]       |
    GZRt Integer                     |
    GZRtC Integer [Control]          |
    GZRtDag Integer                  |
    GZRtDagC Integer [Control]       |
    GS                               |
    GSC [Control]                    |
    GSDag                            |
    GSDagC [Control]                 |
    GT                               |
    GTC [Control]                    |
    GTDag                            |
    GTDagC [Control]                 |
    GSqrtX                           |
    GSqrtXC [Control]                |
    GSqrtXDag                        |
    GSqrtXDagC [Control]             |
    GSqrtY                           |
    GSqrtYC [Control]                |
    GSqrtYDag                        |
    GSqrtYDagC [Control]             |
    GRxTheta Angle                   |
    GRxThetaC Angle [Control]        |
    GRyTheta Angle                   |
    GRyThetaC Angle [Control]        |
    GRzTheta Angle                   |
    GRzThetaC Angle [Control]        |
    GU1 Angle                        |
    GU1C Angle [Control]             |
    GU2 Angle Angle                  |
    GU2C Angle Angle [Control]       |
    GU3 Angle Angle Angle            |
    GU3C Angle Angle Angle [Control] |
    GSwp                             |
    GSwpC [Control]                  |
    GSqrtSwp                         |
    GSqrtSwpC [Control]              |
    GSqrtSwpDag                      |
    GSqrtSwpDagC [Control]           |
    GISwp                            |
    GISwpC [Control]                 |
    GFSwp                            |
    GFSwpC [Control]                 |
    GSwpRt Integer                   |
    GSwpRtC Integer [Control]        |
    GSwpRtDag Integer                |
    GSwpRtDagC Integer [Control]     |
    GGate GateIdent                  |
    GGateC GateIdent [Control]
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

translateType :: GeneratedAbstractSyntax.Type -> Type
translateType GeneratedAbstractSyntax.TypeBit   = TypeBit
translateType GeneratedAbstractSyntax.TypeQbit  = TypeQbit
translateType GeneratedAbstractSyntax.TypeUnit  = TypeUnit
translateType (GeneratedAbstractSyntax.TypeNonLin t) = TypeNonLin (translateType t)
translateType (GeneratedAbstractSyntax.TypeTensr l r) = translateType l :*: translateType r
translateType (GeneratedAbstractSyntax.TypeExp t i) = translateType t :**: i
translateType (GeneratedAbstractSyntax.TypeFunc l r) = translateType l :->: translateType r

reverseTranslateType :: Type -> GeneratedAbstractSyntax.Type
reverseTranslateType TypeBit  = GeneratedAbstractSyntax.TypeBit
reverseTranslateType TypeQbit = GeneratedAbstractSyntax.TypeQbit
reverseTranslateType TypeUnit = GeneratedAbstractSyntax.TypeUnit
reverseTranslateType (TypeNonLin t) = GeneratedAbstractSyntax.TypeNonLin (reverseTranslateType t)
reverseTranslateType (l :*: r) = GeneratedAbstractSyntax.TypeTensr (reverseTranslateType l) (reverseTranslateType r)
reverseTranslateType (t :**: i) = GeneratedAbstractSyntax.TypeExp (reverseTranslateType t) i
reverseTranslateType (l :->: r) = GeneratedAbstractSyntax.TypeFunc (reverseTranslateType l) (reverseTranslateType r)







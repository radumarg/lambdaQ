-- This file contain the specification of the for the intermediate abstract syntax tree.
-- The intermediate abstract syntax tree hase a simpler syntax than the abstract syntax
-- generated by the parser, to make it easier to process by the semantic analyser, type
-- checker and the code generator:
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms 
--   *  introduce De Bruijn indices for bound variables

module Backend.IAST where

import qualified Frontend.LambdaQ.Abs as GenAbSyntax
import qualified Data.Map as Map

data Type = 
    TypeBit           |
    TypeQbit          |
    TypeUnit          |
    TypeNonLin Type   |
    Type :->: Type    |
    Type :*: Type     |
    Type :**: Integer 
  deriving (Eq, Ord, Show, Read)

infixr 1 :->:
infixr 2 :*:
infixr 3 :**:

data ControlState =
    CtrlStateZero   |
    CtrlStateOne    |
    CtrlStatePlus   |
    CtrlStateMinus  |
    CtrlStatePlusI  |
    CtrlStateMinusI
  deriving (Eq, Ord, Show, Read)

newtype Angle = Angle Double
  deriving (Eq, Ord, Show, Read)

data BitValue = BitZero | BitOne
  deriving (Eq, Ord, Show, Read)

-- Integer arguments correspond to line and column position in code
newtype Bit = Bit ((Int, Int), BitValue)
  deriving (Eq, Ord, Show, Read)

-- Integer arguments correspond to line and column position in code
newtype GateIdent = GateIdent ((Int, Int), String)
  deriving (Eq, Ord, Show, Read)

data Gate =
    GateH                      |
    GateX                      |
    GateY                      |
    GateZ                      |
    GateID                     |
    GateXRt Integer            |
    GateXRtDag Integer         |
    GateYRt Integer            |
    GateYRtDag Integer         |
    GateZRt Integer            |
    GateZRtDag Integer         |
    GateS                      |
    GateSDag                   |
    GateT                      |
    GateTDag                   |
    GateSqrtX                  |
    GateSqrtXDag               |
    GateSqrtY                  |
    GateSqrtYDag               |
    GateRxTheta Angle          |
    GateRyTheta Angle          |
    GateRzTheta Angle          |
    GateU1 Angle               |
    GateU2 Angle Angle         |
    GateU3 Angle Angle Angle   |
    GateSwp                    |
    GateSqrtSwp                |
    GateSqrtSwpDag             |
    GateISwp                   |
    GateISwpC                  |
    GateFSwp                   |
    GateSwpTheta Angle         |
    GateSwpRt Integer          |
    GateSwpRtDag Integer       |
    GateGeneric GateIdent 
  deriving (Eq, Ord, Show, Read)

data Term =
    TermFunction String               |
    TermBit Bit                       |
    TermGate Gate                     |
    TermTuple Term Term               |
    TermApp Term Term                 |
    TermDollar Term Term              |
    TermIfElse Term Term Term         |
    TermLet Term Term                 |
    TermLambda Type Term              |
    TermControl [Term] [ControlState] |
    TermNew  (Int, Int)               |
    TermMeasure (Int, Int)            |
    TermUnit
  deriving (Eq, Ord, Show, Read)

data Function = Func String Type Term
type Program = [Function]

mapType :: GenAbSyntax.Type -> Type
mapType GenAbSyntax.TypeBit   = TypeBit
mapType GenAbSyntax.TypeQbit  = TypeQbit
mapType GenAbSyntax.TypeUnit  = TypeUnit
mapType (GenAbSyntax.TypeNonLin t) = TypeNonLin (mapType t)
mapType (GenAbSyntax.TypeTensr l r) = mapType l :*: mapType r
mapType (GenAbSyntax.TypeExp t i) = mapType t :**: i
mapType (GenAbSyntax.TypeFunc l r) = mapType l :->: mapType r

reverseMapType :: Type -> GenAbSyntax.Type
reverseMapType TypeBit  = GenAbSyntax.TypeBit
reverseMapType TypeQbit = GenAbSyntax.TypeQbit
reverseMapType TypeUnit = GenAbSyntax.TypeUnit
reverseMapType (TypeNonLin t) = GenAbSyntax.TypeNonLin (reverseMapType t)
reverseMapType (l :*: r) = GenAbSyntax.TypeTensr (reverseMapType l) (reverseMapType r)
reverseMapType (t :**: i) = GenAbSyntax.TypeExp (reverseMapType t) i
reverseMapType (l :->: r) = GenAbSyntax.TypeFunc (reverseMapType l) (reverseMapType r)

mapControlState :: GenAbSyntax.ControlState -> ControlState
mapControlState GenAbSyntax.CtrlStateZero = CtrlStateZero
mapControlState GenAbSyntax.CtrlStateOne = CtrlStateOne
mapControlState GenAbSyntax.CtrlStatePlus = CtrlStatePlus
mapControlState GenAbSyntax.CtrlStateMinus = CtrlStateMinus
mapControlState GenAbSyntax.CtrlStatePlusI = CtrlStatePlusI
mapControlState GenAbSyntax.CtrlStateMinusI = CtrlStateMinusI

reverseMapControlState :: ControlState -> GenAbSyntax.ControlState
reverseMapControlState CtrlStateZero = GenAbSyntax.CtrlStateZero
reverseMapControlState CtrlStateOne = GenAbSyntax.CtrlStateOne
reverseMapControlState CtrlStatePlus = GenAbSyntax.CtrlStatePlus
reverseMapControlState CtrlStateMinus = GenAbSyntax.CtrlStateMinus
reverseMapControlState CtrlStatePlusI = GenAbSyntax.CtrlStatePlusI
reverseMapControlState CtrlStateMinusI = GenAbSyntax.CtrlStateMinusI

mapAngle :: GenAbSyntax.Angle -> Angle
mapAngle (GenAbSyntax.AAngl angle) = Angle angle

reverseMapAngle :: Angle -> GenAbSyntax.Angle
reverseMapAngle (Angle angle) = GenAbSyntax.AAngl angle

mapBit :: GenAbSyntax.Bit -> Bit
mapBit (GenAbSyntax.Bit ((l, c), "0")) = Bit ((l, c), BitZero) 
mapBit (GenAbSyntax.Bit ((l, c), "1")) = Bit ((l, c), BitOne) 
mapBit (GenAbSyntax.Bit ((l, c), s)) = errorWithoutStackTrace $ "Unsupported bit value " ++ s ++ " at line: " ++ show l ++ " and column: " ++ show c

reverseMapBit :: Bit -> GenAbSyntax.Bit
reverseMapBit (Bit ((l, c), BitZero)) = GenAbSyntax.Bit ((l, c), "0")
reverseMapBit (Bit ((l, c), BitOne)) = GenAbSyntax.Bit ((l, c), "1")

mapGate :: GenAbSyntax.Gate -> Gate
mapGate g = case g of 
    GenAbSyntax.GateH ->  GateH
    GenAbSyntax.GateX -> GateX
    GenAbSyntax.GateY -> GateY
    GenAbSyntax.GateZ -> GateZ
    GenAbSyntax.GateID -> GateID
    GenAbSyntax.GateXRt rt -> GateXRt rt
    GenAbSyntax.GateXRtDag rt -> GateXRtDag rt
    GenAbSyntax.GateYRt rt -> GateYRt rt
    GenAbSyntax.GateYRtDag rt -> GateYRtDag rt
    GenAbSyntax.GateZRt rt -> GateZRt rt
    GenAbSyntax.GateZRtDag rt -> GateZRtDag rt
    GenAbSyntax.GateS -> GateS
    GenAbSyntax.GateSDag -> GateSDag
    GenAbSyntax.GateT -> GateT
    GenAbSyntax.GateTDag -> GateTDag
    GenAbSyntax.GateSqrtX -> GateSqrtX
    GenAbSyntax.GateSqrtXDag -> GateSqrtXDag
    GenAbSyntax.GateSqrtY -> GateSqrtY
    GenAbSyntax.GateSqrtYDag -> GateSqrtYDag 
    GenAbSyntax.GateRxTheta angle -> GateRxTheta (mapAngle angle)
    GenAbSyntax.GateRyTheta angle -> GateRyTheta (mapAngle angle)
    GenAbSyntax.GateRzTheta angle -> GateRzTheta (mapAngle angle)
    GenAbSyntax.GateU1 angle -> GateU1 (mapAngle angle)
    GenAbSyntax.GateU2 angle1 angle2 -> GateU2 (mapAngle angle1) (mapAngle angle2)
    GenAbSyntax.GateU3 angle1 angle2 angle3 -> GateU3 (mapAngle angle1) (mapAngle angle2) (mapAngle angle3)
    GenAbSyntax.GateSwp -> GateSwp 
    GenAbSyntax.GateSqrtSwp -> GateSqrtSwp
    GenAbSyntax.GateSqrtSwpDag -> GateSqrtSwpDag 
    GenAbSyntax.GateISwp -> GateISwp
    GenAbSyntax.GateFSwp -> GateFSwp 
    GenAbSyntax.GateSwpTheta angle -> GateSwpTheta (mapAngle angle)
    GenAbSyntax.GateSwpRt rt -> GateSwpRt rt 
    GenAbSyntax.GateSwpRtDag rt -> GateSwpRtDag rt 
    GenAbSyntax.GateGeneric (GenAbSyntax.GateIdent ((l,c ), name)) -> GateGeneric (GateIdent ((l,c ), name))

reverseMapGate :: Gate -> GenAbSyntax.Gate
reverseMapGate g = case g of 
    GateH ->  GenAbSyntax.GateH
    GateX -> GenAbSyntax.GateX
    GateY -> GenAbSyntax.GateY
    GateZ -> GenAbSyntax.GateZ
    GateID -> GenAbSyntax.GateID
    GateXRt rt -> GenAbSyntax.GateXRt rt
    GateXRtDag rt -> GenAbSyntax.GateXRtDag rt
    GateYRt rt -> GenAbSyntax.GateYRt rt
    GateYRtDag rt -> GenAbSyntax.GateYRtDag rt
    GateZRt rt -> GenAbSyntax.GateZRt rt
    GateZRtDag rt -> GenAbSyntax.GateZRtDag rt
    GateS -> GenAbSyntax.GateS
    GateSDag -> GenAbSyntax.GateSDag
    GateT -> GenAbSyntax.GateT
    GateTDag -> GenAbSyntax.GateTDag
    GateSqrtX -> GenAbSyntax.GateSqrtX
    GateSqrtXDag -> GenAbSyntax.GateSqrtXDag
    GateSqrtY -> GenAbSyntax.GateSqrtY
    GateSqrtYDag -> GenAbSyntax.GateSqrtYDag
    GateRxTheta angle -> GenAbSyntax.GateRxTheta (reverseMapAngle angle)
    GateRyTheta angle -> GenAbSyntax.GateRyTheta (reverseMapAngle angle)
    GateRzTheta angle -> GenAbSyntax.GateRzTheta (reverseMapAngle angle)
    GateU1 angle -> GenAbSyntax.GateU1 (reverseMapAngle angle)
    GateU2 angle1 angle2 -> GenAbSyntax.GateU2 (reverseMapAngle angle1) (reverseMapAngle angle2)
    GateU3 angle1 angle2 angle3 -> GenAbSyntax.GateU3 (reverseMapAngle angle1) (reverseMapAngle angle2) (reverseMapAngle angle3)
    GateSwp -> GenAbSyntax.GateSwp
    GateSqrtSwp -> GenAbSyntax.GateSqrtSwp
    GateSqrtSwpDag -> GenAbSyntax.GateSqrtSwpDag
    GateISwp -> GenAbSyntax.GateISwp
    GateFSwp -> GenAbSyntax.GateFSwp
    GateSwpTheta angle -> GenAbSyntax.GateSwpTheta (reverseMapAngle angle)
    GateSwpRt rt -> GenAbSyntax.GateSwpRt rt
    GateSwpRtDag rt -> GenAbSyntax.GateSwpRtDag rt
    GateGeneric (GateIdent ((l,c ), name)) -> GenAbSyntax.GateGeneric (GenAbSyntax.GateIdent ((l,c ), name))

mapFunction :: GenAbSyntax.FunctionDeclaration -> Function
mapFunction (GenAbSyntax.FunDecl funType funDef) = Func fname (mapType ftype) term
   where
      (GenAbSyntax.FunType _ ftype) = funType
      (GenAbSyntax.FunDef (GenAbSyntax.Var fvar) fargs fbody) = funDef
      ((fline, fcol), fname) = fvar
      term = mapTerm Map.empty $ toLambda (trimNonLinType ftype) fargs fbody

-- convert function to a lambda abstraction 
toLambda :: GenAbSyntax.Type -> [GenAbSyntax.Arg] ->  GenAbSyntax.Term -> GenAbSyntax.Term
toLambda ftype [] fbody = fbody
toLambda (GenAbSyntax.TypeFunc ltype rtype) (GenAbSyntax.FunArg (GenAbSyntax.Var var) : vars ) fbody =
   GenAbSyntax.TermLambda (GenAbSyntax.Lambda "\\") (GenAbSyntax.FunType (GenAbSyntax.Var var) ltype) (toLambda rtype vars fbody)
toLambda (GenAbSyntax.TypeNonLin (GenAbSyntax.TypeFunc ltype rtype)) (GenAbSyntax.FunArg (GenAbSyntax.Var var) : vars ) fbody =
   GenAbSyntax.TermLambda (GenAbSyntax.Lambda "\\") (GenAbSyntax.FunType (GenAbSyntax.Var var) ltype) (toLambda rtype vars fbody)

-- the outer non-linear type flag(s) '!' will be removed if present
trimNonLinType :: GenAbSyntax.Type -> GenAbSyntax.Type
trimNonLinType (GenAbSyntax.TypeNonLin t) = trimNonLinType t
trimNonLinType t = t

reverseMapFunction :: Function -> GenAbSyntax.FunctionDeclaration
reverseMapFunction (Func name typ term) = undefined

type Env = Map.Map String Integer

mapTerm :: Env -> GenAbSyntax.Term -> Term
mapTerm env (GenAbSyntax.TermApp l r) = TermApp (mapTerm env l) (mapTerm env r) 
mapTerm _ (GenAbSyntax.TermVar (GenAbSyntax.Var ((l, c), "new"))) = TermNew (l, c)
mapTerm _ (GenAbSyntax.TermVar (GenAbSyntax.Var ((l, c), "measr"))) = TermMeasure (l, c)
mapTerm env (GenAbSyntax.TermDollar l r) = TermDollar (mapTerm env l) (mapTerm env r)
mapTerm env (GenAbSyntax.TermIfElse cond t f) = TermIfElse (mapTerm env cond) (mapTerm env t) (mapTerm env f)
mapTerm _ (GenAbSyntax.TermBit b) = TermBit (mapBit b)

reverseMapTerm :: Env -> Term -> GenAbSyntax.Term
reverseMapTerm env (TermApp l r) = GenAbSyntax.TermApp (reverseMapTerm env l) (reverseMapTerm env r)
reverseMapTerm _ (TermNew (l, c)) = GenAbSyntax.TermVar (GenAbSyntax.Var ((l, c), "new")) 
reverseMapTerm _ (TermMeasure (l, c)) = GenAbSyntax.TermVar (GenAbSyntax.Var ((l, c), "measr"))
reverseMapTerm env (TermDollar l r) = GenAbSyntax.TermDollar (reverseMapTerm env l) (reverseMapTerm env r)
reverseMapTerm env (TermIfElse cond t f) = GenAbSyntax.TermIfElse (reverseMapTerm env cond) (reverseMapTerm env t) (reverseMapTerm env f)





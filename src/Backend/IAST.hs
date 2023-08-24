-- This file contain the specification of the for the intermediate abstract syntax tree.
-- The intermediate abstract syntax tree hase a simpler syntax than the abstract syntax
-- generated by the parser, to make it easier to process by the semantic analyser, type
-- checker and the code generator:
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms 
--   *  introduce De Bruijn indices for bound variables

module Backend.IAST where

import qualified Frontend.LambdaQ.Abs as GeneratedAbSyntax
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
    TermFunction String                      |
    TermBit Bit                              |
    TermGate Gate                            |
    TermCtrlGate [Term] [ControlState] Gate  |
    TermTuple Term Term                      |
    TermApp Term Term                        |
    TermDollar Term Term                     |
    TermCompose Term Term                    |
    TermIfElse Term Term Term                |
    TermLet Term Term                        |
    TermLetSugar Term Term                   |
    TermLambda Type Term                     |
    TermNew  (Int, Int)                      |
    TermMeasure (Int, Int)                   |
    TermUnit
  deriving (Eq, Ord, Show, Read)

data Function = Function String (Int, Int) Type Term
type Program = [Function]

mapType :: GeneratedAbSyntax.Type -> Type
mapType GeneratedAbSyntax.TypeBit   = TypeBit
mapType GeneratedAbSyntax.TypeQbit  = TypeQbit
mapType GeneratedAbSyntax.TypeUnit  = TypeUnit
mapType (GeneratedAbSyntax.TypeNonLin t) = TypeNonLin (mapType t)
mapType (GeneratedAbSyntax.TypeFunc l r) = mapType l :->: mapType r
mapType (GeneratedAbSyntax.TypeTensr l r) = mapType l :*: mapType r
mapType (GeneratedAbSyntax.TypeExp t i) = mapType t :**: i

reverseMapType :: Type -> GeneratedAbSyntax.Type
reverseMapType TypeBit  = GeneratedAbSyntax.TypeBit
reverseMapType TypeQbit = GeneratedAbSyntax.TypeQbit
reverseMapType TypeUnit = GeneratedAbSyntax.TypeUnit
reverseMapType (TypeNonLin t) = GeneratedAbSyntax.TypeNonLin (reverseMapType t)
reverseMapType (l :->: r) = GeneratedAbSyntax.TypeFunc (reverseMapType l) (reverseMapType r)
reverseMapType (l :*: r) = GeneratedAbSyntax.TypeTensr (reverseMapType l) (reverseMapType r)
reverseMapType (t :**: i) = GeneratedAbSyntax.TypeExp (reverseMapType t) i

mapControlState :: GeneratedAbSyntax.ControlState -> ControlState
mapControlState GeneratedAbSyntax.CtrlStateZero = CtrlStateZero
mapControlState GeneratedAbSyntax.CtrlStateOne = CtrlStateOne
mapControlState GeneratedAbSyntax.CtrlStatePlus = CtrlStatePlus
mapControlState GeneratedAbSyntax.CtrlStateMinus = CtrlStateMinus
mapControlState GeneratedAbSyntax.CtrlStatePlusI = CtrlStatePlusI
mapControlState GeneratedAbSyntax.CtrlStateMinusI = CtrlStateMinusI

reverseMapControlState :: ControlState -> GeneratedAbSyntax.ControlState
reverseMapControlState CtrlStateZero = GeneratedAbSyntax.CtrlStateZero
reverseMapControlState CtrlStateOne = GeneratedAbSyntax.CtrlStateOne
reverseMapControlState CtrlStatePlus = GeneratedAbSyntax.CtrlStatePlus
reverseMapControlState CtrlStateMinus = GeneratedAbSyntax.CtrlStateMinus
reverseMapControlState CtrlStatePlusI = GeneratedAbSyntax.CtrlStatePlusI
reverseMapControlState CtrlStateMinusI = GeneratedAbSyntax.CtrlStateMinusI

mapAngle :: GeneratedAbSyntax.Angle -> Angle
mapAngle (GeneratedAbSyntax.AAngl angle) = Angle angle

reverseMapAngle :: Angle -> GeneratedAbSyntax.Angle
reverseMapAngle (Angle angle) = GeneratedAbSyntax.AAngl angle

mapBit :: GeneratedAbSyntax.Bit -> Bit
mapBit (GeneratedAbSyntax.Bit ((l, c), "0")) = Bit ((l, c), BitZero) 
mapBit (GeneratedAbSyntax.Bit ((l, c), "1")) = Bit ((l, c), BitOne) 
mapBit (GeneratedAbSyntax.Bit ((l, c), s)) = errorWithoutStackTrace $ "Unsupported bit value " ++ s ++ " at line: " ++ show l ++ " and column: " ++ show c

reverseMapBit :: Bit -> GeneratedAbSyntax.Bit
reverseMapBit (Bit ((l, c), BitZero)) = GeneratedAbSyntax.Bit ((l, c), "0")
reverseMapBit (Bit ((l, c), BitOne)) = GeneratedAbSyntax.Bit ((l, c), "1")

mapGate :: GeneratedAbSyntax.Gate -> Gate
mapGate g = case g of 
    GeneratedAbSyntax.GateH ->  GateH
    GeneratedAbSyntax.GateX -> GateX
    GeneratedAbSyntax.GateY -> GateY
    GeneratedAbSyntax.GateZ -> GateZ
    GeneratedAbSyntax.GateID -> GateID
    GeneratedAbSyntax.GateXRt rt -> GateXRt rt
    GeneratedAbSyntax.GateXRtDag rt -> GateXRtDag rt
    GeneratedAbSyntax.GateYRt rt -> GateYRt rt
    GeneratedAbSyntax.GateYRtDag rt -> GateYRtDag rt
    GeneratedAbSyntax.GateZRt rt -> GateZRt rt
    GeneratedAbSyntax.GateZRtDag rt -> GateZRtDag rt
    GeneratedAbSyntax.GateS -> GateS
    GeneratedAbSyntax.GateSDag -> GateSDag
    GeneratedAbSyntax.GateT -> GateT
    GeneratedAbSyntax.GateTDag -> GateTDag
    GeneratedAbSyntax.GateSqrtX -> GateSqrtX
    GeneratedAbSyntax.GateSqrtXDag -> GateSqrtXDag
    GeneratedAbSyntax.GateSqrtY -> GateSqrtY
    GeneratedAbSyntax.GateSqrtYDag -> GateSqrtYDag 
    GeneratedAbSyntax.GateRxTheta angle -> GateRxTheta (mapAngle angle)
    GeneratedAbSyntax.GateRyTheta angle -> GateRyTheta (mapAngle angle)
    GeneratedAbSyntax.GateRzTheta angle -> GateRzTheta (mapAngle angle)
    GeneratedAbSyntax.GateU1 angle -> GateU1 (mapAngle angle)
    GeneratedAbSyntax.GateU2 angle1 angle2 -> GateU2 (mapAngle angle1) (mapAngle angle2)
    GeneratedAbSyntax.GateU3 angle1 angle2 angle3 -> GateU3 (mapAngle angle1) (mapAngle angle2) (mapAngle angle3)
    GeneratedAbSyntax.GateSwp -> GateSwp 
    GeneratedAbSyntax.GateSqrtSwp -> GateSqrtSwp
    GeneratedAbSyntax.GateSqrtSwpDag -> GateSqrtSwpDag 
    GeneratedAbSyntax.GateISwp -> GateISwp
    GeneratedAbSyntax.GateFSwp -> GateFSwp 
    GeneratedAbSyntax.GateSwpTheta angle -> GateSwpTheta (mapAngle angle)
    GeneratedAbSyntax.GateSwpRt rt -> GateSwpRt rt 
    GeneratedAbSyntax.GateSwpRtDag rt -> GateSwpRtDag rt 
    GeneratedAbSyntax.GateGeneric (GeneratedAbSyntax.GateIdent ((l,c ), name)) -> GateGeneric (GateIdent ((l,c ), name))

reverseMapGate :: Gate -> GeneratedAbSyntax.Gate
reverseMapGate g = case g of 
    GateH ->  GeneratedAbSyntax.GateH
    GateX -> GeneratedAbSyntax.GateX
    GateY -> GeneratedAbSyntax.GateY
    GateZ -> GeneratedAbSyntax.GateZ
    GateID -> GeneratedAbSyntax.GateID
    GateXRt rt -> GeneratedAbSyntax.GateXRt rt
    GateXRtDag rt -> GeneratedAbSyntax.GateXRtDag rt
    GateYRt rt -> GeneratedAbSyntax.GateYRt rt
    GateYRtDag rt -> GeneratedAbSyntax.GateYRtDag rt
    GateZRt rt -> GeneratedAbSyntax.GateZRt rt
    GateZRtDag rt -> GeneratedAbSyntax.GateZRtDag rt
    GateS -> GeneratedAbSyntax.GateS
    GateSDag -> GeneratedAbSyntax.GateSDag
    GateT -> GeneratedAbSyntax.GateT
    GateTDag -> GeneratedAbSyntax.GateTDag
    GateSqrtX -> GeneratedAbSyntax.GateSqrtX
    GateSqrtXDag -> GeneratedAbSyntax.GateSqrtXDag
    GateSqrtY -> GeneratedAbSyntax.GateSqrtY
    GateSqrtYDag -> GeneratedAbSyntax.GateSqrtYDag
    GateRxTheta angle -> GeneratedAbSyntax.GateRxTheta (reverseMapAngle angle)
    GateRyTheta angle -> GeneratedAbSyntax.GateRyTheta (reverseMapAngle angle)
    GateRzTheta angle -> GeneratedAbSyntax.GateRzTheta (reverseMapAngle angle)
    GateU1 angle -> GeneratedAbSyntax.GateU1 (reverseMapAngle angle)
    GateU2 angle1 angle2 -> GeneratedAbSyntax.GateU2 (reverseMapAngle angle1) (reverseMapAngle angle2)
    GateU3 angle1 angle2 angle3 -> GeneratedAbSyntax.GateU3 (reverseMapAngle angle1) (reverseMapAngle angle2) (reverseMapAngle angle3)
    GateSwp -> GeneratedAbSyntax.GateSwp
    GateSqrtSwp -> GeneratedAbSyntax.GateSqrtSwp
    GateSqrtSwpDag -> GeneratedAbSyntax.GateSqrtSwpDag
    GateISwp -> GeneratedAbSyntax.GateISwp
    GateFSwp -> GeneratedAbSyntax.GateFSwp
    GateSwpTheta angle -> GeneratedAbSyntax.GateSwpTheta (reverseMapAngle angle)
    GateSwpRt rt -> GeneratedAbSyntax.GateSwpRt rt
    GateSwpRtDag rt -> GeneratedAbSyntax.GateSwpRtDag rt
    GateGeneric (GateIdent ((l,c ), name)) -> GeneratedAbSyntax.GateGeneric (GeneratedAbSyntax.GateIdent ((l,c ), name))

mapFunction :: GeneratedAbSyntax.FunctionDeclaration -> Function
mapFunction (GeneratedAbSyntax.FunDecl funType funDef) = Function fname (fline, fcol) (mapType ftype) term
   where
     (GeneratedAbSyntax.FunType _ ftype) = funType
     (GeneratedAbSyntax.FunDef (GeneratedAbSyntax.Var fvar) fargs fbody) = funDef
     ((fline, fcol), fname) = fvar
     term = mapTerm Map.empty $ toLambda (trimNonLinType ftype) fargs fbody

-- convert function to a lambda abstraction 
toLambda :: GeneratedAbSyntax.Type -> [GeneratedAbSyntax.Arg] ->  GeneratedAbSyntax.Term -> GeneratedAbSyntax.Term
toLambda ftype [] fbody = fbody
toLambda (GeneratedAbSyntax.TypeFunc ltype rtype) (GeneratedAbSyntax.FunArg (GeneratedAbSyntax.Var var) : vars ) fbody =
   GeneratedAbSyntax.TermLambda (GeneratedAbSyntax.Lambda "\\") (GeneratedAbSyntax.FunType (GeneratedAbSyntax.Var var) ltype) (toLambda rtype vars fbody)
toLambda (GeneratedAbSyntax.TypeNonLin (GeneratedAbSyntax.TypeFunc ltype rtype)) (GeneratedAbSyntax.FunArg (GeneratedAbSyntax.Var var) : vars ) fbody =
   GeneratedAbSyntax.TermLambda (GeneratedAbSyntax.Lambda "\\") (GeneratedAbSyntax.FunType (GeneratedAbSyntax.Var var) ltype) (toLambda rtype vars fbody)

-- the outer non-linear type flag(s) '!' will be removed if present
trimNonLinType :: GeneratedAbSyntax.Type -> GeneratedAbSyntax.Type
trimNonLinType (GeneratedAbSyntax.TypeNonLin t) = trimNonLinType t
trimNonLinType t = t

reverseMapFunction :: Function -> GeneratedAbSyntax.FunctionDeclaration
reverseMapFunction (Function fname (fline, fcol) ftype term) = GeneratedAbSyntax.FunDecl funType funDefinition
  where
    funType = GeneratedAbSyntax.FunType (GeneratedAbSyntax.Var ((fline, fcol), fname)) (reverseMapType ftype)
    funDefinition = GeneratedAbSyntax.FunDef (GeneratedAbSyntax.Var ((fline, fcol), fname)) [] (reverseMapTerm Map.empty term)

type Env = Map.Map String Integer

mapTerm :: Env -> GeneratedAbSyntax.Term -> Term
mapTerm env (GeneratedAbSyntax.TermApp l r) = TermApp (mapTerm env l) (mapTerm env r) 
mapTerm _ (GeneratedAbSyntax.TermVar (GeneratedAbSyntax.Var ((l, c), "new"))) = TermNew (l, c)
mapTerm _ (GeneratedAbSyntax.TermVar (GeneratedAbSyntax.Var ((l, c), "measr"))) = TermMeasure (l, c)
mapTerm env (GeneratedAbSyntax.TermDollar l r) = TermDollar (mapTerm env l) (mapTerm env r)
mapTerm env (GeneratedAbSyntax.TermIfElse cond t f) = TermIfElse (mapTerm env cond) (mapTerm env t) (mapTerm env f)
mapTerm _ (GeneratedAbSyntax.TermBit b) = TermBit (mapBit b)

reverseMapTerm :: Env -> Term -> GeneratedAbSyntax.Term
reverseMapTerm env (TermApp l r) = GeneratedAbSyntax.TermApp (reverseMapTerm env l) (reverseMapTerm env r)
reverseMapTerm _ (TermNew (l, c)) = GeneratedAbSyntax.TermVar (GeneratedAbSyntax.Var ((l, c), "new")) 
reverseMapTerm _ (TermMeasure (l, c)) = GeneratedAbSyntax.TermVar (GeneratedAbSyntax.Var ((l, c), "measr"))
reverseMapTerm env (TermDollar l r) = GeneratedAbSyntax.TermDollar (reverseMapTerm env l) (reverseMapTerm env r)
reverseMapTerm env (TermIfElse cond t f) = GeneratedAbSyntax.TermIfElse (reverseMapTerm env cond) (reverseMapTerm env t) (reverseMapTerm env f)





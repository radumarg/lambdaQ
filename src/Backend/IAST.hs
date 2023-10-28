-- This file contain the specification of the for the intermediate abstract syntax tree (IAST).
-- The intermediate abstract syntax tree hase a simpler syntax than the abstract syntax (AST)
-- generated by the parser and is easier to process by the type checker and the code generator:
--   *  functions to be be converted to lambda abstractions 
--   *  introduce De Bruijn indices for bound variables in lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms

module Backend.IAST where

import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import Frontend.LambdaQ.Print ( printTree )
import qualified Data.Map as Map


data Type = 
   TypeBit             |
   TypeQbit            |
   TypeState           |
   TypeUnitary         |
   TypeNonLinear Type  |
   TypeUnit            |
   Type :->: Type      |
   Type :*: Type       |
   Type :+: Type       |
   Type :**: Integer 
  deriving (Eq, Ord, Read)

instance Show Type where
    show = printTree . reverseMapType

infixr 1 :->:
infixr 2 :+:
infixr 3 :*:
infixr 4 :**:

data BasisState =
    BasisStateZero   |
    BasisStateOne    |
    BasisStatePlus   |
    BasisStateMinus  |
    BasisStatePlusI  |
    BasisStateMinusI
  deriving (Eq, Ord, Show, Read)

data ControlBasisState = CtrlBasisState BasisState
  deriving (Eq, Ord, Show, Read)

data ControlBasisStates = CtrlBasisStates BasisState [BasisState]
  deriving (Eq, Ord, Show, Read)

newtype Angle = Angle Double
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
    GateFSwp                   |
    GateSwpTheta Angle         |
    GateSwpRt Integer          |
    GateSwpRtDag Integer
  deriving (Eq, Ord, Read)

instance Show Gate where
    show = printTree . reverseMapGate

data Term =
    TermIfElse Term Term Term                     |
    TermLetSingle Term Term                       |
    TermLetMultiple Term Term                     |
    TermLetSugarSingle Term Term                  |
    TermLetSugarMultiple Term Term                |
    TermCase Term [Term]                          |
    TermLambda Type Term                          |
    TermFunction String                           |
    TermGate Gate                                 |
    TermQuantumCtrlGate Term BasisState           |
    TermQuantumCtrlsGate [Term] [BasisState]      |
    TermClassicCtrlGate Term Integer              |
    TermClassicCtrlsGate [Term] [Integer]         |
    TermApply Term Term                           |
    TermDollar Term Term                          |
    TermCompose Term Term                         |
    TermNew  (Int, Int)                           |
    TermMeasure (Int, Int)                        |
    TermTuple Term Term                           |
    TermUnit
  deriving (Eq, Ord, Read)

instance Show Term where
    show = printTree . reverseMapTerm Map.empty

data ControlTerm = CtrlTerm Term
  deriving (Eq, Ord, Show, Read)

data ControlTerms = CtrlTerms Term [Term]
  deriving (Eq, Ord, Show, Read)

data Function = Function String (Int, Int) Type Term

instance Ord Function where
  compare (Function a _ _ _) (Function b _ _ _) = compare a b

instance Eq Function where
  Function a _ _ _ == Function b _ _ _ = a == b

type Program = [Function]

mapType :: GeneratedAbstractSyntax.Type -> Type
mapType GeneratedAbstractSyntax.TypeBit   = TypeBit
mapType GeneratedAbstractSyntax.TypeQbit  = TypeQbit
mapType GeneratedAbstractSyntax.TypeState  = TypeState
mapType GeneratedAbstractSyntax.TypeUnitary  = TypeUnitary
mapType GeneratedAbstractSyntax.TypeUnit  = TypeUnit
mapType (GeneratedAbstractSyntax.TypeNonLinear t) = TypeNonLinear (mapType t)
mapType (GeneratedAbstractSyntax.TypeFunction l r) = mapType l :->: mapType r
mapType (GeneratedAbstractSyntax.TypeSum l r) = mapType l :+: mapType r
mapType (GeneratedAbstractSyntax.TypeTensorProd l r) = mapType l :*: mapType r
mapType (GeneratedAbstractSyntax.TypeExp t i) = mapType t :**: i

reverseMapType :: Type -> GeneratedAbstractSyntax.Type
reverseMapType TypeBit  = GeneratedAbstractSyntax.TypeBit
reverseMapType TypeQbit = GeneratedAbstractSyntax.TypeQbit
reverseMapType TypeState  = GeneratedAbstractSyntax.TypeState
reverseMapType TypeUnitary  = GeneratedAbstractSyntax.TypeUnitary
reverseMapType TypeUnit = GeneratedAbstractSyntax.TypeUnit
reverseMapType (TypeNonLinear t) = GeneratedAbstractSyntax.TypeNonLinear (reverseMapType t)
reverseMapType (l :->: r) = GeneratedAbstractSyntax.TypeFunction (reverseMapType l) (reverseMapType r)
reverseMapType (l :+: r) = GeneratedAbstractSyntax.TypeSum (reverseMapType l) (reverseMapType r)
reverseMapType (l :*: r) = GeneratedAbstractSyntax.TypeTensorProd (reverseMapType l) (reverseMapType r)
reverseMapType (t :**: i) = GeneratedAbstractSyntax.TypeExp (reverseMapType t) i

mapBasisState :: GeneratedAbstractSyntax.BasisState -> BasisState
mapBasisState GeneratedAbstractSyntax.BasisStateZero = BasisStateZero
mapBasisState GeneratedAbstractSyntax.BasisStateOne = BasisStateOne
mapBasisState GeneratedAbstractSyntax.BasisStatePlus = BasisStatePlus
mapBasisState GeneratedAbstractSyntax.BasisStateMinus = BasisStateMinus
mapBasisState GeneratedAbstractSyntax.BasisStatePlusI = BasisStatePlusI
mapBasisState GeneratedAbstractSyntax.BasisStateMinusI = BasisStateMinusI

reverseMapBasisState :: BasisState -> GeneratedAbstractSyntax.BasisState
reverseMapBasisState BasisStateZero = GeneratedAbstractSyntax.BasisStateZero
reverseMapBasisState BasisStateOne = GeneratedAbstractSyntax.BasisStateOne
reverseMapBasisState BasisStatePlus = GeneratedAbstractSyntax.BasisStatePlus
reverseMapBasisState BasisStateMinus = GeneratedAbstractSyntax.BasisStateMinus
reverseMapBasisState BasisStatePlusI = GeneratedAbstractSyntax.BasisStatePlusI
reverseMapBasisState BasisStateMinusI = GeneratedAbstractSyntax.BasisStateMinusI

mapAngle :: GeneratedAbstractSyntax.Angle -> Angle
mapAngle (GeneratedAbstractSyntax.Angle value) = Angle value

reverseMapAngle :: Angle -> GeneratedAbstractSyntax.Angle
reverseMapAngle (Angle value) = GeneratedAbstractSyntax.Angle value

-- mapGate :: GeneratedAbstractSyntax.Gate -> Gate
mapGate g = case g of 
    GeneratedAbstractSyntax.GateH ->  GateH
    GeneratedAbstractSyntax.GateX -> GateX
    GeneratedAbstractSyntax.GateY -> GateY
    GeneratedAbstractSyntax.GateZ -> GateZ
    GeneratedAbstractSyntax.GateID -> GateID
    GeneratedAbstractSyntax.GateXRt rt -> GateXRt rt
    GeneratedAbstractSyntax.GateXRtDag rt -> GateXRtDag rt
    GeneratedAbstractSyntax.GateYRt rt -> GateYRt rt
    GeneratedAbstractSyntax.GateYRtDag rt -> GateYRtDag rt
    GeneratedAbstractSyntax.GateZRt rt -> GateZRt rt
    GeneratedAbstractSyntax.GateZRtDag rt -> GateZRtDag rt
    GeneratedAbstractSyntax.GateS -> GateS
    GeneratedAbstractSyntax.GateSDag -> GateSDag
    GeneratedAbstractSyntax.GateT -> GateT
    GeneratedAbstractSyntax.GateTDag -> GateTDag
    GeneratedAbstractSyntax.GateSqrtX -> GateSqrtX
    GeneratedAbstractSyntax.GateSqrtXDag -> GateSqrtXDag
    GeneratedAbstractSyntax.GateSqrtY -> GateSqrtY
    GeneratedAbstractSyntax.GateSqrtYDag -> GateSqrtYDag 
    GeneratedAbstractSyntax.GateRxTheta angle -> GateRxTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateRyTheta angle -> GateRyTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateRzTheta angle -> GateRzTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateU1 angle -> GateU1 (mapAngle angle)
    GeneratedAbstractSyntax.GateU2 angle1 angle2 -> GateU2 (mapAngle angle1) (mapAngle angle2)
    GeneratedAbstractSyntax.GateU3 angle1 angle2 angle3 -> GateU3 (mapAngle angle1) (mapAngle angle2) (mapAngle angle3)
    GeneratedAbstractSyntax.GateSwp -> GateSwp 
    GeneratedAbstractSyntax.GateSqrtSwp -> GateSqrtSwp
    GeneratedAbstractSyntax.GateSqrtSwpDag -> GateSqrtSwpDag 
    GeneratedAbstractSyntax.GateISwp -> GateISwp
    GeneratedAbstractSyntax.GateFSwp -> GateFSwp 
    GeneratedAbstractSyntax.GateSwpTheta angle -> GateSwpTheta (mapAngle angle)
    GeneratedAbstractSyntax.GateSwpRt rt -> GateSwpRt rt 
    GeneratedAbstractSyntax.GateSwpRtDag rt -> GateSwpRtDag rt 

reverseMapGate :: Gate -> GeneratedAbstractSyntax.Gate
reverseMapGate g = case g of 
    GateH ->  GeneratedAbstractSyntax.GateH
    GateX -> GeneratedAbstractSyntax.GateX
    GateY -> GeneratedAbstractSyntax.GateY
    GateZ -> GeneratedAbstractSyntax.GateZ
    GateID -> GeneratedAbstractSyntax.GateID
    GateXRt rt -> GeneratedAbstractSyntax.GateXRt rt
    GateXRtDag rt -> GeneratedAbstractSyntax.GateXRtDag rt
    GateYRt rt -> GeneratedAbstractSyntax.GateYRt rt
    GateYRtDag rt -> GeneratedAbstractSyntax.GateYRtDag rt
    GateZRt rt -> GeneratedAbstractSyntax.GateZRt rt
    GateZRtDag rt -> GeneratedAbstractSyntax.GateZRtDag rt
    GateS -> GeneratedAbstractSyntax.GateS
    GateSDag -> GeneratedAbstractSyntax.GateSDag
    GateT -> GeneratedAbstractSyntax.GateT
    GateTDag -> GeneratedAbstractSyntax.GateTDag
    GateSqrtX -> GeneratedAbstractSyntax.GateSqrtX
    GateSqrtXDag -> GeneratedAbstractSyntax.GateSqrtXDag
    GateSqrtY -> GeneratedAbstractSyntax.GateSqrtY
    GateSqrtYDag -> GeneratedAbstractSyntax.GateSqrtYDag
    GateRxTheta angle -> GeneratedAbstractSyntax.GateRxTheta (reverseMapAngle angle)
    GateRyTheta angle -> GeneratedAbstractSyntax.GateRyTheta (reverseMapAngle angle)
    GateRzTheta angle -> GeneratedAbstractSyntax.GateRzTheta (reverseMapAngle angle)
    GateU1 angle -> GeneratedAbstractSyntax.GateU1 (reverseMapAngle angle)
    GateU2 angle1 angle2 -> GeneratedAbstractSyntax.GateU2 (reverseMapAngle angle1) (reverseMapAngle angle2)
    GateU3 angle1 angle2 angle3 -> GeneratedAbstractSyntax.GateU3 (reverseMapAngle angle1) (reverseMapAngle angle2) (reverseMapAngle angle3)
    GateSwp -> GeneratedAbstractSyntax.GateSwp
    GateSqrtSwp -> GeneratedAbstractSyntax.GateSqrtSwp
    GateSqrtSwpDag -> GeneratedAbstractSyntax.GateSqrtSwpDag
    GateISwp -> GeneratedAbstractSyntax.GateISwp
    GateFSwp -> GeneratedAbstractSyntax.GateFSwp
    GateSwpTheta angle -> GeneratedAbstractSyntax.GateSwpTheta (reverseMapAngle angle)
    GateSwpRt rt -> GeneratedAbstractSyntax.GateSwpRt rt
    GateSwpRtDag rt -> GeneratedAbstractSyntax.GateSwpRtDag rt

-- the outer non-linear type flag(s) will be removed if present
trimOuterNonLinearTypeModifier :: GeneratedAbstractSyntax.Type -> GeneratedAbstractSyntax.Type
trimOuterNonLinearTypeModifier (GeneratedAbstractSyntax.TypeNonLinear t) = trimOuterNonLinearTypeModifier t
trimOuterNonLinearTypeModifier t = t

-- convert function to Church-style lambda abstractions
toLambdaAbstraction :: GeneratedAbstractSyntax.Type -> [GeneratedAbstractSyntax.Arg] ->  GeneratedAbstractSyntax.Term -> GeneratedAbstractSyntax.Term
toLambdaAbstraction ftype [] fbody = fbody
toLambdaAbstraction
  (GeneratedAbstractSyntax.TypeFunction ltype rtype)
  (GeneratedAbstractSyntax.FunArg (GeneratedAbstractSyntax.Var var) : vars)
  fbody =
    GeneratedAbstractSyntax.TermLambda
      (GeneratedAbstractSyntax.Lambda "\\")
      (GeneratedAbstractSyntax.Var var)
      (GeneratedAbstractSyntax.FunType (GeneratedAbstractSyntax.Var var) ltype)
      (toLambdaAbstraction rtype vars fbody)
toLambdaAbstraction (GeneratedAbstractSyntax.TypeNonLinear ftype) farg fbody = toLambdaAbstraction ftype farg fbody
toLambdaAbstraction _ _ _ = undefined

mapFunction :: GeneratedAbstractSyntax.FunctionDeclaration -> Function
mapFunction (GeneratedAbstractSyntax.FunDecl funType funDef) = Function fname (fline, fcol) (mapType ftype) term
   where
     (GeneratedAbstractSyntax.FunType var ftype) = funType
     (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
     ((fline, fcol), fname) = fvar
     term = mapTerm Map.empty $ toLambdaAbstraction (trimOuterNonLinearTypeModifier ftype) fargs fbody

reverseMapFunction :: Function -> GeneratedAbstractSyntax.FunctionDeclaration
reverseMapFunction (Function fname (fline, fcol) ftype term) = GeneratedAbstractSyntax.FunDecl funType funDefinition
  where
    funType = GeneratedAbstractSyntax.FunType (GeneratedAbstractSyntax.Var ((fline, fcol), fname)) (reverseMapType ftype)
    funDefinition = GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var ((fline, fcol), fname)) [] (reverseMapTerm Map.empty term)

type Environment = Map.Map String Integer

toTerm :: GeneratedAbstractSyntax.LetVariable -> GeneratedAbstractSyntax.Term
toTerm (GeneratedAbstractSyntax.LetVar var) = GeneratedAbstractSyntax.TermVariable var

toVariableName :: GeneratedAbstractSyntax.Var -> String
toVariableName (GeneratedAbstractSyntax.Var var) = snd var

toLetVariableName :: GeneratedAbstractSyntax.LetVariable -> String
toLetVariableName (GeneratedAbstractSyntax.LetVar var) = toVariableName var

mapTerm :: Environment -> GeneratedAbstractSyntax.Term -> Term
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "new"))) = TermNew (l, c)
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "measr"))) = TermMeasure (l, c)
mapTerm env (GeneratedAbstractSyntax.TermIfElse cond t f) = TermIfElse (mapTerm env cond) (mapTerm env t) (mapTerm env f)
mapTerm env (GeneratedAbstractSyntax.TermLetSingle x letEq letIn) = TermLetSingle (mapTerm env letEq) (mapTerm inEnv letIn)
  where inEnv = Map.insert (toLetVariableName x) 0 (Map.map succ env)
mapTerm env (GeneratedAbstractSyntax.TermLetMultiple x [y] letEq letIn) = TermLetMultiple (mapTerm env letEq) (mapTerm inEnv letIn)
  where inEnv = Map.insert (toLetVariableName y) 1 $ Map.insert (toLetVariableName x) 0 (Map.map (succ . succ) env)
mapTerm env (GeneratedAbstractSyntax.TermLetMultiple x (y:ys) letEq letIn) = 
    TermLetMultiple (mapTerm env letEq) (mapTerm letEnv (GeneratedAbstractSyntax.TermLetMultiple y ys (toTerm y) letIn))
  where letEnv = Map.insert (toLetVariableName x) 1 $ Map.insert (toLetVariableName y) 0 (Map.map (succ . succ) env)
mapTerm env (GeneratedAbstractSyntax.TermLetSugarSingle x letEq letIn) = TermLetSugarSingle (mapTerm env letEq) (mapTerm inEnv letIn)
  where inEnv = Map.insert (toLetVariableName x) 0 (Map.map succ env)
mapTerm env (GeneratedAbstractSyntax.TermLetSugarMultiple x [y] letEq letIn) = TermLetSugarMultiple (mapTerm env letEq) (mapTerm inEnv letIn)
  where inEnv = Map.insert (toLetVariableName y) 1 $ Map.insert (toLetVariableName x) 0 (Map.map (succ . succ) env)
mapTerm env (GeneratedAbstractSyntax.TermLetSugarMultiple x (y:ys) letEq letIn) = 
    TermLetSugarMultiple (mapTerm env letEq) (mapTerm letEnv (GeneratedAbstractSyntax.TermLetSugarMultiple y ys (toTerm y) letIn))
  where letEnv = Map.insert (toLetVariableName x) 1 $ Map.insert (toLetVariableName y) 0 (Map.map (succ . succ) env)
mapTerm env (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm term) (GeneratedAbstractSyntax.CtrlBasisState basisState)) =
   TermQuantumCtrlGate (mapTerm env term) (mapBasisState basisState) 

-- mapTerm env (GeneratedAbstractSyntax.TermQuantumCtrlsGate 
--   (GeneratedAbstractSyntax.CtrlTerms term [terms]) 
--   (GeneratedAbstractSyntax.CtrlBasisStates basisState [basisStates])) =
--       TermQuantumCtrlsGate 
--       (mapTerm env term []) (mapBasisState basisState []) 


--mapTerm env (GeneratedAbstractSyntax.TermLetSingle var letEq letIn) = TermLetSingle (mapTerm env letEq) (mapTerm (env?) letIn)
-- mapTerm env (GeneratedAbstractSyntax.TermApply l r) = TermApply (mapTerm env l) (mapTerm env r) 
-- mapTerm env (GeneratedAbstractSyntax.TermDollar l r) = TermDollar (mapTerm env l) (mapTerm env r)
mapTerm _ _ = undefined


reverseMapTerm :: Environment -> Term -> GeneratedAbstractSyntax.Term
reverseMapTerm _ (TermNew (l, c)) = GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "new")) 
reverseMapTerm _ (TermMeasure (l, c)) = GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "measr"))
reverseMapTerm env (TermIfElse cond t f) = GeneratedAbstractSyntax.TermIfElse (reverseMapTerm env cond) (reverseMapTerm env t) (reverseMapTerm env f)
reverseMapTerm env (TermQuantumCtrlGate term basisState) =
  GeneratedAbstractSyntax.TermQuantumCtrlGate 
  (GeneratedAbstractSyntax.CtrlTerm (reverseMapTerm env term)) 
  (GeneratedAbstractSyntax.CtrlBasisState (reverseMapBasisState basisState))
reverseMapTerm _ _ = undefined


-- TODO:
-- reverseMapTerm TermLetSingle
-- reverseMapTerm TermLetMultiple
-- reverseMapTerm TermLetSugarSingle
-- reverseMapTerm TermLetSugarMultiple
-- reverseMapTerm env (TermLetMultiple letEq letIn) = 
--   GeneratedAbstractSyntax.TermLetMultiple 
--   (GeneratedAbstractSyntax.LetVar . GeneratedAbstractSyntax.Var $ 'x' : show env)
--   [GeneratedAbstractSyntax.LetVar . GeneratedAbstractSyntax.Var $ 'x' : show (env + 1)] 
--   (reverseMapTerm env letEq) 
--   (reverseMapTerm (env + 2) letIn)

mapControlTerm :: Environment -> GeneratedAbstractSyntax.ControlTerm -> ControlTerm
mapControlTerm env (GeneratedAbstractSyntax.CtrlTerm term) = CtrlTerm (mapTerm env term)

reverseMapControlTerm :: Environment -> ControlTerm -> GeneratedAbstractSyntax.ControlTerm
reverseMapControlTerm env (CtrlTerm term) = GeneratedAbstractSyntax.CtrlTerm (reverseMapTerm env term)

mapControlTerms :: Environment -> GeneratedAbstractSyntax.ControlTerms -> ControlTerms
mapControlTerms env (GeneratedAbstractSyntax.CtrlTerms term []) = CtrlTerms (mapTerm env term) []
mapControlTerms env (GeneratedAbstractSyntax.CtrlTerms term [terms]) = CtrlTerms (mapTerm env term) (map mapControl [terms])
  where mapControl = mapTerm env
mapControlTerms _ _ = undefined

reverseMapControlTerms :: Environment -> ControlTerms -> GeneratedAbstractSyntax.ControlTerms
reverseMapControlTerms env (CtrlTerms term []) = GeneratedAbstractSyntax.CtrlTerms (reverseMapTerm env term) []
reverseMapControlTerms env (CtrlTerms term [terms]) = GeneratedAbstractSyntax.CtrlTerms (reverseMapTerm env term) (map reverseMapControl [terms])
  where reverseMapControl = reverseMapTerm env
reverseMapControlTerms _ _ = undefined


-- reverseMapTerm env (TermApply l r) = GeneratedAbstractSyntax.TermApply (reverseMapTerm env l) (reverseMapTerm env r)
-- reverseMapTerm env (TermDollar l r) = GeneratedAbstractSyntax.TermDollar (reverseMapTerm env l) (reverseMapTerm env r)



-- TermLambda           . Term1 ::= Lambda Var FunctionType "." Term ;
-- TermQuantumCtrlGate  . Term2 ::= "with" ControlTerm "ctrl" ControlBasisState ;
-- TermQuantumCtrlsGate . Term2 ::= "with" ControlTerms "ctrl" ControlBasisStates ;
-- TermClassicCtrlGate  . Term2 ::= "with" ControlTerm "ctrl" ControlBit ;
-- TermClassicCtrlsGate . Term2 ::= "with" ControlTerms "ctrl" ControlBits ;
-- TermApply            . Term2 ::= Term2 Term3 ;      -- left-associative  --
-- TermDollar           . Term1 ::= Term2 "$" Term1 ;  -- right-associative --
-- TermCompose          . Term2 ::= Term2 "." Term3 ;  -- left-associative  --
-- TermVariable         . Term3 ::= Var ;
-- TermTuple            . Term3 ::= Tuple ;
-- TermUnit             . Term3 ::= "()" ;

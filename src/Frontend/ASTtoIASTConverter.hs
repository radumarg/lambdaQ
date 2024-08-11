-- This file contain the specification of the for the intermediate abstract syntax tree (IAST).
-- The intermediate abstract syntax tree hase a simpler syntax than the abstract syntax (AST)
-- generated by the parser and is easier to process by the type checker and the code generator:
--   * functions to be be converted to lambda abstractions 
--   * introduce De Bruijn indices for bound variables in lambda abstractions
--   * BNFC generated AST terms to be converted into an intermediate abstract syntax tree terms
--   * syntactic sugar is removed for let sugar terms, let sugar terms are consolidated in a unique term
--   * quantum controlled gates variants are consolidated in a unique term
--   * classsical controlled gates variants are consolidated in a unique term
--   * special functions are replaced with dedicated terms

module Frontend.ASTtoIASTConverter (
  Function(..),
  Gate(..),
  mapProgram,
  Program,
  runAstToIastConverter,
  Term(..),
  Type(..),
) where

import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import qualified Data.Map

data Type =
   TypeBit             |
   TypeQbit            |
   TypeBool            |
   TypeInteger         |
   TypeUnit            |
   TypeList Type       |
   TypeNonLinear Type  |
   Type :->: Type      |
   Type :*: Type       |
   Type :**: Integer
  deriving (Eq, Ord, Read, Show)

infixr 1 :->:
infixr 3 :*:
infixr 4 :**:

data BoolValue = BoolValueTrue | BoolValueFalse
  deriving (Eq, Ord, Show, Read)

data IntegerExpression
    = ArithmExprMinus IntegerExpression
    | ArithmExprAdd IntegerExpression IntegerExpression
    | ArithmExprSub IntegerExpression IntegerExpression
    | ArithmExprMul IntegerExpression IntegerExpression
    | ArithmExprDiv IntegerExpression IntegerExpression
    | ArithmExprInt Integer
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data List
    = ListNil
    | ListSingle Term
    | ListMultiple Term [Term]
    | ListExpressionAdd List List
    | ListCons Term List
  deriving (Eq, Ord, Show, Read)

data BasisState =
    BasisStateZero   |
    BasisStateOne    |
    BasisStatePlus   |
    BasisStateMinus  |
    BasisStatePlusI  |
    BasisStateMinusI
  deriving (Eq, Ord, Show, Read)

newtype ControlBasisState = CtrlBasisState BasisState
  deriving (Eq, Ord, Show, Read)

data ControlBasisStates = CtrlBasisStates BasisState [BasisState]
  deriving (Eq, Ord, Show, Read)

data ControlBit = CtrlBit Integer
  deriving (Eq, Ord, Show, Read)

data ControlBits = CtrlBits Integer [Integer]
  deriving (Eq, Ord, Show, Read)

newtype Angle = Angle Double
  deriving (Eq, Ord, Show, Read)

data Bit = BitZero | BitOne
  deriving (Eq, Ord, Show, Read)

data Gate =
    GateH                      |
    GateX                      |
    GateY                      |
    GateZ                      |
    GateID                     |
    GateXRootInt Integer       |
    GateXRootVar Var           |
    GateXRootDagInt Integer    |
    GateXRootDagVar Var        |
    GateYRootInt Integer       |
    GateYRootVar Var           |
    GateYRootDagInt Integer    |
    GateYRootDagVar Var        |
    GateZRootInt Integer       |
    GateZRootVar Var           |
    GateZRootDagInt Integer    |
    GateZRootDagVar Var        |
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
    GateSwpRtInt Integer       |
    GateSwpRtVar Var           |
    GateSwpRtDagInt Integer    |
    GateSwpRtDagVar Var        |
    GateQftInt Integer         |
    GateQftVar Var             |
    GateQftDagInt Integer      |
    GateQftDagVar Var
  deriving (Eq, Ord, Read, Show)

data Term =
    TermBoundVariable Integer                     |
    TermFreeVariable String                       |
    TermList List                                 |
    TermListElement List Integer                  |
    TermBoolExpression BoolExpression             |
    TermIntegerExpression IntegerExpression       |
    TermVariable Var                              |
    TermIfElse Term Term Term                     |
    TermLet Term Term                             |
    TermCase Term [CaseExpression]                |
    TermLambda Type Term                          |
    TermGate Gate                                 |
    TermQuantumControlGate [Term] [BasisState]    |
    TermClassicControlGate [Term] [Bit]           |
    TermApply Term Term                           |
    TermDollar Term Term                          |
    TermCompose Term Term                         |
    TermNew  (Int, Int)                           |
    TermMeasure (Int, Int)                        |
    TermInverse  (Int, Int)                       |
    TermPower (Int, Int)                          |
    TermReset (Int, Int)                          |
    TermId (Int, Int)                             |
    TermBasisState BasisState                     |
    TermTuple Term [Term]                         |
    TermBit Bit                                   |
    TermTensorProduct Term Term                   |
    TermUnit
  deriving (Eq, Ord, Read, Show)

data CaseExpression = CaseExpr Term Term
  deriving (Eq, Ord, Show, Read)

newtype ControlTerm = CtrlTerm Term
  deriving (Eq, Ord, Show, Read)

data ControlTerms = CtrlTerms Term [Term]
  deriving (Eq, Ord, Show, Read)

data Function = Function String (Int, Int) Type Term
  deriving (Show, Read)

instance Ord Function where
  compare (Function a _ _ _) (Function b _ _ _) = compare a b

instance Eq Function where
  Function a _ _ _ == Function b _ _ _ = a == b

newtype Var = Var ((Int, Int), String)
  deriving (Eq, Ord, Show, Read)

type Program = [Function]

type Environment = Data.Map.Map String Integer

mapProgram :: GeneratedAbstractSyntax.Program -> Program
mapProgram (GeneratedAbstractSyntax.ProgDef functions) = map mapFunction functions

runAstToIastConverter :: GeneratedAbstractSyntax.Program -> Either String Program
runAstToIastConverter program = if substring "error" programString then Left programString else Right mappedProgram
    where
      mappedProgram = mapProgram program
      programString = show mappedProgram

mapType :: GeneratedAbstractSyntax.Type -> Type
mapType GeneratedAbstractSyntax.TypeBool = TypeBool
mapType GeneratedAbstractSyntax.TypeInteger = TypeInteger
mapType GeneratedAbstractSyntax.TypeBit = TypeBit
mapType GeneratedAbstractSyntax.TypeQbit = TypeQbit
mapType GeneratedAbstractSyntax.TypeUnit = TypeUnit
mapType (GeneratedAbstractSyntax.TypeNonLinear t) = TypeNonLinear (mapType t)
mapType (GeneratedAbstractSyntax.TypeFunction l r) = mapType l :->: mapType r
mapType (GeneratedAbstractSyntax.TypeTensorProd l r) = mapType l :*: mapType r
mapType (GeneratedAbstractSyntax.TypeExp t i) = mapType t :**: i
mapType (GeneratedAbstractSyntax.TypeList t) = TypeList (mapType t)

mapVariable :: GeneratedAbstractSyntax.Var -> Var
mapVariable (GeneratedAbstractSyntax.Var ((l, c), var)) = Var ((l, c), var)

toVariableName :: GeneratedAbstractSyntax.Var -> String
toVariableName (GeneratedAbstractSyntax.Var var) = snd var

mapBasisState :: GeneratedAbstractSyntax.BasisState -> BasisState
mapBasisState GeneratedAbstractSyntax.BasisStateZero = BasisStateZero
mapBasisState GeneratedAbstractSyntax.BasisStateOne = BasisStateOne
mapBasisState GeneratedAbstractSyntax.BasisStatePlus = BasisStatePlus
mapBasisState GeneratedAbstractSyntax.BasisStateMinus = BasisStateMinus
mapBasisState GeneratedAbstractSyntax.BasisStatePlusI = BasisStatePlusI
mapBasisState GeneratedAbstractSyntax.BasisStateMinusI = BasisStateMinusI

mapAngle :: GeneratedAbstractSyntax.Angle -> Angle
mapAngle (GeneratedAbstractSyntax.AngleValue value) = Angle value

mapControlBit :: Integer -> Bit
mapControlBit 0 = BitZero
mapControlBit 1 = BitOne
mapControlBit _ = undefined

mapBoolValue :: GeneratedAbstractSyntax.BoolValue -> BoolValue
mapBoolValue GeneratedAbstractSyntax.BoolValueTrue = BoolValueTrue
mapBoolValue GeneratedAbstractSyntax.BoolValueFalse = BoolValueFalse

mapGate :: GeneratedAbstractSyntax.Gate -> Gate
mapGate g = case g of
    GeneratedAbstractSyntax.GateH ->  GateH
    GeneratedAbstractSyntax.GateX -> GateX
    GeneratedAbstractSyntax.GateY -> GateY
    GeneratedAbstractSyntax.GateZ -> GateZ
    GeneratedAbstractSyntax.GateID -> GateID
    GeneratedAbstractSyntax.GateXRootInt rt -> GateXRootInt rt
    GeneratedAbstractSyntax.GateXRootVar rt -> GateXRootVar (mapVariable rt)
    GeneratedAbstractSyntax.GateXRootDagInt rt -> GateXRootDagInt rt
    GeneratedAbstractSyntax.GateXRootDagVar rt -> GateXRootDagVar (mapVariable rt)
    GeneratedAbstractSyntax.GateYRootInt rt -> GateYRootInt rt
    GeneratedAbstractSyntax.GateYRootVar rt -> GateYRootVar (mapVariable rt)
    GeneratedAbstractSyntax.GateYRootDagInt rt -> GateYRootDagInt rt
    GeneratedAbstractSyntax.GateYRootDagVar rt -> GateYRootDagVar (mapVariable rt)
    GeneratedAbstractSyntax.GateZRootInt rt -> GateZRootInt rt
    GeneratedAbstractSyntax.GateZRootVar rt -> GateZRootVar (mapVariable rt)
    GeneratedAbstractSyntax.GateZRootDagInt rt -> GateZRootDagInt rt
    GeneratedAbstractSyntax.GateZRootDagVar rt -> GateZRootDagVar (mapVariable rt)
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
    GeneratedAbstractSyntax.GateSwpRtInt rt -> GateSwpRtInt rt
    GeneratedAbstractSyntax.GateSwpRtVar rt -> GateSwpRtVar (mapVariable rt)
    GeneratedAbstractSyntax.GateSwpRtDagInt rt -> GateSwpRtDagInt rt
    GeneratedAbstractSyntax.GateSwpRtDagVar rt -> GateSwpRtDagVar (mapVariable rt)
    GeneratedAbstractSyntax.GateQftInt n -> GateQftInt n
    GeneratedAbstractSyntax.GateQftVar n -> GateQftVar (mapVariable n)
    GeneratedAbstractSyntax.GateQftDagInt n -> GateQftDagInt n
    GeneratedAbstractSyntax.GateQftDagVar n -> GateQftDagVar (mapVariable n)
    GeneratedAbstractSyntax.GateUnknown3Angle {} -> undefined
    GeneratedAbstractSyntax.GateUnknown2Angle {}  -> undefined
    GeneratedAbstractSyntax.GateUnknown1Angle _ _  -> undefined
    GeneratedAbstractSyntax.GateUnknownInt _ _ -> undefined
    GeneratedAbstractSyntax.GateUnknownVar _ _ -> undefined
    GeneratedAbstractSyntax.GateUnknownSimple _ -> undefined

mapList :: Environment -> GeneratedAbstractSyntax.List -> List
mapList _ GeneratedAbstractSyntax.ListNil = ListNil
mapList env (GeneratedAbstractSyntax.ListSingle term) = ListSingle (mapTerm env term)
mapList env (GeneratedAbstractSyntax.ListMultiple term terms) = ListMultiple (mapTerm env term) (map (mapTerm env) terms)
mapList env (GeneratedAbstractSyntax.ListExpressionAdd l1 l2) = ListExpressionAdd (mapList env l1) (mapList env l2)
mapList env (GeneratedAbstractSyntax.ListCons term list) = ListCons (mapTerm env term) (mapList env list)

mapIntegerExpression :: GeneratedAbstractSyntax.IntegerExpression -> IntegerExpression
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprMinus expr) =
  ArithmExprMinus (mapIntegerExpression expr)
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprAdd expr1 expr2) =
    ArithmExprAdd (mapIntegerExpression expr1) (mapIntegerExpression expr2)
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprSub expr1 expr2) =
    ArithmExprSub (mapIntegerExpression expr1) (mapIntegerExpression expr2)
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprMul expr1 expr2) =
    ArithmExprMul (mapIntegerExpression expr1) (mapIntegerExpression expr2)
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprDiv expr1 expr2) =
    ArithmExprDiv (mapIntegerExpression expr1) (mapIntegerExpression expr2)
mapIntegerExpression (GeneratedAbstractSyntax.ArithmExprInt n) = ArithmExprInt n

mapBoolExpression :: GeneratedAbstractSyntax.BoolExpression -> BoolExpression
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionAnd expr1 expr2) =
    BoolExpressionAnd (mapBoolExpression expr1) (mapBoolExpression expr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionOr expr1 expr2) =
    BoolExpressionOr (mapBoolExpression expr1) (mapBoolExpression expr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionNot expr) =
    BoolExpressionNot (mapBoolExpression expr)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionEq expr1 expr2) =
    BoolExpressionEq (mapBoolExpression expr1) (mapBoolExpression expr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionDif expr1 expr2) =
    BoolExpressionDif (mapBoolExpression expr1) (mapBoolExpression expr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionEqInt intExpr1 intExpr2) =
    BoolExpressionEqInt (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionDifInt intExpr1 intExpr2) =
    BoolExpressionDifInt (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionGt intExpr1 intExpr2) =
    BoolExpressionGt (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionGe intExpr1 intExpr2) =
    BoolExpressionGe (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionLt intExpr1 intExpr2) =
    BoolExpressionLt (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionLe intExpr1 intExpr2) =
    BoolExpressionLe (mapIntegerExpression intExpr1) (mapIntegerExpression intExpr2)
mapBoolExpression (GeneratedAbstractSyntax.BoolExpressionVal val) = BoolExpressionVal (mapBoolValue val)

mapCaseExpression :: Environment -> GeneratedAbstractSyntax.CaseExpression -> CaseExpression
mapCaseExpression env (GeneratedAbstractSyntax.CaseExpr term1 term2) =
    CaseExpr (mapTerm env term1) (mapTerm env term2)

mapFunction :: GeneratedAbstractSyntax.FunctionDeclaration -> Function
mapFunction (GeneratedAbstractSyntax.FunDecl funType funDef) = Function fname (fline, fcol) (mapType ftype) term
   where
     (GeneratedAbstractSyntax.FunType _ ftype) = funType
     (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
     ((fline, fcol), fname) = fvar
     term = mapTerm Data.Map.empty $ toLambdaAbstraction ftype fargs fbody

-- convert functions to Church-style lambda abstractions --

toLambdaAbstraction :: GeneratedAbstractSyntax.Type -> [GeneratedAbstractSyntax.Arg] ->  GeneratedAbstractSyntax.Term -> GeneratedAbstractSyntax.Term

toLambdaAbstraction (GeneratedAbstractSyntax.TypeNonLinear ftype) farg fbody = toLambdaAbstraction ftype farg fbody

toLambdaAbstraction (GeneratedAbstractSyntax.TypeFunction (GeneratedAbstractSyntax.TypeNonLinear ltype) rtype) (GeneratedAbstractSyntax.FunArg (GeneratedAbstractSyntax.Var var) : vars) fbody = 
  GeneratedAbstractSyntax.TermLambda (GeneratedAbstractSyntax.Lambda "\\") (GeneratedAbstractSyntax.Var var) ltype (toLambdaAbstraction rtype vars fbody)

toLambdaAbstraction (GeneratedAbstractSyntax.TypeFunction ltype (GeneratedAbstractSyntax.TypeNonLinear rtype)) (GeneratedAbstractSyntax.FunArg (GeneratedAbstractSyntax.Var var) : vars) fbody = 
  GeneratedAbstractSyntax.TermLambda (GeneratedAbstractSyntax.Lambda "\\") (GeneratedAbstractSyntax.Var var) ltype (toLambdaAbstraction rtype vars fbody)

toLambdaAbstraction (GeneratedAbstractSyntax.TypeFunction ltype rtype) (GeneratedAbstractSyntax.FunArg (GeneratedAbstractSyntax.Var var) : vars) fbody = 
  GeneratedAbstractSyntax.TermLambda (GeneratedAbstractSyntax.Lambda "\\") (GeneratedAbstractSyntax.Var var) ltype (toLambdaAbstraction rtype vars fbody)

toLambdaAbstraction (GeneratedAbstractSyntax.TypeFunction _ _) [] fbody = fbody

toLambdaAbstraction _ _ _ = undefined

-- mapping terms --

mapTerm :: Environment -> GeneratedAbstractSyntax.Term -> Term

mapTerm env (GeneratedAbstractSyntax.TermLambda _ var typ term) = TermLambda (mapType typ) (mapTerm envUpdated term)
  where envUpdated = Data.Map.insert (toVariableName var) 0 (Data.Map.map succ env)

mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "new"))) = TermNew (l, c)         -- special function --
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "measr"))) = TermMeasure (l, c)   -- special function --
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "id"))) = TermId (l, c)           -- special function --
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "inv"))) = TermInverse (l, c)     -- special function --
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "pow"))) = TermPower (l, c)       -- special function --
mapTerm _ (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var ((l, c), "reset"))) = TermReset (l, c)     -- special function --

mapTerm env (GeneratedAbstractSyntax.TermVariable var) = case Data.Map.lookup varName env of
    Just int -> TermBoundVariable int
    Nothing  -> TermFreeVariable varName
  where
    varName = toVariableName var

mapTerm _ (GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil) = TermList ListNil
mapTerm env (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term)) = TermList (ListSingle (mapTerm env term))
mapTerm env (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term terms)) 
  = TermList (ListMultiple (mapTerm env term) (map (mapTerm env) terms))
mapTerm env (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2)) 
  = TermList (ListExpressionAdd (mapList env l1) (mapList env l2))
mapTerm env (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons term list)) =
    TermList (ListCons (mapTerm env term) (mapList env list))

mapTerm env (GeneratedAbstractSyntax.TermListElement l index) = TermListElement (mapList env l) index

mapTerm _ GeneratedAbstractSyntax.TermUnit = TermUnit
mapTerm _ (GeneratedAbstractSyntax.TermBasisState bs) = TermBasisState (mapBasisState bs)
mapTerm _ (GeneratedAbstractSyntax.TermBoolExpression be) = TermBoolExpression (mapBoolExpression be)
mapTerm _ (GeneratedAbstractSyntax.TermIntegerExpression be) = TermIntegerExpression (mapIntegerExpression be)
mapTerm _ (GeneratedAbstractSyntax.TermGate g) = TermGate (mapGate g)
mapTerm env (GeneratedAbstractSyntax.TermTuple term terms) = TermTuple (mapTerm env term)  (map (mapTerm env) (term:terms))
mapTerm env (GeneratedAbstractSyntax.TermApply l r) = TermApply (mapTerm env l) (mapTerm env r)
mapTerm env (GeneratedAbstractSyntax.TermDollar l r) = TermDollar (mapTerm env l) (mapTerm env r)
mapTerm env (GeneratedAbstractSyntax.TermCompose l r) = TermCompose (mapTerm env l) (mapTerm env r)
mapTerm env (GeneratedAbstractSyntax.TermIfElse cond t f) = TermIfElse (mapTerm env cond) (mapTerm env t) (mapTerm env f)
mapTerm env (GeneratedAbstractSyntax.TermTensorProduct t1 t2) = TermTensorProduct (mapTerm env t1) (mapTerm env t2)
mapTerm env (GeneratedAbstractSyntax.TermCase t exprs) = TermCase (mapTerm env t) (map (mapCaseExpression env) exprs)

mapTerm env (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm term) (GeneratedAbstractSyntax.CtrlBasisState bst)) 
  = TermQuantumControlGate [mapTerm env term] [mapBasisState bst]

mapTerm env (GeneratedAbstractSyntax.TermQuantumTCtrlsGate termList bsList)   
  = TermQuantumControlGate (mapTerm env term : map (mapTerm env) terms) (mapBasisState bst : map mapBasisState bsts)
  where
    (GeneratedAbstractSyntax.CtrlTerms term terms) = termList
    (GeneratedAbstractSyntax.CtrlBasisStates bst bsts) = bsList

mapTerm env (GeneratedAbstractSyntax.TermQuantumVCtrlsGate varList bsList) 
  = TermQuantumControlGate (mapTerm env (GeneratedAbstractSyntax.TermVariable var) : map (mapTerm env . GeneratedAbstractSyntax.TermVariable) vars) (mapBasisState bst : map mapBasisState bsts)
  where
      (GeneratedAbstractSyntax.CtrlVars var vars) = varList
      (GeneratedAbstractSyntax.CtrlBasisStates bst bsts) = bsList

mapTerm env (GeneratedAbstractSyntax.TermClassicCtrlGate (GeneratedAbstractSyntax.CtrlTerm term) (GeneratedAbstractSyntax.CtrlBit bit)) 
  = TermClassicControlGate [mapTerm env term] [mapControlBit bit]

mapTerm env (GeneratedAbstractSyntax.TermClassicTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms term terms) (GeneratedAbstractSyntax.CtrlBits bit bits)) 
  = TermClassicControlGate (mapTerm env term : map (mapTerm env) terms) (mapControlBit bit : map mapControlBit bits)

mapTerm env (GeneratedAbstractSyntax.TermClassicVCtrlsGate (GeneratedAbstractSyntax.CtrlVars var vars) (GeneratedAbstractSyntax.CtrlBits bit bits)) 
  = TermClassicControlGate (mapTerm env (GeneratedAbstractSyntax.TermVariable var) : map (mapTerm env . GeneratedAbstractSyntax.TermVariable) vars) (mapControlBit bit : map mapControlBit bits)

mapTerm env (GeneratedAbstractSyntax.TermLetSingle var term1 term2) = TermLet (mapTerm env term1) (mapTerm env' term2)
  where
    env' = Data.Map.insert (getVariableName var) 0 (Data.Map.map succ env)

mapTerm env (GeneratedAbstractSyntax.TermLetSugarSingle var term1 term2) = TermLet (mapTerm env term1) (mapTerm env' term2)
  where
    env' = Data.Map.insert (getVariableName var) 0 (Data.Map.map succ env)

mapTerm env (GeneratedAbstractSyntax.TermLetMultiple var1 [var2] term1 term2) = TermLet (mapTerm env term1) (mapTerm env' term2)
  where
    env' = Data.Map.insert (getVariableName var2) 1 $ Data.Map.insert (getVariableName var1) 0 (Data.Map.map (succ . succ) env)

mapTerm env (GeneratedAbstractSyntax.TermLetSugarMultiple var1 [var2] term1 term2) = TermLet (mapTerm env term1) (mapTerm env' term2)
  where
    env' = Data.Map.insert (getVariableName var2) 1 $ Data.Map.insert (getVariableName var1) 0 (Data.Map.map (succ . succ) env)

mapTerm env (GeneratedAbstractSyntax.TermLetMultiple var1 (var2 : vars) term1 term2) =
  if null vars
      then TermLet (mapTerm env term1) (mapTerm env' (GeneratedAbstractSyntax.TermLetSingle var2 (GeneratedAbstractSyntax.TermVariable var2) term2))
      else TermLet (mapTerm env term1) (mapTerm env' (GeneratedAbstractSyntax.TermLetMultiple var2 vars (GeneratedAbstractSyntax.TermVariable var2) term2))
  where
    env' = Data.Map.insert (getVariableName var1) 1 $ Data.Map.insert (getVariableName var2) 0 (Data.Map.map (succ . succ) env)

mapTerm env (GeneratedAbstractSyntax.TermLetSugarMultiple var1 (var2 : vars) term1 term2) =
  if null vars
      then TermLet (mapTerm env term1) (mapTerm env' (GeneratedAbstractSyntax.TermLetSugarSingle var2 (GeneratedAbstractSyntax.TermVariable var2) term2))
      else TermLet (mapTerm env term1) (mapTerm env' (GeneratedAbstractSyntax.TermLetSugarMultiple var2 vars (GeneratedAbstractSyntax.TermVariable var2) term2))
  where
    env' = Data.Map.insert (getVariableName var1) 1 $ Data.Map.insert (getVariableName var2) 0 (Data.Map.map (succ . succ) env)

-- forbidden by grammar
mapTerm _ (GeneratedAbstractSyntax.TermLetMultiple _ [] _ _) = undefined
mapTerm _ (GeneratedAbstractSyntax.TermLetSugarMultiple _ [] _ _) = undefined

-- some utility functions --

getVariableName:: GeneratedAbstractSyntax.Var -> String
getVariableName ( GeneratedAbstractSyntax.Var (_, qubit)) = qubit

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
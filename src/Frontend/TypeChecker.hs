-- The intermediate abstract syntax tree inspected by a type checker where the type of the
-- expressions in the program are statically checked to be valid according to the type system:
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
{-# LANGUAGE InstanceSigs #-}

module Frontend.TypeChecker
  (
    TypeError,
    runTypeChecker,
  )
where

import qualified Control.Monad.Except
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Control.Monad.State.Class
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Set
import Text.Format (format)

import Debug.Trace (trace, traceIO, traceShow)
-- let tr0 = trace ("What " ++ show term) "?"

import Frontend.ASTtoIASTConverter (Function(..), Gate(..), Program, Term(..), Type(..), List(..), CaseExpression(..), simplifyTensorProd, prnt)

data TypeError
  = NotAFunction Type (Int, Int, String)                        -- this type should be a function but it is not
  | FunctionNotInScope String (Int, Int, String)                -- this variable denotes a function which is not in scope at the point where it is used
  | TypeMismatchFun Type Type (Int, Int, String)                -- this type does not match the type expected at the point where it was declared
  | TypeMismatchIfElse Term Term Type Type (Int, Int, String)   -- this type does not match the type expected at the point where it was declared
  | TypeMismatchApply Term Term Type Type (Int, Int, String)    -- this type does not match the type expected at the point where it was declared
  | TypeMismatchCompose Type Term Type Term (Int, Int, String)  -- this type does not match the type expected at the point where it was declared
  | NotAProductType Type (Int, Int, String)                     -- this type should be a product type but it is not
  | DuplicatedLinearVariable String (Int, Int, String)          -- this linear variable is used more than once
  | NotALinearFunction String (Int, Int, String)                -- this function is used more than once despite not being declared linear
  | NotALinearTerm Term Type (Int, Int, String)                 -- this term should be linear but is is not
  | NoCommonSupertype Type Type (Int, Int, String)              -- these two types have no common supertype
  deriving (Eq, Ord, Read)

instance Show TypeError where
  show :: TypeError -> String

  show (NotAFunction typ (line, _, fname)) = format "The inferred type: '{0}' of the function named: '{1}' defined at line: {2} should be a function type but it is not" [show typ, fname, show line]

  show (FunctionNotInScope var (line, _, fname)) = format "The variable named '{0}' in the function named: '{1}' defined at line: {2} denotes a function which is not in scope" [var, fname, show line]

  show (TypeMismatchFun type1 type2 (line, _, fname)) = format "The expected type '{0}' of the function named: '{1}' defined at line: {2} cannot be matched with actual type: '{3}'" [show type1, fname, show line, show type2]

  show (TypeMismatchIfElse term1 term2 type1 type2 (line, _, fname)) = format "The expected type '{0}' of the function named: '{1}' defined at line: {2} cannot be matched with actual type: '{3}'" [show type1, fname, show line, show type2]

  show (TypeMismatchApply term1 term2 type1 type2 (line, _, fname)) = format "In the function named '{0}' defined at line {1} the expected type '{2}' of term '{3}' is not compatible with type '{4}' of term '{5}'." [fname, show line, show type1, prnt term1, show type2, prnt term2]

  show (TypeMismatchCompose  type1 term1 type2 term2 (line, _, fname)) = format "In the function named '{0}' defined at line {1} the  type '{2}' of term '{3}' cannot composed with type '{4}' of term '{5}'." [fname, show line, show type1, prnt term1, show type2, prnt term2]

  show (NotAProductType typ (line, _, fname)) = format "The type '{0}' in the function named: '{1}' defined at line: {2} is not a product type" [show typ, fname, show line]

  show (DuplicatedLinearVariable var (line, _, fname)) = format "The linear variable '{0}' in the function named: '{1}' defined at line: {2} is used more than once" [var, fname, show line]

  show (NotALinearFunction fun (line, _, fname)) = format "The function named: '{0}' which is used in the function named: '{1}' defined at line: {2} is used more than once despite not being declared linear" [fun, fname, show line]

  show (NotALinearTerm term typ (line, _, fname)) = format "Term: '{0}' having as type: {1} which occurs in function {2} defined at line: {3} is not linear" [show term, show typ, fname, show line]

  show (NoCommonSupertype type1 type2 (line, _, fname)) = format "Could not find a common super-type for types '{0}' and '{1}' expected by function '{2}' defined at line: {3}." [show type1, show type2, fname, show line]

type LinearEnvironment = Data.Set.Set String
type MainEnvironment = Data.Map.Map String Type

data ErrorEnvironment = ErrorEnvironment
  {
    linearEnvironment :: LinearEnvironment,
    currentFunction :: String
  }
  deriving (Show)

type Check = Control.Monad.Except.ExceptT TypeError (Control.Monad.Reader.ReaderT MainEnvironment (Control.Monad.State.State ErrorEnvironment))

runTypeChecker :: Program -> Either String Program
runTypeChecker program =
  case Control.Monad.State.evalState (Control.Monad.Reader.runReaderT (Control.Monad.Except.runExceptT (typeCheckProgram program)) mainEnv) errorEnv of
    Left err -> Left (show err)
    Right _  -> Right program
  where
    mainEnv = Data.Map.fromList (map extractFunNameAndType program)
    errorEnv =
      ErrorEnvironment {
        linearEnvironment = mempty,
        currentFunction = "noCurrentFunction"
      }
    extractFunNameAndType :: Function -> (String, Type)
    extractFunNameAndType (Function fname _ typ _) = (fname, typ)

typeCheckProgram :: Program -> Check ()
typeCheckProgram = mapM_ typeCheckFunction

typeCheckFunction :: Function -> Check ()
typeCheckFunction (Function functionName (line, col) functionType term) = do
    Control.Monad.State.Class.modify $ \x -> x {currentFunction = functionName}
    inferredType <- inferType [] term (line, col, functionName)
    if isSubtype inferredType functionType
        then return ()
        else Control.Monad.Except.throwError (TypeMismatchFun functionType inferredType (line, col, functionName))

isSubtype :: Type -> Type -> Bool
isSubtype (TypeNonLinear t1) (TypeNonLinear t2) = isSubtype (TypeNonLinear t1) t2
isSubtype (TypeNonLinear t1) t2 = isSubtype t1 t2
isSubtype (t1 :->: t2) (t1' :->: t2') = isSubtype t1' t1 && isSubtype t2 t2'
isSubtype (t1 :*: t2) (t1' :*: t2') = isSubtype t1 t1' && isSubtype t2 t2'
isSubtype (t1 :**: n1) (t2 :**: n2) = n1 == n2 && isSubtype t1 t2
isSubtype (TypeList t1) (TypeList t2) = isSubtype t1 t2
isSubtype t1 t2 = t1 == t2

inferType :: [Type] -> Term -> (Int, Int, String) -> Check Type
inferType _ (TermNew _) _  = return $ TypeNonLinear (TypeBasisState :->: TypeQbit)
inferType _ (TermMeasure _) _ = return $ TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)
inferType _ (TermReset _) _  = return $ TypeNonLinear (TypeQbit :->: TypeQbit)
inferType _ (TermId _) _  = return $ TypeNonLinear (TypeQbit :->: TypeQbit)
inferType _ (TermPower _) _  = return $ TypeNonLinear (TypeQbits :->: TypeQbits)
inferType _ (TermInverse _) _  = return $ TypeNonLinear (TypeQbits :->: TypeQbits)
inferType _ (TermBool _) _ = return $ TypeNonLinear TypeBool
inferType _ (TermBit _) _ = return $ TypeNonLinear TypeBit
inferType _ (TermInteger _) _ = return $ TypeNonLinear TypeInteger
inferType _ (TermBasisState _) _ = return $ TypeNonLinear TypeBasisState
inferType _ (TermGate gate) _ = return $ inferGateType gate
inferType _ TermUnit _ = return $ TypeNonLinear TypeUnit

-- new (OK)
-- measr (OK)
-- bit (OK)
-- qbit (OK)
-- reset (OK)
-- function composition (OK)
-- dollar

-- TermCompose
-- TermApply
-- unit
-- TermId
-- TermPower
-- TermInverse
-- TermBool
-- TermInteger
-- TermBasisState BasisState
-- TermTensorProduct
-- TermFreeVariable
-- TermBoundVariable
-- TermVariable Var
-- TermLambda
-- TermIfElse
-- TermTuple
-- TermLet
-- TermCase
-- TermIfElese
-- TermGate Gate
-- TermGateQuantumControl
-- TermGateClassicControl
-- TermList List
-- TermListElement List Integer


inferType context (TermLambda typ term) (line, col, fname) = do
    mainEnv <- Control.Monad.Reader.ask
    let tr0 = trace ("Term " ++ show term) "?"
    let tr1 = trace ("Type " ++ show typ) "?"
    tr0 `seq` tr1 `seq` checkLinearExpression term typ (line, col, fname)
    termTyp <- inferType (typ:context) term (line, col, fname)
    let boundedLinearVars = any (isLinear . (context !!) . fromIntegral) (freeVars (TermLambda typ term))
    let freeLinearVars = any isLinear $ Data.Maybe.mapMaybe (`Data.Map.lookup` mainEnv) (extractFreeVarNames term)
    if boundedLinearVars || freeLinearVars
        then return (typ :->: termTyp)
        else return $ TypeNonLinear (typ :->: termTyp)
        
inferType context (TermIfElse cond t f) (line, col, fname) = do
    typCond <- inferType context cond (line, col, fname)
    -- TODO: are the two lines below correct?
    typT <- inferType context t (line, col, fname)
    typF <- inferType context f (line, col, fname)
    if isSubtype typCond TypeBit
        then supremum typT typF (line, col, fname)
        else Control.Monad.Except.throwError (TypeMismatchIfElse undefined undefined TypeBit typCond (line, col, fname))

inferType context (TermApply termLeft termRight) (line, col, fname) = do
    leftTermType <- inferType context termLeft (line, col, fname)
    rightTermType <- inferType context termRight (line, col, fname)
    case removeBangs leftTermType of
        (argsType :->: returnsType)
            | isSubtype rightTermType argsType -> return returnsType
            | otherwise -> Control.Monad.Except.throwError $ TypeMismatchApply termLeft termRight argsType rightTermType (line, col, fname)
        _ -> Control.Monad.Except.throwError $ NotAFunction leftTermType (line, col, fname)

inferType context (TermCompose termLeft termRight) (line, col, fname) = do
    leftTermType <- inferType context termLeft (line, col, fname)
    rightTermType <- inferType context termRight (line, col, fname)
    case removeBangs leftTermType of
        (argsTypeLeft :->: returnsTypeLeft) ->
            case removeBangs rightTermType of
                (argsTypeRight :->: returnsTypeRight)
                    | isSubtype returnsTypeRight argsTypeLeft -> return (argsTypeRight :->: returnsTypeLeft)
                    | otherwise -> Control.Monad.Except.throwError $ TypeMismatchCompose  leftTermType termLeft rightTermType termRight (line, col, fname)
                _ -> Control.Monad.Except.throwError $ NotAFunction rightTermType (line, col, fname)
        _ -> Control.Monad.Except.throwError $ NotAFunction leftTermType (line, col, fname)

inferType context (TermDollar termLeft termRight) (line, col, fname) = do
    leftTermType <- inferType context termLeft (line, col, fname)
    rightTermType <- inferType context termRight (line, col, fname)
    case removeBangs leftTermType of
        (argsType :->: returnsType)
            | isSubtype rightTermType argsType -> return returnsType
            | otherwise -> Control.Monad.Except.throwError $ TypeMismatchApply termLeft termRight argsType rightTermType (line, col, fname)
        _ -> Control.Monad.Except.throwError $ NotAFunction leftTermType (line, col, fname)

inferType context (TermTuple l [r]) (line, col, fname) = do
    leftTyp <- inferType context l (line, col, fname)
    rightTyp <- inferType context r (line, col, fname)
    return $ simplifyTensorProd $ pullOutBangs (leftTyp :*: rightTyp)

inferType context (TermTuple l (r:rs)) (line, col, fname) = do
    leftTyp <- inferType context l (line, col, fname)
    rightTyp <- inferType context (TermTuple r rs) (line, col, fname)
    return $ simplifyTensorProd $ pullOutBangs (leftTyp :*: rightTyp)

inferType _ (TermFreeVariable var) (line, col, fname) = do
    mainEnv <- Control.Monad.Reader.ask
    linearEnv <- Control.Monad.State.gets linearEnvironment
    case Data.Map.lookup var mainEnv of
        Nothing -> Control.Monad.Except.throwError $ FunctionNotInScope var (line, col, fname)
        Just typ
            | isLinear typ -> if Data.Set.member var linearEnv
                                then Control.Monad.Except.throwError $ NotALinearFunction var (line, col, fname)
                                else Control.Monad.State.Class.modify (\state -> state {linearEnvironment = Data.Set.insert var linearEnv}) >> return typ
            | otherwise -> return typ

inferType context (TermBoundVariable i) _ = do
    let _ = trace ("context: " ++ show context)
    return $ context !! fromIntegral i

inferType _ _ _ = undefined

inferGateType :: Gate -> Type
inferGateType  (GateQftVar _) = TypeQbits
inferGateType  (GateQftDagVar _) = TypeQbits
inferGateType gate
    | qubits >= 2 = TypeQbit :**: qubits
    | otherwise = TypeQbit
    where
        qubits = case gate of
          GateQftInt nq -> nq
          GateQftDagInt nq -> nq
          GateSwp -> 2
          GateSqrtSwp -> 2
          GateSqrtSwpDag -> 2
          GateISwp -> 2
          GateFSwp -> 2
          GateSwpTheta _ -> 2
          GateSwpRtInt _ -> 2
          GateSwpRtVar _ -> 2
          GateSwpRtDagInt _ -> 2
          GateSwpRtDagVar _ -> 2
          _ -> 1

isLinear :: Type -> Bool
isLinear (TypeNonLinear _) = False
isLinear _  = True

removeBangs :: Type -> Type
removeBangs (TypeNonLinear t) = removeBangs t
removeBangs t = t

pullOutBangs :: Type -> Type
pullOutBangs (TypeNonLinear l :*: TypeNonLinear r) = TypeNonLinear (pullOutBangs (l :*: r))
pullOutBangs (TypeNonLinear t :**: n) = TypeNonLinear (pullOutBangs (t :**: n))
pullOutBangs t = t

checkLinearExpression :: Term -> Type -> (Int, Int, String) -> Check ()
checkLinearExpression term typ (line, col, fname) = case typ of
    TypeNonLinear _ -> return ()
    t  -> if boundVarsCount term <= 1
            then return ()
            else  Control.Monad.Except.throwError $ NotALinearTerm term t (line, col, fname)

boundVarsCount :: Term -> Integer
boundVarsCount = boundVarsCount' 0
    where
        boundVarsCount' :: Integer -> Term -> Integer
        boundVarsCount' cnt term = case term of
            TermBoundVariable i -> if cnt == i then 1 else 0
            TermFreeVariable _ -> 0
            TermVariable _ -> undefined -- should not happen
            TermLambda _ lambdaTerm -> boundVarsCount' (cnt + 1) lambdaTerm
            TermLet termEq termIn -> boundVarsCount' cnt termEq + boundVarsCount' (cnt + 2) termIn
            TermIfElse cond t f -> boundVarsCount' cnt cond + max (boundVarsCount' cnt t)  (boundVarsCount' cnt f)
            TermTuple left right -> boundVarsCount' cnt left + sum  (map (boundVarsCount' cnt) right)
            TermApply termLeft termRight  -> boundVarsCount' cnt termLeft + boundVarsCount' cnt termRight
            TermCompose termLeft termRight -> boundVarsCount' cnt termLeft + boundVarsCount' cnt termRight
            TermDollar termLeft termRight -> boundVarsCount' cnt termLeft + boundVarsCount' cnt termRight
            TermTensorProduct t1 t2 -> boundVarsCount' cnt t1 + boundVarsCount' cnt t2
            TermCase t exprs -> boundVarsCount' cnt t + sum  (map extractFromCaseExpr exprs)
              where
                extractFromCaseExpr :: CaseExpression -> Integer
                extractFromCaseExpr (CaseExpr t1 t2) = boundVarsCount' cnt t1 + boundVarsCount' cnt t2
            TermList Frontend.ASTtoIASTConverter.ListNil -> 0
            TermList (ListSingle t) -> boundVarsCount' cnt t
            TermList (ListMultiple t ts) -> boundVarsCount' cnt t + sum (map (boundVarsCount' cnt) ts)
            TermList (ListExpressionAdd l1 l2) -> boundVarsCount' cnt (TermList l1) + boundVarsCount' cnt (TermList l2)
            TermList (ListCons t l) -> boundVarsCount' cnt t + boundVarsCount' cnt (TermList l)
            TermListElement l _ -> boundVarsCount' cnt  (TermList l)
            TermGateQuantumControl terms _ -> sum $ map (boundVarsCount' cnt) terms
            TermGateClassicControl terms _ -> sum $ map (boundVarsCount' cnt) terms
            TermNew _ -> 0
            TermMeasure _ -> 0
            TermInverse _ -> 0
            TermPower _ -> 0
            TermReset _ -> 0
            TermId _ -> 0
            TermBit _ -> 0
            TermBool _ -> 0
            TermInteger _ -> 0
            TermGate _ -> 0
            TermBasisState _ -> 0
            TermUnit -> 0

freeVars :: Term -> [Integer]
freeVars = freeVars' 0
    where
        freeVars' :: Integer -> Term -> [Integer]
        freeVars' cnt (TermLambda _ term) = freeVars' (cnt + 1) term
        freeVars' cnt (TermBoundVariable i) = [i - cnt | i >= cnt]
        freeVars' _ (TermFreeVariable _) = []
        freeVars' _ (TermVariable _) = undefined -- should not happen
        freeVars' cnt (TermIfElse cond t f) = freeVars' cnt cond ++ freeVars' cnt t ++ freeVars' cnt f
        freeVars' cnt (TermTuple left right) = freeVars' cnt left ++ concatMap (freeVars' cnt) right
        freeVars' cnt (TermApply termLeft termRight)  = freeVars' cnt termLeft ++ freeVars' cnt termRight
        freeVars' cnt (TermCompose termLeft termRight)  = freeVars' cnt termLeft ++ freeVars' cnt termRight
        freeVars' cnt (TermDollar termLeft termRight)  = freeVars' cnt termLeft ++ freeVars' cnt termRight
        freeVars' cnt (TermLet termEq termIn) = freeVars' cnt termEq ++ freeVars' (cnt + 2) termIn
        freeVars' cnt (TermTensorProduct t1 t2) = freeVars' cnt t1 ++ freeVars' cnt t2
        freeVars' cnt (TermCase t exprs) = freeVars' cnt t ++ concatMap extractFromCaseExpr exprs
          where
            extractFromCaseExpr :: CaseExpression -> [Integer]
            extractFromCaseExpr (CaseExpr t1 t2) = freeVars' cnt t1 ++ freeVars' cnt t2
        freeVars' _ (TermList  Frontend.ASTtoIASTConverter.ListNil) = []
        freeVars' cnt (TermList  (ListSingle term)) = freeVars' cnt term
        freeVars' cnt (TermList  (ListMultiple term terms)) = freeVars' cnt term ++ concatMap (freeVars' cnt) terms
        freeVars' cnt (TermList  (ListExpressionAdd l1 l2)) = freeVars' cnt (TermList l1) ++ freeVars' cnt (TermList l2)
        freeVars' cnt (TermList  (ListCons t l)) = freeVars' cnt t ++ freeVars' cnt (TermList l)
        freeVars' cnt (TermListElement l _) = freeVars' cnt (TermList l)
        freeVars' cnt (TermGateQuantumControl terms _) = concatMap (freeVars' cnt) terms
        freeVars' cnt (TermGateClassicControl terms _) = concatMap (freeVars' cnt) terms
        freeVars' _ (TermNew _) = []
        freeVars' _ (TermMeasure _) = []
        freeVars' _ (TermInverse _) = []
        freeVars' _ (TermPower _) = []
        freeVars' _ (TermReset _) = []
        freeVars' _ (TermId _) = []
        freeVars' _ (TermBit _) = []
        freeVars' _ (TermBool _) = []
        freeVars' _ (TermInteger _) = []
        freeVars' _ (TermGate _) = []
        freeVars' _ (TermBasisState _) = []
        freeVars' _ TermUnit = []

extractFreeVarNames :: Term -> [String]
extractFreeVarNames (TermFreeVariable fun) = [fun]
extractFreeVarNames (TermBoundVariable _) = []
extractFreeVarNames (TermVariable _) = undefined -- should not happen
extractFreeVarNames (TermLambda _ lambdaTerm) = extractFreeVarNames lambdaTerm
extractFreeVarNames (TermIfElse cond t f) = extractFreeVarNames cond ++ extractFreeVarNames t ++ extractFreeVarNames f
extractFreeVarNames (TermTuple left right) = extractFreeVarNames left ++ concatMap extractFreeVarNames right
extractFreeVarNames (TermApply termLeft termRight)  = extractFreeVarNames termLeft ++ extractFreeVarNames termRight
extractFreeVarNames (TermCompose termLeft termRight)  = extractFreeVarNames termLeft ++ extractFreeVarNames termRight
extractFreeVarNames (TermDollar termLeft termRight)  = extractFreeVarNames termLeft ++ extractFreeVarNames termRight
extractFreeVarNames (TermLet termEq termIn) = extractFreeVarNames termEq ++ extractFreeVarNames termIn
extractFreeVarNames (TermTensorProduct t1 t2) = extractFreeVarNames t1 ++ extractFreeVarNames t2
extractFreeVarNames (TermCase t exprs) = extractFreeVarNames t ++ concatMap extractFromCaseExpr exprs
  where
    extractFromCaseExpr :: CaseExpression -> [String]
    extractFromCaseExpr (CaseExpr t1 t2) = extractFreeVarNames t1 ++ extractFreeVarNames t2
extractFreeVarNames (TermList  Frontend.ASTtoIASTConverter.ListNil) = []
extractFreeVarNames (TermList  (ListSingle term)) = extractFreeVarNames term
extractFreeVarNames (TermList  (ListMultiple term terms)) = extractFreeVarNames term ++ concatMap extractFreeVarNames terms
extractFreeVarNames (TermList  (ListExpressionAdd l1 l2)) = extractFreeVarNames (TermList l1) ++ extractFreeVarNames (TermList l2)
extractFreeVarNames (TermList  (ListCons t l)) = extractFreeVarNames t ++ extractFreeVarNames (TermList l)
extractFreeVarNames (TermListElement l _) = extractFreeVarNames (TermList l)
extractFreeVarNames (TermGateQuantumControl terms _) = concatMap extractFreeVarNames terms
extractFreeVarNames (TermGateClassicControl terms _) = concatMap extractFreeVarNames terms
extractFreeVarNames (TermNew _) = []
extractFreeVarNames (TermMeasure _) = []
extractFreeVarNames (TermInverse _) = []
extractFreeVarNames (TermPower _) = []
extractFreeVarNames (TermReset _) = []
extractFreeVarNames (TermId _) = []
extractFreeVarNames (TermBit _) = []
extractFreeVarNames (TermBool _) = []
extractFreeVarNames (TermInteger _) = []
extractFreeVarNames (TermGate _) = []
extractFreeVarNames (TermBasisState _) = []
extractFreeVarNames TermUnit = []

-- smallest common supertype
supremum :: Type -> Type -> (Int, Int, String) -> Check Type
supremum t1 t2 _
 | t1 == t2 = return t1
supremum (TypeNonLinear (t1 :*: t2)) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> supremum  (TypeNonLinear t1) t1' (line, col, fname) <*> supremum (TypeNonLinear t2) t2' (line, col, fname)
supremum (t1 :*: t2) (TypeNonLinear (t1' :*: t2')) (line, col, fname)
    = (:*:) <$> supremum  t1 (TypeNonLinear t1') (line, col, fname)  <*> supremum t2  (TypeNonLinear t2') (line, col, fname)
supremum (TypeNonLinear (t1 :**: i)) (t2 :**: j) (line, col, fname)
    | i == j =  (:**: i) <$> supremum (TypeNonLinear t1) t2 (line, col, fname)
supremum (t1 :**: i) (TypeNonLinear (t2 :**: j)) (line, col, fname)
    | i == j =  (:**: i) <$> supremum t1 (TypeNonLinear t2) (line, col, fname)
supremum (TypeNonLinear t1) (TypeNonLinear t2) (line, col, fname) =
    TypeNonLinear <$> supremum t1 t2 (line, col, fname)
supremum (TypeNonLinear t1) t2 (line, col, fname) = supremum t1 t2 (line, col, fname)
supremum t1 (TypeNonLinear t2) (line, col, fname) = supremum t1 t2 (line, col, fname)
supremum (t1 :**: i) (t2 :**: j) (line, col, fname)
    | i == j = (:**: i) <$> supremum t1 t2  (line, col, fname)
supremum (t1 :*: t2) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> supremum t1 t1' (line, col, fname) <*> supremum t2 t2' (line, col, fname)
supremum (t1 :->: t2) (t1' :->: t2') (line, col, fname)
    = (:->:) <$> infimum t1 t1' (line, col, fname) <*> supremum t2 t2' (line, col, fname)
supremum t1 t2 (line, col, fname) = Control.Monad.Except.throwError (NoCommonSupertype t1 t2 (line, col, fname))

-- largest common subtype
infimum :: Type -> Type -> (Int, Int, String) -> Check Type
infimum t1 t2 _
    | t1 == t2 = return t1
infimum (TypeNonLinear (t1 :*: t2)) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> infimum  (TypeNonLinear t1) t1' (line, col, fname) <*> infimum (TypeNonLinear t2) t2' (line, col, fname)
infimum (t1 :*: t2) (TypeNonLinear (t1' :*: t2')) (line, col, fname)
    = (:*:) <$> infimum  t1 (TypeNonLinear t1') (line, col, fname)  <*> infimum t2  (TypeNonLinear t2') (line, col, fname)
infimum (TypeNonLinear (t1 :**: i)) (t2 :**: j) (line, col, fname)
    | i == j =  (:**: i) <$> infimum (TypeNonLinear t1) t2 (line, col, fname)
infimum (t1 :**: i) (TypeNonLinear (t2 :**: j)) (line, col, fname)
    | i == j =  (:**: i) <$> infimum t1 (TypeNonLinear t2) (line, col, fname)
infimum (TypeNonLinear t1) (TypeNonLinear t2) (line, col, fname) =
    TypeNonLinear <$> infimum t1 t2 (line, col, fname)
infimum (TypeNonLinear t1) t2 (line, col, fname) = TypeNonLinear <$> infimum t1 t2 (line, col, fname)
infimum t1 (TypeNonLinear t2) (line, col, fname) = TypeNonLinear <$> infimum t1 t2 (line, col, fname)
infimum (t1 :**: i) (t2 :**: j) (line, col, fname)
    | i == j = (:**: i) <$> infimum t1 t2  (line, col, fname)
infimum (t1 :*: t2) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> infimum t1 t1' (line, col, fname) <*> infimum t2 t2' (line, col, fname)
infimum (t1 :->: t2) (t1' :->: t2') (line, col, fname)
    = (:->:) <$> supremum t1 t1' (line, col, fname) <*> infimum t2 t2' (line, col, fname)
infimum t1 t2 (line, col, fname) = Control.Monad.Except.throwError (NoCommonSupertype t1 t2 (line, col, fname))

extractArgTypes :: Type -> [Type]
extractArgTypes typ = reverse $ extractArgTypes' typ
  where
    extractArgTypes' :: Type -> [Type]
    extractArgTypes' (arg :->: res) = res : extractArgTypes' arg
    extractArgTypes' t = [t]

                                       


-- The intermediate abstract syntax tree inspected by a type checker where the type of the
-- expressions in the program are statically checked to be valid according to the type system:
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..

-- TODO: remove after implementation is complete
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Backend.TypeChecker
  (
    TypeError,
    runTypeChecker,
  )
where

import Common (ErrorMessage)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Reader (ReaderT, MonadReader (ask))
import Control.Monad.State (State, gets)
import Control.Monad.State.Class (modify)
import Data.Maybe (mapMaybe)
import Data.Map (Map, lookup)
import Data.Set (Set, member, insert)

import Backend.ASTtoIASTConverter (Function(..), Gate(..), Program, Term(..), Type(..))


data TypeError
  = NotAFunction Type (Int, Int, String)               -- this type should be a function but it is not
  | FunctionNotInScope String (Int, Int, String)       -- this variable denotes a function which is not in scope at the point where it is used
  | TypeMismatch Type Type (Int, Int, String)          -- this type does not match the type expected at the point where it was declared
  | NotAProductType Type (Int, Int, String)            -- this type should be a product type but it is not
  | DuplicatedLinearVariable String (Int, Int, String) -- this linear variable is used more than once
  | NotALinearFunction String (Int, Int, String)       -- this function is used more than once despite being declared linear
  | NotALinearTerm Term Type (Int, Int, String)        -- this term should be linear but is is not
  | NoCommonSupertype Type Type (Int, Int, String)     -- these two types have no common supertype
  deriving (Eq, Ord, Read)

instance Show TypeError where
  show (NotAFunction typ (line, _, fname)) =
    "The inferred type: '" ++ show typ ++  "' of the function named " ++ fname ++ " defined at line: " ++ show line ++ " should be a function type but it is not"
  show (FunctionNotInScope var (line, _, fname)) =
    "The variable named " ++ var ++ " in function " ++ fname ++ " defined at line: " ++ show line ++ " denotes a function which is not in the scope of " ++ fname ++ "."
  show (TypeMismatch type1 type2 (line, _, fname)) =
    "The expected type '" ++ show type1 ++  "' in function " ++ fname ++ " defined at line: " ++ show line ++ " cannot be matched with actual type: " ++ show type2 ++ "'."
  show (NotAProductType typ (line, _, fname)) =
    "The type '" ++ show typ ++ "' in function named " ++ fname ++ " defined at line: " ++ show line ++ " is not a product type."
  show (DuplicatedLinearVariable var (line, _, fname)) =
    "The linear variable '" ++ var ++ "' in function named " ++ fname ++ " defined at line: " ++ show line ++ " is used more than once."
  show (NotALinearFunction fun (line, _, fname)) =
    "Function named: '" ++ show fun ++ "' which is used in function " ++ fname ++ " defined at line: " ++ show line ++ " is used more than once despite being declared linear."
  show (NotALinearTerm term typ (line, _, fname)) =
    "Term: '" ++ show term ++ "' having as type: " ++ show typ ++ " which occurs in function " ++ fname ++ " defined at line: " ++ show line  ++ " is not linear"
  show (NoCommonSupertype type1 type2 (line, _, fname)) =
    "Could not find a common super-type for types '" ++ show type1 ++ " and '" ++ show type2 ++ "' expected by function " ++ fname ++ " defined at line: " ++ show line ++ "."

type LinearEnvironment = Set String
type MainEnvironment = Map String Type

data ErrorEnvironment = ErrorEnvironment
  {
    linearEnvironment :: LinearEnvironment,
    currentFunction :: String
  }
  deriving (Show)

type Check = ExceptT TypeError (ReaderT MainEnvironment (State ErrorEnvironment))

runTypeChecker :: Program -> Either ErrorMessage Program
runTypeChecker program = if null err then Right program else Left err
  where
    err = concatMap typeCheckFunction' program

typeCheckFunction' :: Function -> String
typeCheckFunction' = undefined

-----------------------------------------

typeCheckProgram :: Program -> Check ()
typeCheckProgram = mapM_ typeCheckFunction

typeCheckFunction :: Function -> Check ()
typeCheckFunction (Function functionName (line, col) functionType term) = do
    Control.Monad.State.Class.modify $ \x -> x {currentFunction = functionName}
    inferredType <- inferType [] term (line, col, functionName)
    if isSubtype inferredType functionType
        then return ()
        else Control.Monad.Except.throwError (TypeMismatch functionType inferredType (line, col, functionName))

inferType :: [Type] -> Term -> (Int, Int, String) -> Check Type
inferType _ (TermNew _) _  = return $ TypeNonLinear (TypeBit :->: TypeQbit)
inferType _ (TermMeasure _) _ = return $ TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)
inferType _ (TermBit _) _ = return $ TypeNonLinear TypeBit
inferType _ (TermGate gate) _ = return $ inferGateType gate
inferType _ TermUnit _ = return $ TypeNonLinear TypeUnit

-- TODO: extend
inferType _ (TermBoundVariable _) _ = undefined
inferType _ (TermCase _ _) _ = undefined
inferType _ (TermQuantumCtrlGate _ _) _ = undefined
inferType _ (TermQuantumCtrlsGate _ _) _ = undefined
inferType _ (TermClassicCtrlGate _ _) _ = undefined
inferType _ (TermClassicCtrlsGate _ _) _ = undefined
inferType _ (TermDollar _ _) _ = undefined
inferType _ (TermCompose _ _) _ = undefined
inferType _ (TermBasisState _) _ = undefined

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

inferType context (TermLambda typ term) (line, col, fname) = do
    mainEnv <- Control.Monad.Reader.ask
    checkLinearExpression term typ (line, col, fname)
    termTyp <- inferType (typ:context) term (line, col, fname)
    let boundedLinearVars = any (isLinear . (context !!) . fromIntegral) (freeVariables (TermLambda typ term))
    let freeLinearVars = any isLinear $ Data.Maybe.mapMaybe (`Data.Map.lookup` mainEnv) (extractFunctionNames term)
    if boundedLinearVars || freeLinearVars
        then return (typ :->: termTyp)
        else return $ TypeNonLinear (typ :->: termTyp)

inferType context (TermApply termLeft termRight) (line, col, fname) = do
    leftTermType <- inferType context termLeft (line, col, fname)
    rightTermType <- inferType context termRight (line, col, fname)
    case removeBangs leftTermType of
        (argsType :->: returnsType)
            | isSubtype rightTermType argsType -> return returnsType
            | otherwise -> Control.Monad.Except.throwError $ TypeMismatch argsType rightTermType (line, col, fname)
        _ -> throwError $ NotAFunction leftTermType (line, col, fname)

inferType context (TermTuple left right) (line, col, fname) = do
    leftTyp <- inferType context left (line, col, fname)
    rightTyp <- inferType context right (line, col, fname)
    return $ pullOutBangs (leftTyp :*: rightTyp)

inferType context (TermIfElse cond t f) (line, col, fname) = do
    typCond <- inferType context cond (line, col, fname)
    -- TODO: are the two lines below correct?
    typT <- inferType context t (line, col, fname)
    typF <- inferType context f (line, col, fname)
    if isSubtype typCond TypeBit
        then smallestCommonSupertype typT typF (line, col, fname)
        else Control.Monad.Except.throwError (TypeMismatch TypeBit typCond (line, col, fname))

inferType context (TermLetMultiple termEq termIn) (line, col, fname) = do
    typEq <- inferType context termEq (line, col, fname)
    let noBangs = numberOfBangs typEq
    case removeBangs typEq of
        (t1 :*: t2) -> do
            let t1' = appendBangs noBangs t1
            let t2' = appendBangs noBangs t2
            checkLinearExpression termIn t2' (line, col, fname)
            checkLinearExpression (TermLambda t2' termIn) t1' (line, col, fname)
            inferType (t2' : t1' : context) termIn (line, col, fname)
        _ -> throwError $ NotAProductType typEq (line, col, fname)

inferType context (TermLetSugarMultiple termEq termIn) (line, col, fname) = do
    typEq <- inferType context termEq (line, col, fname)
    let noBangs = numberOfBangs typEq
    case removeBangs typEq of
        (t1 :*: t2) -> do
            let t1' = appendBangs noBangs t1
            let t2' = appendBangs noBangs t2
            checkLinearExpression termIn t2' (line, col, fname)
            checkLinearExpression (TermLambda t2' termIn) t1' (line, col, fname)
            inferType (t2' : t1' : context) termIn (line, col, fname)
        _ -> throwError $ NotAProductType typEq (line, col, fname)

-- TODO: fix
inferType context (TermLetSingle termEq termIn) (line, col, fname) = undefined

-- TODO: fix
inferType context (TermLetSugarSingle termEq termIn) (line, col, fname) = undefined

smallestCommonSupertype :: Type -> Type -> (Int, Int, String) -> Check Type
smallestCommonSupertype t1 t2 _
    | t1 == t2 = return t1
smallestCommonSupertype (TypeNonLinear (t1 :*: t2)) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> smallestCommonSupertype  (TypeNonLinear t1) t1' (line, col, fname) <*> smallestCommonSupertype (TypeNonLinear t2) t2' (line, col, fname)
smallestCommonSupertype (t1 :*: t2) (TypeNonLinear (t1' :*: t2')) (line, col, fname)
    = (:*:) <$> smallestCommonSupertype  t1 (TypeNonLinear t1') (line, col, fname)  <*> smallestCommonSupertype t2  (TypeNonLinear t2') (line, col, fname)
smallestCommonSupertype (TypeNonLinear (t1 :**: i)) (t2 :**: j) (line, col, fname)
    | i == j =  (:**: i) <$> smallestCommonSupertype (TypeNonLinear t1) t2 (line, col, fname)
smallestCommonSupertype (t1 :**: i) (TypeNonLinear (t2 :**: j)) (line, col, fname)
    | i == j =  (:**: i) <$> smallestCommonSupertype t1 (TypeNonLinear t2) (line, col, fname)
smallestCommonSupertype (TypeNonLinear t1) (TypeNonLinear t2) (line, col, fname) =
    TypeNonLinear <$> smallestCommonSupertype t1 t2 (line, col, fname)
smallestCommonSupertype (TypeNonLinear t1) t2 (line, col, fname) = smallestCommonSupertype t1 t2 (line, col, fname)
smallestCommonSupertype t1 (TypeNonLinear t2) (line, col, fname) = smallestCommonSupertype t1 t2 (line, col, fname)
smallestCommonSupertype (t1 :**: i) (t2 :**: j) (line, col, fname)
    | i == j = (:**: i) <$> smallestCommonSupertype t1 t2  (line, col, fname)
smallestCommonSupertype (t1 :*: t2) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> smallestCommonSupertype t1 t1' (line, col, fname) <*> smallestCommonSupertype t2 t2' (line, col, fname)
smallestCommonSupertype (t1 :->: t2) (t1' :->: t2') (line, col, fname)
    = (:->:) <$> largestCommonSubtype t1 t1' (line, col, fname) <*> smallestCommonSupertype t2 t2' (line, col, fname)
smallestCommonSupertype t1 t2 (line, col, fname) = Control.Monad.Except.throwError (NoCommonSupertype t1 t2 (line, col, fname))

largestCommonSubtype :: Type -> Type -> (Int, Int, String) -> Check Type
largestCommonSubtype t1 t2 _
    | t1 == t2 = return t1
largestCommonSubtype (TypeNonLinear (t1 :*: t2)) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> largestCommonSubtype  (TypeNonLinear t1) t1' (line, col, fname) <*> largestCommonSubtype (TypeNonLinear t2) t2' (line, col, fname)
largestCommonSubtype (t1 :*: t2) (TypeNonLinear (t1' :*: t2')) (line, col, fname)
    = (:*:) <$> largestCommonSubtype  t1 (TypeNonLinear t1') (line, col, fname)  <*> largestCommonSubtype t2  (TypeNonLinear t2') (line, col, fname)
largestCommonSubtype (TypeNonLinear (t1 :**: i)) (t2 :**: j) (line, col, fname)
    | i == j =  (:**: i) <$> largestCommonSubtype (TypeNonLinear t1) t2 (line, col, fname)
largestCommonSubtype (t1 :**: i) (TypeNonLinear (t2 :**: j)) (line, col, fname)
    | i == j =  (:**: i) <$> largestCommonSubtype t1 (TypeNonLinear t2) (line, col, fname)
largestCommonSubtype (TypeNonLinear t1) (TypeNonLinear t2) (line, col, fname) =
    TypeNonLinear <$> largestCommonSubtype t1 t2 (line, col, fname)
largestCommonSubtype (TypeNonLinear t1) t2 (line, col, fname) = TypeNonLinear <$> largestCommonSubtype t1 t2 (line, col, fname)
largestCommonSubtype t1 (TypeNonLinear t2) (line, col, fname) = TypeNonLinear <$> largestCommonSubtype t1 t2 (line, col, fname)
largestCommonSubtype (t1 :**: i) (t2 :**: j) (line, col, fname)
    | i == j = (:**: i) <$> largestCommonSubtype t1 t2  (line, col, fname)
largestCommonSubtype (t1 :*: t2) (t1' :*: t2') (line, col, fname)
    = (:*:) <$> largestCommonSubtype t1 t1' (line, col, fname) <*> largestCommonSubtype t2 t2' (line, col, fname)
largestCommonSubtype (t1 :->: t2) (t1' :->: t2') (line, col, fname)
    = (:->:) <$> smallestCommonSupertype t1 t1' (line, col, fname) <*> largestCommonSubtype t2 t2' (line, col, fname)
largestCommonSubtype t1 t2 (line, col, fname) = Control.Monad.Except.throwError (NoCommonSupertype t1 t2 (line, col, fname))

isSubtype :: Type -> Type -> Bool
isSubtype (TypeNonLinear t1 :*: t2) (t1' :*: t2') = isSubtype (TypeNonLinear t1) t1' && isSubtype (TypeNonLinear t2) t2'
isSubtype (TypeNonLinear t1 :+: t2) (t1' :+: t2') = isSubtype (TypeNonLinear t1) t1' && isSubtype (TypeNonLinear t2) t2'
isSubtype (TypeNonLinear t1) (TypeNonLinear t2) = isSubtype (TypeNonLinear t1) t2
isSubtype (TypeNonLinear t1) t2 = isSubtype t1 t2
isSubtype (t1 :->: t2) (t1' :->: t2') = isSubtype t1 t1' && isSubtype t2 t2'
isSubtype (t1 :*: t2) (t1' :*: t2') = isSubtype t1 t1' && isSubtype t2 t2'
isSubtype (t1 :**: i) (t1' :**: j) = isSubtype t1 t1' && i == j
isSubtype (t1 :+: t2) (t1' :+: t2') = isSubtype t1 t1' && isSubtype t2 t2'
isSubtype t1 t2  = t1 == t2

inferGateType :: Gate -> Type
inferGateType gate
    | qubits > 2 = TypeQbit :**: qubits
    | qubits == 2 = TypeQbit :*: TypeQbit
    | otherwise = TypeQbit
    where
        qubits = case gate of
          GateQft nq -> nq
          GateQftDag nq -> nq
          GateSwp -> 2
          GateSqrtSwp -> 2
          GateSqrtSwpDag -> 2
          GateFSwp -> 2
          GateSwpTheta _ -> 2
          GateSwpRt _ -> 2
          GateSwpRtDag _ -> 2
          _ -> 1

pullOutBangs :: Type -> Type
pullOutBangs (TypeNonLinear l :+: TypeNonLinear r) = TypeNonLinear (pullOutBangs (l :+: r))
pullOutBangs (TypeNonLinear l :*: TypeNonLinear r) = TypeNonLinear (pullOutBangs (l :*: r))
pullOutBangs (TypeNonLinear t :**: n) = TypeNonLinear (pullOutBangs (t :**: n))
pullOutBangs t = t

removeBangs :: Type -> Type
removeBangs (TypeNonLinear t) = removeBangs t
removeBangs t = t

numberOfBangs :: Type -> Integer
numberOfBangs (TypeNonLinear t) = 1 + numberOfBangs t
numberOfBangs _ = 0

appendBangs :: Integer -> Type -> Type
appendBangs 0 t = t
appendBangs n t = appendBangs (n - 1) (TypeNonLinear t)

isLinear :: Type -> Bool
isLinear (TypeNonLinear _) = False
isLinear _  = True

checkLinearExpression :: Term -> Type -> (Int, Int, String) -> Check ()
checkLinearExpression term typ (line, col, fname) = case typ of
    TypeNonLinear _ -> return ()
    t  -> if headBoundVariableCount term <= 1
            then return ()
            else  Control.Monad.Except.throwError $ NotALinearTerm term t (line, col, fname)

headBoundVariableCount :: Term -> Integer
headBoundVariableCount = headBoundVariableCount' 0
    where
        headBoundVariableCount' :: Integer -> Term -> Integer
        headBoundVariableCount' cnt term = case term of
            TermBoundVariable i -> if cnt == i then 1 else 0
            TermLambda _ lambdaTerm -> headBoundVariableCount' (cnt + 1) lambdaTerm
            TermApply termLeft termRight -> headBoundVariableCount' cnt termLeft + headBoundVariableCount' cnt termRight
            TermIfElse cond t f -> headBoundVariableCount' cnt cond + max (headBoundVariableCount' cnt t) (headBoundVariableCount' cnt f)
            TermTuple left right -> headBoundVariableCount' cnt left + headBoundVariableCount' cnt right
            TermLetSingle termEq termIn -> headBoundVariableCount' cnt termEq + headBoundVariableCount' (cnt + 1) termIn        --TODO: verify
            TermLetSugarSingle termEq termIn -> headBoundVariableCount' cnt termEq + headBoundVariableCount' (cnt + 1) termIn   --TODO: verify
            TermLetMultiple termEq termIn -> headBoundVariableCount' cnt termEq + headBoundVariableCount' (cnt + 2) termIn
            TermLetSugarMultiple termEq termIn -> headBoundVariableCount' cnt termEq + headBoundVariableCount' (cnt + 2) termIn
            _  -> 0

freeVariables :: Term -> [Integer]
freeVariables = freeVariables' 0
    where
        freeVariables' :: Integer -> Term -> [Integer]
        freeVariables' cnt (TermTuple left right)    = freeVariables' cnt left ++ freeVariables' cnt right
        freeVariables' cnt (TermApply termLeft termRight)    = freeVariables' cnt termLeft ++ freeVariables' cnt termRight
        freeVariables' cnt (TermLetSingle termEq termIn) = freeVariables' cnt termEq ++ freeVariables' (cnt + 1) termIn         --TODO: verify
        freeVariables' cnt (TermLetSugarSingle termEq termIn) = freeVariables' cnt termEq ++ freeVariables' (cnt + 1) termIn    --TODO: verify
        freeVariables' cnt (TermLetMultiple termEq termIn) = freeVariables' cnt termEq ++ freeVariables' (cnt + 2) termIn
        freeVariables' cnt (TermLetSugarMultiple termEq termIn) = freeVariables' cnt termEq ++ freeVariables' (cnt + 2) termIn
        freeVariables' cnt (TermLambda _ lambdaTerm)    = freeVariables' (cnt + 1) lambdaTerm
        freeVariables' cnt (TermBoundVariable i) = [i - cnt | i >= cnt]
        freeVariables' _ _ = []

extractFunctionNames :: Term -> [String]
extractFunctionNames (TermTuple left right) = extractFunctionNames left ++ extractFunctionNames right
extractFunctionNames (TermApply termLeft termRight)  = extractFunctionNames termLeft ++ extractFunctionNames termRight
extractFunctionNames (TermLetSingle termEq termIn) = extractFunctionNames termEq ++ extractFunctionNames termIn
extractFunctionNames (TermLetSugarSingle termEq termIn) = extractFunctionNames termEq ++ extractFunctionNames termIn
extractFunctionNames (TermLetMultiple termEq termIn) = extractFunctionNames termEq ++ extractFunctionNames termIn
extractFunctionNames (TermLetSugarMultiple termEq termIn) = extractFunctionNames termEq ++ extractFunctionNames termIn
extractFunctionNames (TermLambda _ lambdaTerm) = extractFunctionNames lambdaTerm
extractFunctionNames (TermFreeVariable fun) = [fun]
extractFunctionNames _ = []
-- The intermediate abstract syntax tree inspected by a type checker where the type of the
-- expressions in the program are statically checked to be valid according to the type system:
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..

module Backend.TypeChecker
  (
    TypeError,
    runTypeChecker,
  )
where

import Common (ErrorMessage)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Control.Monad.State.Class (modify)
import Data.Map (Map)
import Data.Set (Set)

import Backend.ASTtoIASTConverter (Function(..), Gate(..), Program, Term(..), Type(..))

data TypeError
  = NotAFunction Type (Int, Int, String)               -- this type should be a function but it is not
  | FunctionNotInScope String (Int, Int, String)       -- this variable denotes a function which is not in scope at the point where it is declared
  | TypeMismatch Type Type (Int, Int, String)          -- this type does not match the type expected at the point where it was declared
  | NotAProductType Type (Int, Int, String)            -- this type should be a product type but it is not
  | DuplicatedLinearVariable String (Int, Int, String) -- this linear variable is used more than once
  | NotALinearTerm Type Term (Int, Int, String)        -- this term is not linear despite being declared linear
  | NoCommonSupertype Type Type (Int, Int, String)     -- these two types have no common supertype
  deriving (Eq, Ord, Read)

instance Show TypeError where
  show (NotAFunction typ (line, col, fname)) =
    "The type '" ++ show typ ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is not a function type."
  show (FunctionNotInScope var (line, col, fname)) =
    "The variable " ++ var ++ " at line: " ++ show line ++ " and column: "++ show col ++ " denotes a function which is not in scope."
  show (TypeMismatch type1 type2 (line, col, fname)) =
    "The expected type '" ++ show type1 ++  "' for function " ++ fname ++ " at line: " ++ show line ++ " and column: "++ show col ++ " cannot be matched with actual type '" ++ show type2 ++ "'"
  show (NotAProductType typ (line, col, fname)) =
    "The type '" ++ show typ ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is not a product type."
  show (DuplicatedLinearVariable var (line, col, fname)) =
    "The linear variable '" ++ var ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is used more than once."
  show (NotALinearTerm typ term (line, col, fname)) =
    "Expression " ++ show term ++ " at line: " ++ show line ++ " and column: "++ show col ++ " is not a linear type but: " ++ show typ
  show (NoCommonSupertype type1 type2 (line, col, fname)) =
    "Could not find a common super-type for types '" ++ show type1 ++ " and '" ++ show type2 ++ "' expected by function " ++ fname ++ " at line: " ++ show line ++ " and column: " ++ show col

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
    modify $ \x -> x {currentFunction = functionName}
    inferredType <- inferType [] term (line, col, functionName)
    if subtypeOf inferredType functionType
        then return ()
        else throwError (TypeMismatch functionType inferredType (line, col, functionName))

inferType :: [Type] -> Term -> (Int, Int, String) -> Check Type
inferType _ (TermNew _) _  = return $ TypeNonLinear (TypeBit :->: TypeQbit)
inferType _ (TermMeasure _) _ = return $ TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)
inferType _ (TermBit _) _ = return $ TypeNonLinear TypeBit
inferType _ (TermGate gate) _ = return $ inferGateType gate
inferType _ TermUnit _ = return $ TypeNonLinear TypeUnit
inferType context (TermTuple left right) (line, col, fname) = do
    left <- inferType context left (line, col, fname)
    right <- inferType context right (line, col, fname)
    return $ pullOutBangs (left :*: right)
inferType context (TermIfElse cond t f) (line, col, fname) = do
    typCond <- inferType context cond (line, col, fname)
    -- TODO: are the two lines below correct?
    typT <- inferType context t (line, col, fname)
    typF <- inferType context f (line, col, fname)
    if subtypeOf typCond TypeBit
        then smallestCommonSupertype typT typF (line, col, fname)
        else throwError (TypeMismatch TypeBit typCond (line, col, fname))
-- inferType context (TermLetSingle termEq termIn) = do
--     typEq <- inferType context termEq
--     let bangs = numberOfBangs typEq
--     case removeBangs typEq of
--         (a1 :*: a2) -> do
--             let a1t = addBangs bangs a1
--             let a2t = addBangs bangs a2
--             checkLinear inn a2t
--             checkLinear (Abs a2t inn) a1t
--             inferTerm (a2t : a1t : ctx) inn
--         _ -> throwError $ NotAProductType teq


smallestCommonSupertype :: Type -> Type -> (Int, Int, String) -> Check Type
smallestCommonSupertype t1 t2 _ | t1 == t2 = return t1
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
smallestCommonSupertype t1 t2 (line, col, fname) = throwError (NoCommonSupertype t1 t2 (line, col, fname))

largestCommonSubtype :: Type -> Type -> (Int, Int, String) -> Check Type
largestCommonSubtype t1 t2 _ | t1 == t2 = return t1
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
largestCommonSubtype t1 t2 (line, col, fname) = throwError (NoCommonSupertype t1 t2 (line, col, fname))

subtypeOf :: Type -> Type -> Bool
subtypeOf (TypeNonLinear t1 :*: t2) (t1' :*: t2') = subtypeOf (TypeNonLinear t1) t1' && subtypeOf (TypeNonLinear t2) t2'
subtypeOf (TypeNonLinear t1 :+: t2) (t1' :+: t2') = subtypeOf (TypeNonLinear t1) t1' && subtypeOf (TypeNonLinear t2) t2'
subtypeOf (TypeNonLinear t1) (TypeNonLinear t2) = subtypeOf (TypeNonLinear t1) t2
subtypeOf (TypeNonLinear t1) t2 = subtypeOf t1 t2
subtypeOf (t1 :->: t2) (t1' :->: t2') = subtypeOf t1 t1' && subtypeOf t2 t2'
subtypeOf (t1 :*: t2) (t1' :*: t2') = subtypeOf t1 t1' && subtypeOf t2 t2'
subtypeOf (t1 :**: i) (t1' :**: j) = subtypeOf t1 t1' && i == j
subtypeOf (t1 :+: t2) (t1' :+: t2') = subtypeOf t1 t1' && subtypeOf t2 t2'
subtypeOf t1 t2  = t1 == t2

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
numberOfBangs t = 0

appendBangs :: Integer -> Type -> Type
appendBangs 0 t = t
appendBangs n t = appendBangs (n - 1) (TypeNonLinear t)

isLinear :: Type -> Bool
isLinear (TypeNonLinear _) = False
isLinear _  = True


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

--import Control.Monad.Except (ExceptT (..), throwError, runExceptT)
--import Control.Monad.Reader (ReaderT, MonadReader (ask), runReaderT)
--import Control.Monad.State (State, gets, evalState)
--import Control.Monad.State.Class (modify)
--import Data.Maybe (mapMaybe)
--import Data.Map (Map, lookup)
--import Data.Set (Set, member, insert)

import qualified Common
import qualified Data.Map
import qualified Control.Monad.Except
import qualified Control.Monad.Reader
import qualified Control.Monad.State
import qualified Control.Monad.State.Class
import qualified Data.Set

import Frontend.ASTtoIASTConverter (Function(..), Gate(..), Program, Term(..), Type(..))

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
  show :: TypeError -> String
  show (NotAFunction typ (line, _, fname)) = "The inferred type: '" ++ show typ ++  "' of the top level function named: '" ++ fname ++ "' defined at line: " ++ show line ++ " should be a function type but it is not"
    
  show (FunctionNotInScope var (line, _, fname)) = "The variable named '" ++ var ++ "' in the top level function named: '" ++ fname ++ "' defined at line: " 
    ++ show line ++ " denotes a function which is not in scope"

  show (TypeMismatch type1 type2 (line, _, fname)) = "The expected type '" ++ show type1 ++  "' of the top level function named: '" ++ fname ++ "' defined at line: " ++ show line ++ " cannot be matched with actual type: '" ++ show type2 ++ "'"

  show (NotAProductType typ (line, _, fname)) = "The type '" ++ show typ ++ "' in the top level function named: '" ++ fname ++ "' defined at line: " 
    ++ show line ++ " is not a product type"

  show (DuplicatedLinearVariable var (line, _, fname)) = "The linear variable '" ++ var ++ "' in the top level function named: '" ++ fname ++ "' defined at line: " ++ show line ++ " is used more than once"

  show (NotALinearFunction fun (line, _, fname)) = "The function named: '" ++ show fun ++ "' which is used in the top level function named: '" ++ fname 
    ++ ". defined at line: " ++ show line ++ " is used more than once despite being declared linear"

  show (NotALinearTerm term typ (line, _, fname)) = "Term: '" ++ show term ++ "' having as type: " ++ show typ 
    ++ " which occurs in function " ++ fname ++ " defined at line: " ++ show line  ++ " is not linear"

  show (NoCommonSupertype type1 type2 (line, _, fname)) = "Could not find a common super-type for types '" 
    ++ show type1 ++ " and '" ++ show type2 ++ "' expected by top level function '" ++ fname ++ "' defined at line: " ++ show line ++ "."

type LinearEnvironment = Data.Set.Set String
type MainEnvironment = Data.Map.Map String Type

data ErrorEnvironment = ErrorEnvironment
  {
    linearEnvironment :: LinearEnvironment,
    currentFunction :: String
  }
  deriving (Show)

type Check = Control.Monad.Except.ExceptT TypeError (Control.Monad.Reader.ReaderT MainEnvironment (Control.Monad.State.State ErrorEnvironment))

runTypeChecker :: Program -> Either Common.ErrorMessage Program
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
        else Control.Monad.Except.throwError (TypeMismatch functionType inferredType (line, col, functionName))

isSubtype :: Type -> Type -> Bool
isSubtype (TypeNonLinear t1) t2 = isSubtype t1 t2
isSubtype _ (TypeNonLinear _) = False
isSubtype (TypeList t1) (TypeList t2) = isSubtype t1 t2
isSubtype (t1 :->: t2) (t1' :->: t2') = isSubtype t1' t1 && isSubtype t2 t2'
isSubtype (t1 :*: t2) (t1' :*: t2') = isSubtype t1 t1' && isSubtype t2 t2'
isSubtype (t1 :**: n1) (t2 :**: n2) = n1 == n2 && isSubtype t1 t2
isSubtype t1 t2 = t1 == t2

inferType :: [Type] -> Term -> (Int, Int, String) -> Check Type
inferType _ (TermNew _) _  = return $ TypeNonLinear (TypeBasisState :->: TypeQbit)
inferType _ (TermMeasure _) _ = return $ TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)
inferType _ (TermReset _) _  = return $ TypeNonLinear (TypeQbit :->: TypeQbit)
inferType _ (TermId _) _  = return $ TypeNonLinear (TypeQbit :->: TypeQbit)
inferType _ (TermPower _) _  = return $ TypeNonLinear (TypeQbits :->: TypeQbits)
inferType _ (TermInverse _) _  = return $ TypeNonLinear (TypeQbits :->: TypeQbits)
inferType _ (TermBit _) _ = return $ TypeNonLinear TypeBit
inferType _ (TermGate gate) _ = return $ inferGateType gate
inferType _ TermUnit _ = return $ TypeNonLinear TypeUnit
inferType _ (TermBasisState _) _ = return TypeBasisState

inferType context (TermApply termLeft termRight) (line, col, fname) = do
    leftTermType <- inferType context termLeft (line, col, fname)
    rightTermType <- inferType context termRight (line, col, fname)
    case removeBangs leftTermType of
        (argsType :->: returnsType)
            | isSubtype rightTermType argsType -> return returnsType
            | otherwise -> Control.Monad.Except.throwError $ TypeMismatch argsType rightTermType (line, col, fname)
        _ -> Control.Monad.Except.throwError $ NotAFunction leftTermType (line, col, fname)

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
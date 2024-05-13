-- The intermediate abstract syntax tree inspected by a type checker where the type of the
-- expressions in the program are statically checked to be valid according to the type system:
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Backend.TypeChecker
  ( TypeError,
    runTypeChecker,
  )
where

import Backend.ASTtoIASTConverter (Function(..), Program, Term, Type)
import Common (ErrorMessage)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Map (Map)
import Data.Set (Set)

-- import Backend.ASTtoIASTConverter (Function, Program, Term, Type, Var, mapProgram)
-- import Frontend.LambdaQ.Par ( myLexer, pProgram )
-- import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
-- import GHC.Base (undefined)

data TypeError
  = NotAFunction Type -- this type should be a function but it is not
  | FunctionNotInScope String -- this variable denotes a function which is not in scope at the point where it is declared
  | TypeMismatch Type Type -- this type does not match the type expected at the point where it was declared
  | NotAProductType Type -- this type should be a product type but it is not
  | DuplicatedLinearVariable String -- this linear variable is used more than once
  | NotALinearTerm Type Term -- this term is not linear despite being declared linear
  | NoCommonSupertype Type Type -- these two types have no common supertype
  deriving (Eq, Ord, Read)

instance Show TypeError where
  show (NotAFunction typ) = "The type '" ++ show typ ++ "' is not a function type."
  show (FunctionNotInScope var) = "The variable " ++ var ++ " denotes a function which is not in scope."
  show (TypeMismatch type1 type2) = "The expected type '" ++ show type1 ++ "' cannot be matched with actual type '" ++ show type2 ++ "'"
  show (NotAProductType typ) = "The type '" ++ show typ ++ "' is not a product type."
  show (DuplicatedLinearVariable var) = "The linear variable '" ++ var ++ "' is used more than once."
  show (NotALinearTerm typ term) = "Expression " ++ show term ++ " is not a linear type but: " ++ show typ
  show (NoCommonSupertype type1 type2) = "Could not find a common super-type for types '" ++ show type1 ++ "' and '" ++ show type2 ++ "'"

data TypeCheckFailure = TypeCheckFailure String TypeError
  deriving (Eq, Ord, Show, Read)

type LinearEnvironment = Set String
type MainEnvironment = Map String Type

data ErrorEnvironment = ErrorEnvironment
  { 
    linearEnvironment :: LinearEnvironment,
    currentFunction :: String
  }
  deriving (Show)

type Check = ExceptT TypeCheckFailure (ReaderT MainEnvironment (State ErrorEnvironment))

runTypeChecker :: Program -> Either ErrorMessage Program
runTypeChecker program = if null err then Right program else Left err
  where
    err = concatMap typeCheckFunction' program

typeCheckFunction' :: Function -> String
typeCheckFunction' = undefined

-----------------------------------------

typeCheckProgram :: Program -> Check ()
typeCheckProgram program = mapM_ typeCheckFunction program

typeCheckFunction :: Function -> Check ()
typeCheckFunction (Function functionName (line, col) functionType term) = do
    modify $ \x -> x {currentFunction = functionName}
    inferredType <- inferType term
    if inferredType <: functionType
        then return ()
        else throwError (TypeMismatch functionType inferredType)

inferType :: Term -> Check Type
inferType = undefined

infixr 1 <:

(<:) :: Type -> Type -> Bool
a <: b  = a == b    
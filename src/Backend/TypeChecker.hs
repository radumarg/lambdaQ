-- The intermediate abstract syntax tree inspected by a type checker where the type of the 
-- expressions in the program are statically checked to be valid according to the type system: 
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally 
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..

module Backend.TypeChecker (TypeError, runTypeChecker, ) where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)

import Backend.ASTtoIASTConverter (Program, Term, Type, Var, mapProgram)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import GHC.Base (undefined)

data TypeErrorInstance =
    NotAFunction Type             |
    VariableNotInScope String     |
    TypeMismatch Type Type        |
    NotAProductType Type          | 
    NotALinearType String         |
    NotALinearTerm Var Type Term  |
    NoCommonSupertype Type Type
    deriving (Eq, Ord, Read)

instance Show TypeErrorInstance where
    show (NotAFunction typ) = "The type '" ++ show typ ++ "' is not a function type."
    show (VariableNotInScope var) = "The variable " ++ var ++ " is not in scope."
    show (TypeMismatch type1 type2) = "The expected type '" ++ show type1 ++"' cannot be matched with actual type '" ++ show type2 ++ "'"
    show (NotAProductType typ) = "The type '" ++ show typ ++ "' is not a product type."
    show (NotALinearType var) = "The linear variable '" ++ var ++ "' is used more than once."
    --show (NotALinearTerm var typ term) = "Expression is not a linear term: " ++ show (GeneratedAbstractSyntax.TermLambda GeneratedAbstractSyntax.Lambda var typ term)
    show (NoCommonSupertype type1 type2) = "Could not find a common super-type for type '" ++ show type1 ++ "' and type '" ++ show type2 ++ "'"

data TypeError = TypeError String TypeErrorInstance
  deriving (Eq, Ord, Show, Read)

type LinearEnvironment = Set String
type MainEnvironment = Map String Type

data ErrorEnvironment = ErrorEnv {
    linearEnvironment :: LinearEnvironment, 
    currentFunction :: String
  } deriving Show

type Check = ExceptT TypeError (ReaderT MainEnvironment (State ErrorEnvironment))

runTypeChecker :: Program -> Either String Program
runTypeChecker program = undefined

inferTerm :: [Type] -> Term -> Check Type
inferTerm = undefined

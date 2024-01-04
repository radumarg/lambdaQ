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

import Backend.ASTtoIASTConverter (Program, Term, Type, mapProgram)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import GHC.Base (undefined)

data TypeErrorInstance =
    NotAFunction Type             |
    FunctionNotInScope String     |
    TypeMismatch Type Type        |
    NotAProductType Type          | 
    NotALinearType String         |
    NotALinearTerm Term Type      |
    NoCommonSupertype Type Type
    deriving (Eq, Ord, Read)

instance Show TypeErrorInstance where
    show (NotAFunction typ) = "?: " ++ show typ
    show (FunctionNotInScope error) = "?: " ++ error
    show (TypeMismatch type1 type2) = "?: " ++ show type1 ++ show type2
    show (NotAProductType typ) = "?: " ++ show typ
    show (NotALinearType error) = "?: " ++ error
    show (NotALinearTerm type1 type2) = "?: " ++ show type1 ++ show type2
    show (NoCommonSupertype type1 type2) = "?: " ++ show type1 ++ show type2

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

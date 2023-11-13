-- The intermediate abstract syntax tree inspected by a type checker where the type of the 
-- expressions in the program are statically checked to be valid according to the type system: 
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally 
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..
module Backend.TypeChecker (
  TypeError,
  runTypeChecker,
) where

-- import GHC.Err ( errorWithoutStackTrace )
import Control.Monad.Reader ()
import Control.Monad.State ()
import Data.Map (Map)
import Data.Set (Set)

import Backend.ASTtoIASTConverter (Program, Term, Type, mapProgram)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

-- typecheckString :: String -> Either TypeError ()
-- typecheckString = runTypeChecker . parse

runTypeChecker :: Program -> Either String Program
runTypeChecker program = undefined

data TypeErrorInstance =
    NotAFunction Type             |
    FunctionNotInScope String     |
    TypeMismatch Type Type        |
    NotAProductType Type          | 
    NotALinearType String         |
    NotALinearTerm Term Type      |
    NoCommonSupertype Type Type
  deriving (Eq, Ord, Show, Read)

data TypeError = TypeError String TypeErrorInstance
  deriving (Eq, Ord, Show, Read)

type LinearEnvironment = Set String
type TopEnvironment = Map String Type

parse :: String -> Program
parse str = case pProgram (myLexer str) of
   Left str -> errorWithoutStackTrace str
   Right p -> mapProgram p
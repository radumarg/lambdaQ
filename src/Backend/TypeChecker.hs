-- The intermediate abstract syntax tree inspected by a type checker where the type of the 
-- expressions in the program are statically checked to be valid according to the type system: 
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally 
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..
module Backend.TypeChecker where

import Backend.ASTConverter (parse)
import Backend.IAST (Program, Term, Type)
import Data.Map (Map)
import Data.Set (Set)

typecheckProgram :: Program -> Either TypeError ()
typecheckProgram program = undefined

typecheckString :: String -> Either TypeError ()
typecheckString = typecheckProgram . parse

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
-- The intermediate abstract syntax tree inspected by a type checker where the type of the
-- expressions in the program are statically checked to be valid according to the type system:
-- affine intuitionistic linear logic. An afine value can be used at most once but can optionally
-- be discarded (not used at all), see https://arxiv.org/abs/cs/0404056.
-- Type checker will return an annotated syntax tree (??) - to be determined ..
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

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


-- import Backend.ASTtoIASTConverter (Function, Program, Term, Type, Var, mapProgram)
-- import Frontend.LambdaQ.Par ( myLexer, pProgram )
-- import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
-- import GHC.Base (undefined)

data TypeError
  = NotAFunction Type (Int, Int)               -- this type should be a function but it is not
  | FunctionNotInScope String (Int, Int)       -- this variable denotes a function which is not in scope at the point where it is declared
  | TypeMismatch Type Type (Int, Int)          -- this type does not match the type expected at the point where it was declared
  | NotAProductType Type (Int, Int)            -- this type should be a product type but it is not
  | DuplicatedLinearVariable String (Int, Int) -- this linear variable is used more than once
  | NotALinearTerm Type Term (Int, Int)        -- this term is not linear despite being declared linear
  | NoCommonSupertype Type Type                -- these two types have no common supertype
  deriving (Eq, Ord, Read)

instance Show TypeError where
  show (NotAFunction typ (line, col)) = 
    "The type '" ++ show typ ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is not a function type."
  show (FunctionNotInScope var (line, col)) = 
    "The variable " ++ var ++ " at line: " ++ show line ++ " and column: "++ show col ++ " denotes a function which is not in scope."
  show (TypeMismatch type1 type2 (line, col)) = 
    "The expected type '" ++ show type1 ++  "' at line: " ++ show line ++ " and column: "++ show col ++ " cannot be matched with actual type '" ++ show type2 ++ "'"
  show (NotAProductType typ (line, col)) = 
    "The type '" ++ show typ ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is not a product type."
  show (DuplicatedLinearVariable var (line, col)) = 
    "The linear variable '" ++ var ++ "' at line: " ++ show line ++ " and column: "++ show col ++ " is used more than once."
  show (NotALinearTerm typ term (line, col)) = 
    "Expression " ++ show term ++ " at line: " ++ show line ++ " and column: "++ show col ++ " is not a linear type but: " ++ show typ
  show (NoCommonSupertype type1 type2) = 
    "Could not find a common super-type for types '" ++ show type1 ++ " and '" ++ show type2 ++ "'"

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
typeCheckProgram program = mapM_ typeCheckFunction program

typeCheckFunction :: Function -> Check ()
typeCheckFunction (Function functionName (line, col) functionType term) = do
    modify $ \x -> x {currentFunction = functionName}
    inferredType <- inferType [] term
    if inferredType <: functionType
        then return ()
        else throwError (TypeMismatch functionType inferredType (line, col))

inferType :: [Type] -> Term -> Check Type
inferType _ (TermNew _)  = return $ TypeNonLinear (TypeBit :->: TypeQbit)
inferType _ (TermMeasure _)  = return $ TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)
inferType _ (TermBit _)  = return $ TypeNonLinear TypeBit
inferType _ (TermGate gate)  = return $ inferGateType gate
inferType _ TermUnit = return $ TypeNonLinear TypeUnit

inferGateType :: Gate -> Type
inferGateType GateH = undefined

-- Verify it type left is a subtype of type right
(<:) :: Type -> Type -> Bool
a <: b  = a == b    
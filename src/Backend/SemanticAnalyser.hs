
module Backend.SemanticAnalyer where

import Backend.IAST (Program)
import Data.List (intercalate)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import Text.Parsec.Error (errorMessages)

data SemanticError =
    DuplicatedFunctionName String                     |  -- function names must be unique
    MismatchedFunctionDefinitionAndDeclaration String |  -- function signature in declaration has a definition with a matching signature
    IncorrectNumberOfFunctionArguments String         |  -- number of function arguments for a function call does not exceed number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates must be distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates must be distinct
    ControlAndTargetQbitsNotDistinct String           |  -- for a controlled gate the control and target qubits must be distinct
    InvalidBitValue String                            |  -- the value of a bit must be either 0 or 1
    UnknownGate String                                   -- gate names should be recognized as belonging to the set of supported gates

instance Show SemanticError where
    show (DuplicatedFunctionName error) = "Function name is not unique: " ++ error
    show (MismatchedFunctionDefinitionAndDeclaration error) = "Function signature in declaration  does not match the signature in definition: " ++ error
    show (IncorrectNumberOfFunctionArguments error) = "Number of function arguments exceeds the number of arguments in signature: " ++ error
    show (ControlQbitsNotDistinct error) = "The control qubits for controlled gate are not distinct: " ++ error
    show (ControlBitsNotDistinct error) = "The control bits for classical controlled gate are not distinct: " ++ error
    show (ControlAndTargetQbitsNotDistinct error) = "The control and target qubits are not distinct: " ++ error
    show (InvalidBitValue error) = "The bit value should be either 0 or 1: " ++ error
    show (UnknownGate error) = "This gate is not supported: " ++ error

runSemanticAnalysis :: GeneratedAbstractSyntax.Program -> Either String ()
runSemanticAnalysis (GeneratedAbstractSyntax.ProgDef functions) = 
    mapM_ ($ functions) [
        functionNamesAreUnique, 
        functionDeclarationSignaturesMatchDefinitions,
        functionsHaveCorrectNumberOfArguments,
        controlQubitsAreDistinct,
        controlBitsAreDistinct,
        controlAndTargetQubitsAreDistinct,
        bitValuesAreValid,
        gateNamesAreValid
    ]

testSemanticCondition :: [GeneratedAbstractSyntax.FunctionDeclaration] -> (GeneratedAbstractSyntax.FunctionDeclaration -> Bool) -> [String] -> [String]
testSemanticCondition [] _  errorMessages = errorMessages
testSemanticCondition (fun:funs) conditionPredicate  errorMessages = 
  if conditionPredicate fun 
    then 
      testSemanticCondition funs conditionPredicate  errorMessages
    else 
      testSemanticCondition funs conditionPredicate  (newErrorMessage : errorMessages)
    where 
      newErrorMessage = show (DuplicatedFunctionName errorInfo)
      errorInfo = getFunctionInfo fun

testFunctionNames :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testFunctionNames [] errorMessages = errorMessages
testFunctionNames (fun:funs)  errorMessages = 
  if functionNamesAreMatching fun
    then
      testFunctionNames funs errorMessages
    else
      testFunctionNames funs (newErrorMessage : errorMessages)
    where
      newErrorMessage = show (MismatchedFunctionDefinitionAndDeclaration errorInfo)
      errorInfo = getFunctionInfo fun

testNumberOfFunctionArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testNumberOfFunctionArguments [] errorMessages = errorMessages
testNumberOfFunctionArguments (fun:funs)  errorMessages = 
  if toInteger noFunctionArguments > maxTypeArgs
    then
      testFunctionNames funs errorMessages
    else
      testFunctionNames funs (newErrorMessage : errorMessages)
    where
      noFunctionArguments = getNoFunctionArguments funDef
      maxTypeArgs = getMaxTypeArgs tp
      newErrorMessage = show (IncorrectNumberOfFunctionArguments (errorInfo ++ "function has " ++ show noFunctionArguments ++ " but expect as most " ++ show maxTypeArgs))
      errorInfo = getFunctionInfo fun
      getNoFunctionArguments (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = length fargs
      (GeneratedAbstractSyntax.FunDecl (GeneratedAbstractSyntax.FunType var tp) funDef) = fun
      getMaxTypeArgs :: GeneratedAbstractSyntax.Type -> Integer
      getMaxTypeArgs (GeneratedAbstractSyntax.TypeFunction t1 t2) = getMaxTypeArgs t1 + getMaxTypeArgs t2
      getMaxTypeArgs (GeneratedAbstractSyntax.TypeTensorProd t1 t2) = getMaxTypeArgs t1 + getMaxTypeArgs t2
      getMaxTypeArgs (GeneratedAbstractSyntax.TypeSum t1 t2) = getMaxTypeArgs t1 + getMaxTypeArgs t2
      getMaxTypeArgs (GeneratedAbstractSyntax.TypeNonLinear t) = getMaxTypeArgs t
      getMaxTypeArgs (GeneratedAbstractSyntax.TypeExp t i) = getMaxTypeArgs t * i
      getMaxTypeArgs _ = 1


-- test for DuplicatedFunctionName
functionNamesAreUnique :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionNamesAreUnique functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ uniquify $ testSemanticCondition functions conditionPredicate []
    conditionPredicate function = length (filter (== getFunctionName function) functionNames) == 1
    functionNames = map getFunctionName functions

-- test for MismatchedFunctionDefinitionAndDeclaration
functionDeclarationSignaturesMatchDefinitions :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionDeclarationSignaturesMatchDefinitions functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testFunctionNames functions []

-- test for IncorrectNumberOfFunctionArguments
functionsHaveCorrectNumberOfArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionsHaveCorrectNumberOfArguments functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testNumberOfFunctionArguments functions []

-- test for ControlQbitsNotDistinct
controlQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlQubitsAreDistinct function = undefined

-- test for controlBitsNotDistinct
controlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlBitsAreDistinct function = undefined

-- test for ControlAndTargetQbitsNotDistinct
controlAndTargetQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlAndTargetQubitsAreDistinct function = undefined

-- test for InvalidBitValue
bitValuesAreValid :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
bitValuesAreValid function = undefined

-- test for UnknownGate
gateNamesAreValid :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
gateNamesAreValid function = undefined

-- some helper functions

getFunctionName :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunctionName (GeneratedAbstractSyntax.FunDecl _ funDef) = fname
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    ((fline, fcol), fname) = fvar

getFunctionInfo :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunctionInfo (GeneratedAbstractSyntax.FunDecl _ funDef) = 
    "for function: " ++ fname ++ " at line: " ++ show fline ++  " and column: " ++ show fcol
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    ((fline, fcol), fname) = fvar

uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst

functionNamesAreMatching :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
functionNamesAreMatching (GeneratedAbstractSyntax.FunDecl funType funDef) = show fname == functionName
  where
    functionName = getFunctionName (GeneratedAbstractSyntax.FunDecl funType funDef)
    (GeneratedAbstractSyntax.FunType fname ftype) = funType
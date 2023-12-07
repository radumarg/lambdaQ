module Backend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import Data.List (intercalate)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data SemanticError =
    DuplicatedFunctionName String                     |  -- function names must be unique
    MismatchedFunctionDefinitionAndDeclaration String |  -- function name in declaration has a definition with a matching name
    IncorrectNumberOfFunArgs String                   |  -- number of function arguments for a function call does not exceed number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates must be distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates must be distinct
    ControlAndTargetQubitsNotDistinct String          |  -- for a controlled gate the control and target qubits must be distinct
    UnknownGate String                                   -- gate names should be recognized as belonging to the set of supported gates

instance Show SemanticError where
    show (DuplicatedFunctionName error) = "Function name is not unique: " ++ error
    show (MismatchedFunctionDefinitionAndDeclaration error) = "Function name in type definition does not match the function name in declaration: " ++ error
    show (IncorrectNumberOfFunArgs error) = "Number of function arguments exceeds the number of arguments in signature: " ++ error
    show (ControlQbitsNotDistinct error) = "The control qubits for controlled gate are not distinct: " ++ error
    show (ControlBitsNotDistinct error) = "The control bits for classical controlled gate are not distinct: " ++ error
    show (ControlAndTargetQubitsNotDistinct error) = "The control and target qubits are not distinct: " ++ error
    show (UnknownGate error) = "This gate is not supported: " ++ error

runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ funNamesAreUnique functions
    err2 = toString $ functionDeclarationNameMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    -- err4 = toString $ controlQubitsAreDistinct functions
    -- err5 = toString $ controlBitsAreDistinct functions
    -- err6 = toString $ controlAndTargetQubitsAreDistinct functions
    -- err7 = toString $ gateNamesAreValid functions
    err = err1 ++ err2 ++ err3 -- ++ err4 ++ err5 ++ err6 ++ err7
    toString::Either String () -> String
    toString (Left str) = str
    toString (Right ()) = ""

-- test for DuplicatedFunctionName --
funNamesAreUnique :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
funNamesAreUnique functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ reverse $ uniquify $ testForDuplicatedFunNamesAndGetErrors functions conditionPredicate []
    conditionPredicate function = length (filter (== getFunName function) funNames) == 1
    funNames = map getFunName functions

-- test for MismatchedFunctionDefinitionAndDeclaration --
functionDeclarationNameMatchesDefinition :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionDeclarationNameMatchesDefinition functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ reverse $ testFunNamesAndGetErrors functions []

-- test for IncorrectNumberOfFunArgs --
functionsHaveCorrectNumberOfArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionsHaveCorrectNumberOfArguments functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ reverse $ testNumberOfFunArgsAndGetErrors functions []

-- test for UnknownGates --
gateNamesAreValid :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
gateNamesAreValid functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testGateNamesAndGetErrors functions []

-- test for ControlQbitsNotDistinct --
controlQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlQubitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testQubitsAreDistinctAndGetErrors functions []

-- test for controlBitsNotDistinct --
controlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlBitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testBitsAreDistinctAndGetErrors functions []

-- test for ControlAndTargetQubitsNotDistinct --
controlAndTargetQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlAndTargetQubitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ testCtrlAndTgtQubitsAreDistinctAndGetErrors functions []

-- test semantic conditions --

testForDuplicatedFunNamesAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> (GeneratedAbstractSyntax.FunctionDeclaration -> Bool) -> [String] -> [String]
testForDuplicatedFunNamesAndGetErrors [] _  errorMessages = errorMessages
testForDuplicatedFunNamesAndGetErrors (fun:funs) conditionPredicate  errorMessages =
  if conditionPredicate fun
    then
      testForDuplicatedFunNamesAndGetErrors funs conditionPredicate  errorMessages
    else
      testForDuplicatedFunNamesAndGetErrors funs conditionPredicate  (newErrorMessage : errorMessages)
    where
      newErrorMessage = show (DuplicatedFunctionName errorInfo)
      errorInfo = getFunInfo fun

testFunNamesAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testFunNamesAndGetErrors [] errorMessages = errorMessages
testFunNamesAndGetErrors (fun:funs)  errorMessages =
  if funNamesAreMatching fun
    then
      testFunNamesAndGetErrors funs errorMessages
    else
      testFunNamesAndGetErrors funs (newErrorMessage : errorMessages)
    where
      newErrorMessage = show (MismatchedFunctionDefinitionAndDeclaration errorInfo)
      errorInfo = getFunInfo fun

testNumberOfFunArgsAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testNumberOfFunArgsAndGetErrors [] errorMessages = errorMessages
testNumberOfFunArgsAndGetErrors (fun:funs)  errorMessages =
  if toInteger noFunArgs > maxTypeArgs
    then
      testNumberOfFunArgsAndGetErrors funs (newErrorMessage : errorMessages)
    else
      testNumberOfFunArgsAndGetErrors funs errorMessages
    where
      noFunArgs = getNoFunArgs funDef
      maxTypeArgs = getMaxArgs funType
      newErrorMessage = show (IncorrectNumberOfFunArgs (errorInfo ++ ", the function has " ++ show noFunArgs ++ " arguments but expects as most " ++ show maxTypeArgs))
      errorInfo = getFunInfo fun
      (GeneratedAbstractSyntax.FunDecl (GeneratedAbstractSyntax.FunType _ funType) funDef) = fun
      getNoFunArgs (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) fargs _) = length fargs
      getMaxArgs :: GeneratedAbstractSyntax.Type -> Integer
      getMaxArgs typ = getNumberOfTypes typ - 1
      getNumberOfTypes :: GeneratedAbstractSyntax.Type -> Integer
      getNumberOfTypes (GeneratedAbstractSyntax.TypeFunction t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeTensorProd t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeSum t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeExp t i) = getNumberOfTypes t * i
      getNumberOfTypes (GeneratedAbstractSyntax.TypeNonLinear t) = getNumberOfTypes t
      getNumberOfTypes GeneratedAbstractSyntax.TypeBit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeQbit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeState = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeUnitary = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeUnit = 1

testGateNamesAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testGateNamesAndGetErrors [] errorMessages = errorMessages
testGateNamesAndGetErrors (fun:funs)  errorMessages =
  if null unknownGates
    then
      testGateNamesAndGetErrors funs errorMessages
    else
      testGateNamesAndGetErrors funs (newErrorMessage : errorMessages)
    where
      unknownGates = getUnknownGates fun
      newErrorMessage = show (UnknownGate errorInfo) ++ " for gates named: " ++ intercalate ", " unknownGates
      errorInfo = getFunInfo fun

testQubitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testQubitsAreDistinctAndGetErrors [] errorMessages = errorMessages
testQubitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null incorrectQubits
    then
      testQubitsAreDistinctAndGetErrors funs errorMessages
    else
      testQubitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      incorrectQubits = getNotDistinctQubits fun []
      newErrorMessage = show (ControlQbitsNotDistinct errorInfo)
      errorInfo = getFunInfo fun

testBitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testBitsAreDistinctAndGetErrors [] errorMessages = errorMessages
testBitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null incorrectQubits
    then
      testBitsAreDistinctAndGetErrors funs errorMessages
    else
      testBitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      incorrectQubits = getNotDistinctBits fun []
      newErrorMessage = show (ControlBitsNotDistinct errorInfo)
      errorInfo = getFunInfo fun

testCtrlAndTgtQubitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testCtrlAndTgtQubitsAreDistinctAndGetErrors [] errorMessages = errorMessages
testCtrlAndTgtQubitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null duplicatedQubits
    then
      testBitsAreDistinctAndGetErrors funs errorMessages
    else
      testBitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      duplicatedQubits = getDuplicatedCtrlAndTgtQubits fun []
      newErrorMessage = show (ControlAndTargetQubitsNotDistinct errorInfo) ++ " for qubits identified with: " ++ intercalate ", " duplicatedQubits
      errorInfo = getFunInfo fun

-- some helper functions --

getFunName :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunName (GeneratedAbstractSyntax.FunDecl _ funDef) = fname
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    ((fline, fcol), fname) = fvar

getFunInfo :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunInfo (GeneratedAbstractSyntax.FunDecl _ funDef) =
    "for function '" ++ fname ++ "' at line: " ++ show fline ++  " and column: " ++ show fcol
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    ((fline, fcol), fname) = fvar

uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst

funNamesAreMatching :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
funNamesAreMatching (GeneratedAbstractSyntax.FunDecl funType funDef) = varName == functionName
  where
    functionName = getFunName (GeneratedAbstractSyntax.FunDecl funType funDef)
    (GeneratedAbstractSyntax.FunType fname ftype) = funType
    (GeneratedAbstractSyntax.Var ((fline, fcol), varName)) = fname 

getUnknownGates :: GeneratedAbstractSyntax.FunctionDeclaration -> [String]
getUnknownGates (GeneratedAbstractSyntax.FunDecl _ funDef) = collectUnknowns fbody []
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    collectUnknowns :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectUnknowns _ _ = []
    -- TODO: DO I WANT TO CATCH INCORRECTLY NAMED GATES DURING PARSING OR HERE?

getNotDistinctQubits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getNotDistinctQubits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNotDistinct fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    collectNotDistinct :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumCtrlsGate controlTerms _) unknownGates = if distinct then unknownGates else unknownGates ++ [show controlTerms]
      where
        distinct = length termVariables == length (uniquify termVariables)
        (GeneratedAbstractSyntax.CtrlTerms term [terms]) = controlTerms
        termVariables = show term : map show [terms]
    collectNotDistinct (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 [] ++ collectNotDistinct t3 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLambda _ _ _  t) unknownGates = unknownGates ++ collectNotDistinct t []
    collectNotDistinct (GeneratedAbstractSyntax.TermApply t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermDollar t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermCompose t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    -- TODO: ADD TUPLE TERM
    collectNotDistinct _ unknownGates = unknownGates

getNotDistinctBits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getNotDistinctBits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNotDistinct fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    collectNotDistinct :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlsGate controlTerms _) unknownGates = if distinct then unknownGates else unknownGates ++ [show controlTerms]
      where
        distinct = length termVariables == length (uniquify termVariables)
        (GeneratedAbstractSyntax.CtrlTerms term [terms]) = controlTerms
        termVariables = show term : map show [terms]
    collectNotDistinct (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 [] ++ collectNotDistinct t3 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLambda _ _ _  t) unknownGates = unknownGates ++ collectNotDistinct t []
    collectNotDistinct (GeneratedAbstractSyntax.TermApply t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermDollar t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermCompose t1 t2) unknownGates = unknownGates ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    -- TODO: ADD TUPLE TERM
    collectNotDistinct _ unknownGates = unknownGates

getDuplicatedCtrlAndTgtQubits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getDuplicatedCtrlAndTgtQubits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectDuplicated fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
    collectDuplicated :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectDuplicated (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 [] ++ collectDuplicated t3 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLambda _ _ _ t) duplicatedQubits = duplicatedQubits ++ collectDuplicated t []
    collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm ctrlTerm) _) term) duplicatedQubits
      = duplicatedQubits ++ getDuplicated ctrlTerm term
      where
        getDuplicated ctrlTerm term = [show term | show term == show ctrlTerm]
    collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) term) duplicatedQubits
      = duplicatedQubits ++ getDuplicated (ctrlTerm:ctrlTerms) term
      where
        getDuplicated ctrlTerms term = [show term | show term `elem` map show ctrlTerms]
    collectDuplicated (GeneratedAbstractSyntax.TermApply t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermDollar t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermCompose t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated _ duplicatedQubits = duplicatedQubits

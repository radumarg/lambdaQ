module Backend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import Data.List (intercalate)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data SemanticError =
    DuplicatedFunctionName String                     |  -- function name is not unique
    MismatchedFunctionDefinitionAndDeclaration String |  -- function name in function declaration does not have a definition with a matching name
    IncorrectNumberOfFunArgs String                   |  -- number of function arguments for a function exceeds the number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates are not distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates are not distinct
    ControlAndTargetQubitsNotDistinct String          |  -- for a controlled gate the control and target qubits are not distinct
    UnknownGate String                                   -- gate names should be recognized as belonging to the set of supported gates

instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (MismatchedFunctionDefinitionAndDeclaration err) = "Function name in type definition does not match the function name in declaration: " ++ err
    show (IncorrectNumberOfFunArgs err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinct err) = "The control qubits for controlled gate(s) are not distinct: " ++ err
    show (ControlBitsNotDistinct err) = "The control bits for classical controlled gate(s) are not distinct: " ++ err
    show (ControlAndTargetQubitsNotDistinct err) = "The control and target qubits are not distinct: " ++ err
    show (UnknownGate err) = "This gate is not supported: " ++ err

runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ funNamesAreUnique functions
    err2 = toString $ functionDeclarationNameMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    err4 = toString $ controlQubitsAreDistinct functions
    err5 = toString $ controlBitsAreDistinct functions
    err6 = toString $ controlAndTargetQubitsAreDistinct functions
    err7 = toString $ gateNamesAreValid functions
    err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7
    toString::Either String () -> String
    toString (Left str) = str
    toString (Right ()) = ""

-- test for DuplicatedFunctionName --
funNamesAreUnique :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
funNamesAreUnique functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = intercalate ", " $ uniquify $ testForDuplicatedFunNamesAndGetErrors functions conditionPredicate []
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
    allErrors = intercalate ", " $ testNumberOfFunArgsAndGetErrors functions []

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
    allErrors = intercalate ", " $ reverse $ testCtrlAndTgtQubitsAreDistinctAndGetErrors functions []

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
testNumberOfFunArgsAndGetErrors [] errorMessages = reverse errorMessages
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
      getNoFunArgs :: GeneratedAbstractSyntax.FunctionDefinition -> Int
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
      newErrorMessage = show (ControlQbitsNotDistinct errorInfo) ++ " for the following qubits: " ++ format incorrectQubits
      errorInfo = getFunInfo fun

testBitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testBitsAreDistinctAndGetErrors [] errorMessages = errorMessages
testBitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null incorrectBits
    then
      testBitsAreDistinctAndGetErrors funs errorMessages
    else
      testBitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      incorrectBits = getNotDistinctBits fun []
      newErrorMessage = show (ControlBitsNotDistinct errorInfo) ++ " for the following bits: " ++ format incorrectBits
      errorInfo = getFunInfo fun

testCtrlAndTgtQubitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testCtrlAndTgtQubitsAreDistinctAndGetErrors [] errorMessages = errorMessages
testCtrlAndTgtQubitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null duplicatedQubits
    then
      testCtrlAndTgtQubitsAreDistinctAndGetErrors funs errorMessages
    else
      testCtrlAndTgtQubitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      duplicatedQubits =  getDuplicatedCtrlAndTgtQubits fun []
      newErrorMessage = show (ControlAndTargetQubitsNotDistinct errorInfo) ++ " for qubits identified with names: " ++ intercalate ", " duplicatedQubits
      errorInfo = getFunInfo fun

-- some helper functions --

uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst

-- This sucks and is incomplete, I should learn some Haskell
format :: [String] -> String
format items = filter (`notElem` "[]\"") (show (drop 1 items))

getFunName :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunName (GeneratedAbstractSyntax.FunDecl _ funDef) = fname
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) _ _) = funDef
    ((_, _), fname) = fvar

getFunInfo :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunInfo (GeneratedAbstractSyntax.FunDecl _ funDef) =
    "for function '" ++ fname ++ "' at line: " ++ show fline ++  " and column: " ++ show fcol
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) _ _) = funDef
    ((fline, fcol), fname) = fvar

funNamesAreMatching :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
funNamesAreMatching (GeneratedAbstractSyntax.FunDecl funType funDef) = varName == functionName
  where
    functionName = getFunName (GeneratedAbstractSyntax.FunDecl funType funDef)
    (GeneratedAbstractSyntax.FunType fname _) = funType
    (GeneratedAbstractSyntax.Var ((_, _), varName)) = fname 

-- TODO: DO I WANT TO CATCH INCORRECTLY NAMED GATES DURING PARSING OR HERE?
getUnknownGates :: GeneratedAbstractSyntax.FunctionDeclaration -> [String]
getUnknownGates (GeneratedAbstractSyntax.FunDecl _ funDef) = collectUnknowns fbody []
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectUnknowns :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectUnknowns _ _ = []

getNotDistinctQubits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getNotDistinctQubits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNotDistinct fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectNotDistinct :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumCtrlsGate controlTerms _) notDistinctQubits = 
      if distinct then notDistinctQubits else notDistinctQubits ++ [" and "] ++ termVariables
      where
        distinct = length termVariables == length (uniquify termVariables)
        GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
        termVariables = getQubitFromControlTerm term : map getQubitFromControlTerm terms
        getQubitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
        getQubitFromControlTerm term = qubit
          where (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, qubit))) = term
    collectNotDistinct (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 [] ++ collectNotDistinct t3 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLambda _ _ _  t) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t []
    collectNotDistinct (GeneratedAbstractSyntax.TermApply t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermDollar t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermCompose t1 t2) notDistinctQubits = notDistinctQubits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlsGate _ _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermVariable _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermBasisState _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermGate _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermTuple _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermBit _) notDistinctQubits = notDistinctQubits
    collectNotDistinct GeneratedAbstractSyntax.TermUnit notDistinctQubits = notDistinctQubits

getNotDistinctBits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getNotDistinctBits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNotDistinct fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectNotDistinct :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlsGate controlTerms _) notDistinctBits = 
      if distinct then notDistinctBits else notDistinctBits ++ [" and "] ++ termVariables
      where
        distinct = length termVariables == length (uniquify termVariables)
        GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
        termVariables = getBitFromControlTerm term : map getBitFromControlTerm terms
        getBitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
        getBitFromControlTerm term = qubit
          where (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, qubit))) = term
    collectNotDistinct (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 [] ++ collectNotDistinct t3 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermLambda _ _ _  t) notDistinctBits = notDistinctBits ++ collectNotDistinct t []
    collectNotDistinct (GeneratedAbstractSyntax.TermApply t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermDollar t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermCompose t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinct t1 [] ++ collectNotDistinct t2 []
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumCtrlsGate _ _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermVariable _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermBasisState _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermGate _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermTuple _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermBit _) notDistinctBits = notDistinctBits
    collectNotDistinct GeneratedAbstractSyntax.TermUnit notDistinctBits = notDistinctBits

-- TODO multiple qubits gates not supported yet
getDuplicatedCtrlAndTgtQubits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getDuplicatedCtrlAndTgtQubits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectDuplicated fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectDuplicated :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectDuplicated (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 [] ++ collectDuplicated t3 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermLambda _ _ _ t) duplicatedQubits = duplicatedQubits ++ collectDuplicated t []
    collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm ctrlTerm) _) _) term) duplicatedQubits
      = if termQubit == controlQubit then  duplicatedQubits ++ [controlQubit] else duplicatedQubits
      where
        controlQubit = getQubit ctrlTerm
        termQubit = getQubit term
    collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) _) term) duplicatedQubits
      = duplicatedQubits ++ [q | q <- controlQubits, q == termQubit]
      where
        controlQubits = getQubit ctrlTerm : map getQubit ctrlTerms
        termQubit = getQubit term
    collectDuplicated (GeneratedAbstractSyntax.TermApply t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermDollar t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermCompose t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermQuantumCtrlsGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermClassicCtrlsGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermVariable _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermBasisState _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermGate _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermTuple _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermBit _) duplicatedQubits = duplicatedQubits
    collectDuplicated GeneratedAbstractSyntax.TermUnit duplicatedQubits = duplicatedQubits

getQubit ctrlTerm = qubit
  where GeneratedAbstractSyntax.TermVariable ( GeneratedAbstractSyntax.Var (_, qubit)) = ctrlTerm


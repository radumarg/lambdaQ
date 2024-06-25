module Frontend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import Data.List (intercalate)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data SemanticError =
    DuplicatedFunctionName String                     |  -- function name is not uniquely defined
    MismatchedFunctionNameInTypeAndDeclaration String |  -- function name in type declaration does not match function name in definition
    TooManyFunctionArguments String                   |  -- number of function arguments for a function exceeds the number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates are not distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates are not distinct
    ControlAndTargetQubitsNotDistinct String          |  -- for a controlled gate the control and target qubits are not distinct
    UnknownGate String                                |  -- gate names should be recognized as belonging to the set of supported gates
    CaseTermsNotDistinct String                          -- case terms should be distinct

instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (MismatchedFunctionNameInTypeAndDeclaration err) = "Function name in type definition does not match the function name in declaration " ++ err
    show (TooManyFunctionArguments err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinct err) = "The control qubits for controlled gate(s) are not distinct: " ++ err
    show (ControlBitsNotDistinct err) = "The control bits for classical controlled gate(s) are not distinct: " ++ err
    show (ControlAndTargetQubitsNotDistinct err) = "The control and target qubits are not distinct: " ++ err
    show (UnknownGate err) = "This gate is not supported: " ++ err
    show (CaseTermsNotDistinct err) = "Some case terms are duplicated: " ++ err

runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ functionNamesAreUnique functions
    err2 = toString $ functionNameInTypeMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    --err4 = toString $ controlQubitsAreDistinct functions
    --err5 = toString $ controlBitsAreDistinct functions
    --err6 = toString $ controlAndTargetQubitsAreDistinct functions
    --err7 = toString $ gateNamesAreValid functions
    --err8 = toString $ caseTermsAreDistinct functions
    --err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7 + err8
    err = err1 ++ err2 ++ err3
    toString::Either String () -> String
    toString (Left str) = str
    toString (Right ()) = ""

-- test function names are uniquely defined --
functionNamesAreUnique :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionNamesAreUnique functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ getDuplicatedFunctionNames functions predicate []
    predicate function = length (filter (== getFunctionName function) functionNames) == 1
    functionNames = map getFunctionName functions

-- test function name in type declaration matches function name in definition --
functionNameInTypeMatchesDefinition :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionNameInTypeMatchesDefinition functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyFunctionsNames functions []

-- test for TooManyFunctionArguments --
functionsHaveCorrectNumberOfArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionsHaveCorrectNumberOfArguments functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyNumberOfFunctionArguments functions []

-- test for UnknownGates --
gateNamesAreValid :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
gateNamesAreValid functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ testGateNamesAndGetErrors functions []

-- test for ControlQbitsNotDistinct --
controlQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlQubitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ testQubitsAreDistinctAndGetErrors functions []

-- test for controlBitsNotDistinct --
controlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlBitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ testBitsAreDistinctAndGetErrors functions []

-- test for ControlAndTargetQubitsNotDistinct --
controlAndTargetQubitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlAndTargetQubitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ testCtrlAndTgtQubitsAreDistinctAndGetErrors functions []

-- test for UnknownGates --
caseTermsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
caseTermsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ testCaseTermsAndGetErrors functions []

-- test semantic conditions --

getDuplicatedFunctionNames :: [GeneratedAbstractSyntax.FunctionDeclaration] -> (GeneratedAbstractSyntax.FunctionDeclaration -> Bool) -> [String] -> [String]
getDuplicatedFunctionNames [] _  errorMessages = reverse errorMessages
getDuplicatedFunctionNames (fun:funs) predicate errorMessages =
  if predicate fun
    then
      getDuplicatedFunctionNames funs predicate errorMessages
    else
      getDuplicatedFunctionNames funs predicate (newErrorMessage : errorMessages)
    where
      newErrorMessage = "  " ++ show (DuplicatedFunctionName funInfo)
      funInfo = getFunctionNameAndPosition fun

verifyFunctionsNames :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyFunctionsNames [] errorMessages = reverse errorMessages
verifyFunctionsNames (fun:funs)  errorMessages =
  if functionNamesMatch fun
    then
      verifyFunctionsNames funs errorMessages
    else
      verifyFunctionsNames funs (newErrorMessage : errorMessages)
    where
      newErrorMessage = "  " ++ show (MismatchedFunctionNameInTypeAndDeclaration funInfo)
      funInfo = getFunctionNameAndPosition fun

verifyNumberOfFunctionArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyNumberOfFunctionArguments [] errorMessages = reverse errorMessages
verifyNumberOfFunctionArguments (fun:funs)  errorMessages =
  if toInteger noFunctionArgs > maxTypeArgs
    then
      verifyNumberOfFunctionArguments funs (newErrorMessage : errorMessages)
    else
      verifyNumberOfFunctionArguments funs errorMessages
    where
      noFunctionArgs = getNoFunctionArgs funDef
      maxTypeArgs = getMaxArgs funType
      newErrorMessage = "  " ++ show (TooManyFunctionArguments innerErrorMessage)
      innerErrorMessage = funInfo ++ ", the function has " ++ show noFunctionArgs ++ " arguments but expects as most " ++ show maxTypeArgs
      funInfo = getFunctionNameAndPosition fun
      (GeneratedAbstractSyntax.FunDecl (GeneratedAbstractSyntax.FunType _ funType) funDef) = fun
      getNoFunctionArgs :: GeneratedAbstractSyntax.FunctionDefinition -> Int
      getNoFunctionArgs (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) fargs _) = length fargs
      getMaxArgs :: GeneratedAbstractSyntax.Type -> Integer
      getMaxArgs typ = getNumberOfTypes typ - 1
      getNumberOfTypes :: GeneratedAbstractSyntax.Type -> Integer
      getNumberOfTypes (GeneratedAbstractSyntax.TypeFunction t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeSum t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeTensorProd t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeExp t i) = getNumberOfTypes t * i
      getNumberOfTypes (GeneratedAbstractSyntax.TypeNonLinear t) = getNumberOfTypes t
      getNumberOfTypes (GeneratedAbstractSyntax.TypeList _) = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBool = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeInteger = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeQbit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeUnit = 1

testGateNamesAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testGateNamesAndGetErrors [] errorMessages = reverse errorMessages
testGateNamesAndGetErrors (fun:funs)  errorMessages =
  if null unknownGates
    then
      testGateNamesAndGetErrors funs errorMessages
    else
      testGateNamesAndGetErrors funs (newErrorMessage : errorMessages)
    where
      unknownGates = getUnknownGates fun
      newErrorMessage = "  " ++ show (UnknownGate funInfo) ++ " for gates named: " ++ unlines unknownGates
      funInfo = getFunctionNameAndPosition fun

testCaseTermsAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testCaseTermsAndGetErrors [] errorMessages = reverse errorMessages
testCaseTermsAndGetErrors (fun:funs)  errorMessages = undefined

testQubitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testQubitsAreDistinctAndGetErrors [] errorMessages = reverse errorMessages
testQubitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null incorrectQubits
    then
      testQubitsAreDistinctAndGetErrors funs errorMessages
    else
      testQubitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      incorrectQubits = getNotDistinctQubits fun []
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinct funInfo) ++ " for the following qubits: " ++ format incorrectQubits
      funInfo = getFunctionNameAndPosition fun

testBitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testBitsAreDistinctAndGetErrors [] errorMessages = reverse errorMessages
testBitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null incorrectBits
    then
      testBitsAreDistinctAndGetErrors funs errorMessages
    else
      testBitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      incorrectBits = getNotDistinctBits fun []
      newErrorMessage = "  " ++ show (ControlBitsNotDistinct funInfo) ++ " for the following bits: " ++ format incorrectBits
      funInfo = getFunctionNameAndPosition fun

testCtrlAndTgtQubitsAreDistinctAndGetErrors :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
testCtrlAndTgtQubitsAreDistinctAndGetErrors [] errorMessages = reverse errorMessages
testCtrlAndTgtQubitsAreDistinctAndGetErrors (fun:funs) errorMessages =
  if null duplicatedQubits
    then
      testCtrlAndTgtQubitsAreDistinctAndGetErrors funs errorMessages
    else
      testCtrlAndTgtQubitsAreDistinctAndGetErrors funs (newErrorMessage : errorMessages)
    where
      duplicatedQubits =  getDuplicatedCtrlAndTgtQubits fun []
      newErrorMessage = "  " ++ show (ControlAndTargetQubitsNotDistinct funInfo) ++ " for qubits identified with names: " ++ unlines duplicatedQubits
      funInfo = getFunctionNameAndPosition fun

-- some helper functions --

uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst

-- This sucks and is incomplete, I should learn some Haskell
format :: [String] -> String
format items = filter (`notElem` "[]\"") (show (drop 1 items))

getFunctionName :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunctionName (GeneratedAbstractSyntax.FunDecl _ funDef) = fname
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) _ _) = funDef
    ((_, _), fname) = fvar

getFunctionNameAndPosition :: GeneratedAbstractSyntax.FunctionDeclaration -> String
getFunctionNameAndPosition (GeneratedAbstractSyntax.FunDecl _ funDef) =
    "for function: \"" ++ fname ++ "\" at line: " ++ show fline ++  " and column: " ++ show fcol
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) _ _) = funDef
    ((fline, fcol), fname) = fvar

functionNamesMatch :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
functionNamesMatch (GeneratedAbstractSyntax.FunDecl funType funDef) = varName == functionName
  where
    functionName = getFunctionName (GeneratedAbstractSyntax.FunDecl funType funDef)
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
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumTCtrlsGate controlTerms _) notDistinctQubits =
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
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicTCtrlsGate _ _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermVariable _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermBasisState _) notDistinctQubits = notDistinctQubits
    collectNotDistinct (GeneratedAbstractSyntax.TermGate _) notDistinctQubits = notDistinctQubits
    --collectNotDistinct (GeneratedAbstractSyntax.TermTupleOfTerm _) notDistinctQubits = notDistinctQubits
    --collectNotDistinct (GeneratedAbstractSyntax.TermBit _) notDistinctQubits = notDistinctQubits
    collectNotDistinct GeneratedAbstractSyntax.TermUnit notDistinctQubits = notDistinctQubits

getNotDistinctBits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getNotDistinctBits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNotDistinct fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectNotDistinct :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicTCtrlsGate controlTerms _) notDistinctBits =
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
    collectNotDistinct (GeneratedAbstractSyntax.TermQuantumTCtrlsGate _ _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermVariable _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermBasisState _) notDistinctBits = notDistinctBits
    collectNotDistinct (GeneratedAbstractSyntax.TermGate _) notDistinctBits = notDistinctBits
    --collectNotDistinct (GeneratedAbstractSyntax.TermTupleOfTerm _) notDistinctBits = notDistinctBits
    --collectNotDistinct (GeneratedAbstractSyntax.TermBit _) notDistinctBits = notDistinctBits
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
    collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) _) term) duplicatedQubits
      = duplicatedQubits ++ [q | q <- controlQubits, q == termQubit]
      where
        controlQubits = getQubit ctrlTerm : map getQubit ctrlTerms
        termQubit = getQubit term
    collectDuplicated (GeneratedAbstractSyntax.TermApply t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermDollar t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermCompose t1 t2) duplicatedQubits = duplicatedQubits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
    collectDuplicated (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermQuantumTCtrlsGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermClassicTCtrlsGate _ _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermVariable _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermBasisState _) duplicatedQubits = duplicatedQubits
    collectDuplicated (GeneratedAbstractSyntax.TermGate _) duplicatedQubits = duplicatedQubits
    --collectDuplicated (GeneratedAbstractSyntax. _) duplicatedQubits = duplicatedQubits
    --collectDuplicated (GeneratedAbstractSyntax.TermBit _) duplicatedQubits = duplicatedQubits
    collectDuplicated GeneratedAbstractSyntax.TermUnit duplicatedQubits = duplicatedQubits

getQubit ctrlTerm = qubit
  where GeneratedAbstractSyntax.TermVariable ( GeneratedAbstractSyntax.Var (_, qubit)) = ctrlTerm


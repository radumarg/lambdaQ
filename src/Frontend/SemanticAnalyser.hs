module Frontend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import Data.List (intercalate, isPrefixOf)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data SemanticError =
    DuplicatedFunctionName String                     |  -- function name is not uniquely defined
    MismatchedFunctionNameInTypeAndDeclaration String |  -- function name in type declaration does not match function name in definition
    TooManyFunctionArguments String                   |  -- number of function arguments for a function exceeds the number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates are not distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates are not distinct
    ControlAndTargetQbitsNotDistinct String           |  -- for a controlled gate the control and target qubits are not distinct
    UnknownGate String                                |  -- gate names should be recognized as belonging to the set of supported gates
    CaseTermsNotDistinct String                          -- case terms should be distinct


instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (MismatchedFunctionNameInTypeAndDeclaration err) = "Function name in type definition does not match the function name in declaration " ++ err
    show (TooManyFunctionArguments err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinct err) = "The control qubits for controlled gate(s) are not distinct: " ++ err
    show (ControlBitsNotDistinct err) = "The control bits for classical controlled gate(s) are not distinct: " ++ err
    show (ControlAndTargetQbitsNotDistinct err) = "The control and target qubits are not distinct: " ++ err
    show (UnknownGate err) = "This gate is not supported: " ++ err
    show (CaseTermsNotDistinct err) = "Some case terms are duplicated: " ++ err


runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ functionNamesAreUnique functions
    err2 = toString $ functionNameInTypeMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    err4 = toString $ controlQbitsAreDistinct functions
    --err5 = toString $ controlBitsAreDistinct functions
    --err6 = toString $ controlAndTargetQbitsAreDistinct functions
    --err7 = toString $ gateNamesAreValid functions
    --err8 = toString $ caseTermsAreDistinct functions
    --err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7 + err8
    err = err1 ++ err2 ++ err3 ++ err4
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
controlQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlQbitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyControlQbitsAreDistinct functions []


-- test for controlBitsNotDistinct --
controlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlBitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyControlBitsAreDistinct functions []


-- test for ControlAndTargetQbitsNotDistinct --
controlAndTargetQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlAndTargetQbitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyControlAndTargetQbitsAreDistinct functions []


-- test for UnknownGates --
caseTermsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
caseTermsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyCaseTerms functions []


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
      getNumberOfTypes (GeneratedAbstractSyntax.TypeTensorProd t1 t2) = getNumberOfTypes t1 + getNumberOfTypes t2
      getNumberOfTypes (GeneratedAbstractSyntax.TypeExp t i) = getNumberOfTypes t * i
      getNumberOfTypes (GeneratedAbstractSyntax.TypeNonLinear t) = getNumberOfTypes t
      getNumberOfTypes (GeneratedAbstractSyntax.TypeList _) = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBool = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeInteger = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeQbit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeUnit = 1


verifyControlQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyControlQbitsAreDistinct [] errorMessages = reverse errorMessages
verifyControlQbitsAreDistinct (fun:funs) errorMessages =
  if null duplicatedQbits
    then
      verifyControlQbitsAreDistinct funs errorMessages
    else
      verifyControlQbitsAreDistinct funs (newErrorMessage : errorMessages)
    where
      duplicatedQbits = collectNotDistinctQbits fbody ""
      (GeneratedAbstractSyntax.FunDecl _ funDef) = fun
      (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinct funInfo) ++ " for the following qubits: " ++ stripFirstSubstring " and " duplicatedQbits
      funInfo = getFunctionNameAndPosition fun


verifyControlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyControlBitsAreDistinct [] errorMessages = reverse errorMessages
verifyControlBitsAreDistinct (fun:funs) errorMessages =
  if null duplicatedBits
    then
      verifyControlBitsAreDistinct funs errorMessages
    else
      verifyControlBitsAreDistinct funs (newErrorMessage : errorMessages)
    where
      duplicatedBits = collectNotDistinctBits fbody ""
      (GeneratedAbstractSyntax.FunDecl _ funDef) = fun
      (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
      newErrorMessage = "  " ++ show (ControlBitsNotDistinct funInfo) ++ " for the following bits: " ++ stripFirstSubstring " and " duplicatedBits
      funInfo = getFunctionNameAndPosition fun


verifyControlAndTargetQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyControlAndTargetQbitsAreDistinct [] errorMessages = reverse errorMessages
verifyControlAndTargetQbitsAreDistinct (fun:funs) errorMessages =
  if null duplicatedQbits
    then
      verifyControlAndTargetQbitsAreDistinct funs errorMessages
    else
      verifyControlAndTargetQbitsAreDistinct funs (newErrorMessage : errorMessages)
    where
      duplicatedQbits =  getDuplicatedCtrlAndTgtQbits fun []
      newErrorMessage = "  " ++ show (ControlAndTargetQbitsNotDistinct funInfo) ++ " for qubits identified with names: " ++ unlines duplicatedQbits
      funInfo = getFunctionNameAndPosition fun


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


verifyCaseTerms :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyCaseTerms [] errorMessages = reverse errorMessages
verifyCaseTerms (fun:funs)  errorMessages = undefined


-- some helper functions --


uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst


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


getUnknownGates :: GeneratedAbstractSyntax.FunctionDeclaration -> [String]
getUnknownGates (GeneratedAbstractSyntax.FunDecl _ funDef) = collectUnknowns fbody []
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectUnknowns :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    collectUnknowns _ _ = []


collectNotDistinctQbits :: GeneratedAbstractSyntax.Term -> String -> String
-- TermQuantumVCtrlsGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermQuantumVCtrlsGate controlVars _) notDistinctQbits =
  if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables
  where
    distinct = length termVariables == length (uniquify termVariables)
    GeneratedAbstractSyntax.CtrlVars var vars = controlVars
    termVariables = getQbitFromControlVar var : map getQbitFromControlVar vars
    getQbitFromControlVar :: GeneratedAbstractSyntax.Var -> String
    getQbitFromControlVar (GeneratedAbstractSyntax.Var (_, varName)) = varName
-- TermQuantumTCtrlsGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermQuantumTCtrlsGate controlTerms _) notDistinctQbits =
  if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables
  where
    distinct = length termVariables == length (uniquify termVariables)
    GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
    termVariables = filter (not . null) $ getQbitFromControlTerm term : map getQbitFromControlTerm terms
    getQbitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
    getQbitFromControlTerm (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, varName))) = varName
    getQbitFromControlTerm _ = ""
-- TermList
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil) notDistinctQbits = notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term)) notDistinctQbits 
  = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term [])) notDistinctQbits
  = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term1 (term2 : terms))) notDistinctQbits 
  = collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term2 terms)) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons term list)) notDistinctQbits = 
  collectNotDistinctQbits (GeneratedAbstractSyntax.TermList list) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2)) notDistinctQbits =
  collectNotDistinctQbits (GeneratedAbstractSyntax.TermList l2) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits (GeneratedAbstractSyntax.TermList l1) notDistinctQbits
-- TermListElement
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement GeneratedAbstractSyntax.ListNil _) notDistinctQbits = notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListSingle term) _) notDistinctQbits 
  = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListMultiple term []) _) notDistinctQbits 
  = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListMultiple term1 (term2 : terms)) no) notDistinctQbits 
  = collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListMultiple term2 terms) no) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListCons term GeneratedAbstractSyntax.ListNil) _) notDistinctQbits 
  = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListCons term1  (GeneratedAbstractSyntax.ListSingle term2)) _) notDistinctQbits 
  = collectNotDistinctQbits (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term2)) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListCons term1  (GeneratedAbstractSyntax.ListMultiple term2 [])) _) notDistinctQbits 
  = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListCons term1  (GeneratedAbstractSyntax.ListMultiple term2 (term3 : terms))) no) notDistinctQbits 
  = collectNotDistinctQbits (GeneratedAbstractSyntax.TermListElement (GeneratedAbstractSyntax.ListCons term1  (GeneratedAbstractSyntax.ListMultiple term3 terms)) no) notDistinctQbitsTmp 
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term2 notDistinctQbits
-- TermUnit    
collectNotDistinctQbits GeneratedAbstractSyntax.TermUnit notDistinctQbits = notDistinctQbits
-- TermBasisState
collectNotDistinctQbits (GeneratedAbstractSyntax.TermBasisState _) notDistinctQbits = notDistinctQbits
-- TermBoolExpression (TODO: at some point BoolExpressions should support lists)
collectNotDistinctQbits (GeneratedAbstractSyntax.TermBoolExpression _) notDistinctQbits = notDistinctQbits
-- TermIntegerExpression
collectNotDistinctQbits (GeneratedAbstractSyntax.TermIntegerExpression _) notDistinctQbits = notDistinctQbits
-- TermGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermGate _) notDistinctQbits = notDistinctQbits
-- TermVariable
collectNotDistinctQbits (GeneratedAbstractSyntax.TermVariable _) notDistinctQbits = notDistinctQbits
-- TermTupleOfTerms
collectNotDistinctQbits (GeneratedAbstractSyntax.TermTupleOfTerms term []) notDistinctQbits = collectNotDistinctQbits term notDistinctQbits
collectNotDistinctQbits (GeneratedAbstractSyntax.TermTupleOfTerms term1 (term2:terms)) notDistinctQbits = 
  collectNotDistinctQbits (GeneratedAbstractSyntax.TermTupleOfTerms term2 terms) notDistinctQbitsTmp
  where 
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermTupleOfVars
collectNotDistinctQbits (GeneratedAbstractSyntax.TermTupleOfVars _ _) notDistinctQbits = notDistinctQbits
-- TermQuantumCtrlGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctQbits = notDistinctQbits
-- TermClassicCtrlGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctQbits = notDistinctQbits
-- TermClassicVCtrlsGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermClassicVCtrlsGate _ _) notDistinctQbits = notDistinctQbits
-- TermClassicTCtrlsGate
collectNotDistinctQbits (GeneratedAbstractSyntax.TermClassicTCtrlsGate _ _) notDistinctQbits = notDistinctQbits
-- TermVariableList
collectNotDistinctQbits (GeneratedAbstractSyntax.TermVariableList _ _) notDistinctQbits = notDistinctQbits
-- TermApply
collectNotDistinctQbits (GeneratedAbstractSyntax.TermApply term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermCompose
collectNotDistinctQbits (GeneratedAbstractSyntax.TermCompose term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermIfElse
collectNotDistinctQbits (GeneratedAbstractSyntax.TermIfElse term1 term2 term3) notDistinctQbits = collectNotDistinctQbits term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits term2 notDistinctQbitsTmp'
-- TermLetSingle
collectNotDistinctQbits (GeneratedAbstractSyntax.TermLetSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermLetMultiple
collectNotDistinctQbits (GeneratedAbstractSyntax.TermLetMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermLetSugarSingle
collectNotDistinctQbits (GeneratedAbstractSyntax.TermLetSugarSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermLetSugarMultiple
collectNotDistinctQbits (GeneratedAbstractSyntax.TermLetSugarMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits
-- TermCase
collectNotDistinctQbits (GeneratedAbstractSyntax.TermCase term1 [GeneratedAbstractSyntax.CaseExpr term2 term3]) notDistinctQbits = collectNotDistinctQbits term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits term2 notDistinctQbitsTmp'
collectNotDistinctQbits (GeneratedAbstractSyntax.TermCase term1 ((GeneratedAbstractSyntax.CaseExpr term2 term3):caseExpressions)) notDistinctQbits = collectNotDistinctQbits (GeneratedAbstractSyntax.TermCase term1 caseExpressions) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits term2 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits term3 notDistinctQbitsTmp'
-- TermLambda
collectNotDistinctQbits (GeneratedAbstractSyntax.TermLambda _ _ _ term) notDistinctQbits = collectNotDistinctQbits term notDistinctQbits
-- TermDollar
collectNotDistinctQbits (GeneratedAbstractSyntax.TermDollar term1 term2) notDistinctQbits = collectNotDistinctQbits term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits term1 notDistinctQbits

--collectNotDistinctQbits _ _ = undefined





collectNotDistinctBits :: GeneratedAbstractSyntax.Term -> String -> String
collectNotDistinctBits = undefined
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermClassicTCtrlsGate controlTerms _) notDistinctBits =
--   if distinct then notDistinctBits else notDistinctBits ++ " and " ++ intercalate ", " termVariables
--   where
--     distinct = length termVariables == length (uniquify termVariables)
--     GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
--     termVariables = getBitFromControlTerm term : map getBitFromControlTerm terms
--     getBitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
--     getBitFromControlTerm term = qubit
--       where (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, qubit))) = term
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 [] ++ collectNotDistinctBits t3 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermLambda _ _ _  t) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermApply t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermDollar t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermCompose t1 t2) notDistinctBits = notDistinctBits ++ collectNotDistinctBits t1 [] ++ collectNotDistinctBits t2 []
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermQuantumTCtrlsGate _ _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermVariable _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermBasisState _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits (GeneratedAbstractSyntax.TermGate _) notDistinctBits = notDistinctBits
-- --collectNotDistinctBits (GeneratedAbstractSyntax.TermTupleOfTerm _) notDistinctBits = notDistinctBits
-- --collectNotDistinctBits (GeneratedAbstractSyntax.TermBit _) notDistinctBits = notDistinctBits
-- collectNotDistinctBits GeneratedAbstractSyntax.TermUnit notDistinctBits = notDistinctBits


-- TODO multiple qubits gates not supported yet
getDuplicatedCtrlAndTgtQbits :: GeneratedAbstractSyntax.FunctionDeclaration -> [String] -> [String]
getDuplicatedCtrlAndTgtQbits = undefined
-- getDuplicatedCtrlAndTgtQbits (GeneratedAbstractSyntax.FunDecl _ funDef) = collectDuplicated fbody
--   where
--     (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
--     collectDuplicated :: GeneratedAbstractSyntax.Term -> [String] -> [String]
--     collectDuplicated (GeneratedAbstractSyntax.TermIfElse t1 t2 t3) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 [] ++ collectDuplicated t3 []
--     collectDuplicated (GeneratedAbstractSyntax.TermLetSingle _ t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermLambda _ _ _ t) duplicatedQbits = duplicatedQbits ++ collectDuplicated t []
--     collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm ctrlTerm) _) _) term) duplicatedQbits
--       = if termQbit == controlQbit then  duplicatedQbits ++ [controlQbit] else duplicatedQbits
--       where
--         controlQbit = getQbit ctrlTerm
--         termQbit = getQbit term
--     collectDuplicated (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) _) term) duplicatedQbits
--       = duplicatedQbits ++ [q | q <- controlQbits, q == termQbit]
--       where
--         controlQbits = getQbit ctrlTerm : map getQbit ctrlTerms
--         termQbit = getQbit term
--     collectDuplicated (GeneratedAbstractSyntax.TermApply t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermDollar t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermCompose t1 t2) duplicatedQbits = duplicatedQbits ++ collectDuplicated t1 [] ++ collectDuplicated t2 []
--     collectDuplicated (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermQuantumTCtrlsGate _ _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermClassicTCtrlsGate _ _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermVariable _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermBasisState _) duplicatedQbits = duplicatedQbits
--     collectDuplicated (GeneratedAbstractSyntax.TermGate _) duplicatedQbits = duplicatedQbits
--     --collectDuplicated (GeneratedAbstractSyntax. _) duplicatedQbits = duplicatedQbits
--     --collectDuplicated (GeneratedAbstractSyntax.TermBit _) duplicatedQbits = duplicatedQbits
--     collectDuplicated GeneratedAbstractSyntax.TermUnit duplicatedQbits = duplicatedQbits


getQbit ctrlTerm = qubit
  where GeneratedAbstractSyntax.TermVariable ( GeneratedAbstractSyntax.Var (_, qubit)) = ctrlTerm

stripFirstSubstring :: String -> String -> String
stripFirstSubstring _ "" = ""
stripFirstSubstring substr str@(x:xs)
  | substr `isPrefixOf` str = drop (length substr) str
  | otherwise               = x : stripFirstSubstring substr xs

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
    ControlQbitsNotDistinctQuantumCtrlsGates String   |  -- control qubits for controlled gates are not distinct
    ControlQbitsNotDistinctClassicCtrlsGates String   |  -- control bits for classically controlled gates are not distinct
    ControlAndTargetQbitsNotDistinct String           |  -- for a controlled gate the control and target qubits are not distinct
    UnknownGate String                                |  -- gate names should be recognized as belonging to the set of supported gates
    CaseTermsNotDistinct String                          -- case terms should be distinct


instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (MismatchedFunctionNameInTypeAndDeclaration err) = "Function name in type definition does not match the function name in declaration " ++ err
    show (TooManyFunctionArguments err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinctQuantumCtrlsGates err) = "The control qubits for controlled gate(s) are not distinct " ++ err
    show (ControlQbitsNotDistinctClassicCtrlsGates err) = "The control qubits for classical controlled gate(s) are not distinct " ++ err
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
    err5 = toString $ controlBitsAreDistinct functions
    --err6 = toString $ controlAndTargetQbitsAreDistinct functions
    --err7 = toString $ gateNamesAreValid functions
    --err8 = toString $ caseTermsAreDistinct functions
    --err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7 + err8
    err = err1 ++ err2 ++ err3 ++ err4 ++ err5
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
    allErrors = unlines $ verifyQuantumCtrlsQbitsAreDistinct functions []


-- test for controlBitsNotDistinct --
controlBitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
controlBitsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyClassicCtrlsQbitsAreDistinct functions []


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


verifyQuantumCtrlsQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyQuantumCtrlsQbitsAreDistinct [] errorMessages = reverse errorMessages
verifyQuantumCtrlsQbitsAreDistinct (fun:funs) errorMessages =
  if null duplicatedQbits
    then
      verifyQuantumCtrlsQbitsAreDistinct funs errorMessages
    else
      verifyQuantumCtrlsQbitsAreDistinct funs (newErrorMessage : errorMessages)
    where
      duplicatedQbits = collectNotDistinctQbits "quantum-ctrl-gates" fbody ""
      (GeneratedAbstractSyntax.FunDecl _ funDef) = fun
      (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinctQuantumCtrlsGates funInfo) ++ " for the following qubits: " ++ stripFirstSubstring " and " duplicatedQbits
      funInfo = getFunctionNameAndPosition fun


verifyClassicCtrlsQbitsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyClassicCtrlsQbitsAreDistinct [] errorMessages = reverse errorMessages
verifyClassicCtrlsQbitsAreDistinct (fun:funs) errorMessages =
  if null duplicatedQubits
    then
      verifyClassicCtrlsQbitsAreDistinct funs errorMessages
    else
      verifyClassicCtrlsQbitsAreDistinct funs (newErrorMessage : errorMessages)
    where
      duplicatedQubits = collectNotDistinctQbits "classic-ctrl-gates" fbody ""
      (GeneratedAbstractSyntax.FunDecl _ funDef) = fun
      (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinctClassicCtrlsGates funInfo) ++ " for the following qubits: " ++ stripFirstSubstring " and " duplicatedQubits
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
    "for function \"" ++ fname ++ "\" at line: " ++ show fline ++  " and column: " ++ show fcol
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


collectNotDistinctQbits :: String -> GeneratedAbstractSyntax.Term ->  String -> String
-- TermQuantumVCtrlsGate
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermQuantumVCtrlsGate controlVars _) notDistinctQbits =
  if mode == "quantum-ctrl-gates" then
    (if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables)
  else notDistinctQbits
    where
      distinct = length termVariables == length (uniquify termVariables)
      GeneratedAbstractSyntax.CtrlVars var vars = controlVars
      termVariables = getQbitFromControlVar var : map getQbitFromControlVar vars
      getQbitFromControlVar :: GeneratedAbstractSyntax.Var -> String
      getQbitFromControlVar (GeneratedAbstractSyntax.Var (_, varName)) = varName
-- TermQuantumTCtrlsGate
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermQuantumTCtrlsGate controlTerms _) notDistinctQbits =
  if mode == "quantum-ctrl-gates" then
    (if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables)
  else notDistinctQbits
    where
      distinct = length termVariables == length (uniquify termVariables)
      GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
      termVariables = filter (not . null) $ getQbitFromControlTerm term : map getQbitFromControlTerm terms
      getQbitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
      getQbitFromControlTerm (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, varName))) = varName
      getQbitFromControlTerm _ = ""
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermClassicVCtrlsGate controlVars _) notDistinctQbits =
  if mode == "classic-ctrl-gates" then
    (if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables)
  else notDistinctQbits
    where
      distinct = length termVariables == length (uniquify termVariables)
      GeneratedAbstractSyntax.CtrlVars var vars = controlVars
      termVariables = getBitFromControlVar var : map getBitFromControlVar vars
      getBitFromControlVar :: GeneratedAbstractSyntax.Var -> String
      getBitFromControlVar (GeneratedAbstractSyntax.Var (_, varName)) = varName
-- TermClassicTCtrlsGate
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermClassicTCtrlsGate controlTerms _) notDistinctQbits =
  if mode == "classic-ctrl-gates" then
    (if distinct then notDistinctQbits else notDistinctQbits ++ " and " ++ intercalate ", " termVariables)
  else notDistinctQbits
    where
      distinct = length termVariables == length (uniquify termVariables)
      GeneratedAbstractSyntax.CtrlTerms term terms = controlTerms
      termVariables = filter (not . null) $ getBitFromControlTerm term : map getBitFromControlTerm terms
      getBitFromControlTerm :: GeneratedAbstractSyntax.Term -> String
      getBitFromControlTerm (GeneratedAbstractSyntax.TermVariable (GeneratedAbstractSyntax.Var (_, varName))) = varName
      getBitFromControlTerm _ = ""
-- TermList
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil) notDistinctQbits = notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term)) notDistinctQbits 
  = collectNotDistinctQbits mode term notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term [])) notDistinctQbits
  = collectNotDistinctQbits mode term notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term1 (term2 : terms))) notDistinctQbits 
  = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term2 terms)) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons term list)) notDistinctQbits = 
  collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList list) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2)) notDistinctQbits =
  collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList l2) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList l1) notDistinctQbits
-- TermListElement
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermListElement l _) notDistinctQbits = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList l) notDistinctQbits
-- TermUnit    
collectNotDistinctQbits _ GeneratedAbstractSyntax.TermUnit notDistinctQbits = notDistinctQbits
-- TermBasisState
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermBasisState _) notDistinctQbits = notDistinctQbits
-- TermBoolExpression (TODO: at some point BoolExpressions should support lists)
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermBoolExpression _) notDistinctQbits = notDistinctQbits
-- TermIntegerExpression
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermIntegerExpression _) notDistinctQbits = notDistinctQbits
-- TermGate
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermGate _) notDistinctQbits = notDistinctQbits
-- TermVariable
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermVariable _) notDistinctQbits = notDistinctQbits
-- TermTupleOfTerms
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTupleOfTerms term []) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTupleOfTerms term1 (term2:terms)) notDistinctQbits = 
  collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTupleOfTerms term2 terms) notDistinctQbitsTmp
  where 
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermTupleOfVars
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermTupleOfVars _ _) notDistinctQbits = notDistinctQbits
-- TermQuantumCtrlGate
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctQbits = notDistinctQbits
-- TermClassicCtrlGate
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctQbits = notDistinctQbits
-- TermVariableList
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermVariableList _ _) notDistinctQbits = notDistinctQbits
-- TermApply
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermApply term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermCompose
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCompose term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermTensorProduct
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTensorProduct term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermIfElse
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermIfElse term1 term2 term3) notDistinctQbits = collectNotDistinctQbits mode term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term2 notDistinctQbitsTmp'
-- TermLetSingle
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermLetMultiple
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermLetSugarSingle
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSugarSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermLetSugarMultiple
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSugarMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermLambda
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLambda _ _ _ term) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits
-- TermDollar
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermDollar term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits
-- TermCase
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term []) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 [GeneratedAbstractSyntax.CaseExpr term2 term3]) notDistinctQbits = collectNotDistinctQbits mode term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term2 notDistinctQbitsTmp'
collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 ((GeneratedAbstractSyntax.CaseExpr term2 term3):caseExpressions)) notDistinctQbits 
  = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 caseExpressions) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term2 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term3 notDistinctQbitsTmp'


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

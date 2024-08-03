module Frontend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import Data.List (intercalate, isPrefixOf)
import Data.Set (toList, fromList)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data SemanticError =
    DuplicatedFunctionName String                       |  -- function name is not uniquely defined
    MismatchedFunctionNameInTypeAndDeclaration String   |  -- function name in type declaration does not match function name in definition
    TooManyFunctionArguments String                     |  -- number of function arguments for a function exceeds the number of arguments in signature
    ControlQbitsNotDistinctQCtrlGates String            |  -- control qubits for controlled gates are not distinct
    ControlQbitsNotDistinctCCtrlGates String            |  -- control bits for classically controlled gates are not distinct
    CtrlAndTgtQubitsNotDistinctQCtrlGates String        |  -- for a quantum controlled gate the control and target qubits are not distinct
    CtrlAndTgtQubitsNotDistinctCCtrlGates String        |  -- for a classicallly controlled gate the control and target qubits are not distinct
    UnknownGate String                                  |  -- gate names should be recognized as belonging to the set of supported gates
    CaseTermsNotDistinct String                            -- case terms should be distinct


instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (MismatchedFunctionNameInTypeAndDeclaration err) = "Function name in type definition does not match the function name in declaration, " ++ err
    show (TooManyFunctionArguments err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinctQCtrlGates err) = "The control qubits for some quantum controlled gate(s) are not distinct, " ++ err
    show (ControlQbitsNotDistinctCCtrlGates err) = "The control qubits for some classically controlled gate(s) are not distinct, " ++ err
    show (CtrlAndTgtQubitsNotDistinctQCtrlGates err) = "For some quantum controlled gate(s) the control and target qubits are not distinct, " ++ err
    show (CtrlAndTgtQubitsNotDistinctCCtrlGates err) = "For some classically controlled gate(s) the control and target qubits are not distinct, " ++ err
    show (UnknownGate err) = "Detected gate(s) which are not supported " ++ err
    show (CaseTermsNotDistinct err) = "Some case terms are duplicated: " ++ err


runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ functionNamesAreUnique functions
    err2 = toString $ functionNameInTypeMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    err4 = toString $ ctrlQbitsAreDistinctQuantumCtrlGates functions
    err5 = toString $ ctrlQbitsAreDistinctClassicCtrlGates functions
    err6 = toString $ ctrlAndTgtQubitsAreDistinctQuantumCtrlGates functions
    err7 = toString $ ctrlAndTgtQubitsAreDistinctClassicCtrlGates functions
    err8 = toString $ gateNamesAreValid functions
    err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7 ++ err8
    toString::Either String () -> String
    toString (Left str) = str
    toString (Right ()) = ""


functionNamesAreUnique :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionNamesAreUnique functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ getDuplicatedFunctionNames functions predicate []
    predicate function = length (filter (== getFunctionName function) functionNames) == 1
    functionNames = map getFunctionName functions


functionNameInTypeMatchesDefinition :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionNameInTypeMatchesDefinition functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyFunctionsNames functions []


functionsHaveCorrectNumberOfArguments :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
functionsHaveCorrectNumberOfArguments functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyNumberOfFunctionArguments functions []


gateNamesAreValid :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
gateNamesAreValid functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyGatesNames functions []


ctrlQbitsAreDistinctQuantumCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
ctrlQbitsAreDistinctQuantumCtrlGates functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyQuantumCtrlsQbitsAreDistinct functions []


ctrlQbitsAreDistinctClassicCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
ctrlQbitsAreDistinctClassicCtrlGates functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyClassicCtrlsQbitsAreDistinct functions []


ctrlAndTgtQubitsAreDistinctQuantumCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
ctrlAndTgtQubitsAreDistinctQuantumCtrlGates functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyControlAndTargetQbitsAreDistinct "quantum-ctrl-gates" functions []

ctrlAndTgtQubitsAreDistinctClassicCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
ctrlAndTgtQubitsAreDistinctClassicCtrlGates functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyControlAndTargetQbitsAreDistinct "classic-ctrl-gates" functions []


caseTermsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
caseTermsAreDistinct functions = if null allErrors then Right () else Left allErrors
  where
    allErrors = unlines $ verifyCaseTerms functions []


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
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinctQCtrlGates funInfo) ++ " for the following qubit sequence(s): " ++ stripFirstSubstring " and " duplicatedQbits
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
      newErrorMessage = "  " ++ show (ControlQbitsNotDistinctCCtrlGates funInfo) ++ " for the following qubit sequence(s): " ++ stripFirstSubstring " and " duplicatedQubits
      funInfo = getFunctionNameAndPosition fun


verifyControlAndTargetQbitsAreDistinct :: String -> [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyControlAndTargetQbitsAreDistinct _ [] errorMessages = reverse errorMessages
verifyControlAndTargetQbitsAreDistinct mode (fun:funs) errorMessages =
  if null notDistinctQubits
    then
      verifyControlAndTargetQbitsAreDistinct mode funs errorMessages
    else
      verifyControlAndTargetQbitsAreDistinct mode funs (newErrorMessage : errorMessages)
    where
      notDistinctQubits = collectDuplicatedTgtAndCtrl mode fbody ""
      (GeneratedAbstractSyntax.FunDecl _ funDef) = fun
      (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
      intro = if mode == "quantum-ctrl-gates" then show (CtrlAndTgtQubitsNotDistinctQCtrlGates funInfo) else  show (CtrlAndTgtQubitsNotDistinctCCtrlGates funInfo)
      newErrorMessage = "  " ++ intro ++ " for qubit(s) identified with variable name(s): " ++ stripFirstSubstring " and " notDistinctQubits
      funInfo = getFunctionNameAndPosition fun

verifyGatesNames :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyGatesNames [] errorMessages = reverse errorMessages
verifyGatesNames (fun:funs)  errorMessages =
  if null unknownGates
    then
      verifyGatesNames funs errorMessages
    else
      verifyGatesNames funs (newErrorMessage : errorMessages)
    where
      unknownGates = getUnknownGates fun
      newErrorMessage = "  " ++ show (UnknownGate funInfo) ++ " for gate(s) named: " ++ intercalate ", " unknownGates
      funInfo = getFunctionNameAndPosition fun


verifyCaseTerms :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyCaseTerms [] errorMessages = reverse errorMessages
verifyCaseTerms (fun:funs)  errorMessages = undefined


functionNamesMatch :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
functionNamesMatch (GeneratedAbstractSyntax.FunDecl funType funDef) = varName == functionName
  where
    functionName = getFunctionName (GeneratedAbstractSyntax.FunDecl funType funDef)
    (GeneratedAbstractSyntax.FunType fname _) = funType
    (GeneratedAbstractSyntax.Var ((_, _), varName)) = fname


collectNotDistinctQbits :: String -> GeneratedAbstractSyntax.Term ->  String -> String
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

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermListElement l _) notDistinctQbits = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermList l) notDistinctQbits
   
collectNotDistinctQbits _ GeneratedAbstractSyntax.TermUnit notDistinctQbits = notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermBasisState _) notDistinctQbits = notDistinctQbits

-- (TODO: at some point BoolExpressions should support lists)
collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermBoolExpression _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermIntegerExpression _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermGate _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermVariable _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTuple term []) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTuple term1 (term2:terms)) notDistinctQbits = 
  collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTuple term2 terms) notDistinctQbitsTmp
  where 
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits _ (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) notDistinctQbits = notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermApply term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCompose term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermTensorProduct term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermIfElse term1 term2 term3) notDistinctQbits = collectNotDistinctQbits mode term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term2 notDistinctQbitsTmp'

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSugarSingle _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLetSugarMultiple _  _ term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermLambda _ _ _ term) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermDollar term1 term2) notDistinctQbits = collectNotDistinctQbits mode term2 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp = collectNotDistinctQbits mode term1 notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term []) notDistinctQbits = collectNotDistinctQbits mode term notDistinctQbits

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 [GeneratedAbstractSyntax.CaseExpr term2 term3]) notDistinctQbits 
  = collectNotDistinctQbits mode term3 notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term1 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term2 notDistinctQbitsTmp'

collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 ((GeneratedAbstractSyntax.CaseExpr term2 term3):caseExpressions)) notDistinctQbits 
  = collectNotDistinctQbits mode (GeneratedAbstractSyntax.TermCase term1 caseExpressions) notDistinctQbitsTmp
  where
    notDistinctQbitsTmp' = collectNotDistinctQbits mode term2 notDistinctQbits
    notDistinctQbitsTmp = collectNotDistinctQbits mode term3 notDistinctQbitsTmp'


-- TODO multiple qubits gates not supported yet
collectDuplicatedTgtAndCtrl :: String -> GeneratedAbstractSyntax.Term -> String -> String
collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm ctrlTerm) _) _) term) duplicatedQubits =
  if (mode == "quantum-ctrl-gates") && not (null termQbit) && (termQbit == controlQbit) then
    duplicatedQubits ++ " and " ++ controlQbit
  else duplicatedQubits
      where
      controlQbit = getTermVariableName ctrlTerm
      termQbit = getTermVariableName term

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermClassicCtrlGate (GeneratedAbstractSyntax.CtrlTerm ctrlTerm) _) _) term) duplicatedQubits =
  if (mode == "classic-ctrl-gates") && not (null termQbit) && (termQbit == controlQbit) then
    duplicatedQubits ++ " and " ++ controlQbit
  else duplicatedQubits
      where
      controlQbit = getTermVariableName ctrlTerm
      termQbit = getTermVariableName term

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumVCtrlsGate (GeneratedAbstractSyntax.CtrlVars ctrlVar ctrlVars) _) _) term) duplicatedQbits =
  if (mode == "quantum-ctrl-gates") && not (null termQbit) && (termQbit `elem` controlQbits) then
    duplicatedQbits ++ " and " ++ unwords [q | q <- controlQbits, q == termQbit]
   else duplicatedQbits
      where
        controlQbits = getVariableName ctrlVar : map getVariableName ctrlVars
        termQbit = getTermVariableName term

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermClassicVCtrlsGate (GeneratedAbstractSyntax.CtrlVars ctrlVar ctrlVars) _) _) term) duplicatedQbits =
  if (mode == "classic-ctrl-gates") && not (null termQbit) && (termQbit `elem` controlQbits) then
    duplicatedQbits ++ " and " ++ unwords [q | q <- controlQbits, q == termQbit]
   else duplicatedQbits
      where
        controlQbits = getVariableName ctrlVar : map getVariableName ctrlVars
        termQbit = getTermVariableName term

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) _) term) duplicatedQbits =
  if (mode == "quantum-ctrl-gates") then -- && not (null termQbit) && (termQbit `elem` controlQbits) then
    duplicatedQbits ++ " and " ++ show ctrlTerms  -- ++ unwords [q | q <- controlQbits, q == termQbit]
   else duplicatedQbits
      where
        controlQbits = getTermVariableName ctrlTerm : map getTermVariableName ctrlTerms
        termQbit = getTermVariableName term

-- collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumVCtrlsGate (GeneratedAbstractSyntax.CtrlVars ctrlVar ctrlVars) _) _) term) duplicatedQbits =
--   if (mode == "quantum-ctrl-gates") && not (null termQbit) && (termQbit `elem` controlQbits) then
--     duplicatedQbits ++ " and " ++ unwords [q | q <- controlQbits, q == termQbit]
--    else duplicatedQbits
--       where
--         controlQbits = getVariableName ctrlVar : map getVariableName ctrlVars
--         termQbit = getTermVariableName term

-- collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermApply (GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms ctrlTerm ctrlTerms) _) _) term) duplicatedQbits =
--   if (mode == "quantum-ctrl-gates") && not (null termQbit) && (termQbit `elem` controlQbits) then
--     duplicatedQbits ++  intercalate " " controlQbits -- ++ " and " ++ intercalate " "  [q | q <- controlQbits, q == termQbit]
--    else duplicatedQbits
--       where
--         controlQbits = getVariableName ctrlTerm : map getVariableName ctrlTerms
--         termQbit = getVariableName term

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermQuantumCtrlGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermClassicCtrlGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermQuantumVCtrlsGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermQuantumTCtrlsGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermClassicVCtrlsGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermClassicTCtrlsGate _ _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term)) duplicatedQubits 
  = collectDuplicatedTgtAndCtrl mode term duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term [])) duplicatedQubits
  = collectDuplicatedTgtAndCtrl mode term duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term1 (term2 : terms))) duplicatedQubits 
  = collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term2 terms)) duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons term list)) duplicatedQubits = 
  collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList list) duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2)) duplicatedQubits =
  collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList l2) duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList l1) duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermListElement l _) duplicatedQubits = collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermList l) duplicatedQubits
  
collectDuplicatedTgtAndCtrl _ GeneratedAbstractSyntax.TermUnit duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermBasisState _) duplicatedQubits = duplicatedQubits

-- (TODO: at some point BoolExpressions should support lists)
collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermBoolExpression _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermIntegerExpression _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermGate _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl _ (GeneratedAbstractSyntax.TermVariable _) duplicatedQubits = duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermTuple term []) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term duplicatedQubits
collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermTuple term1 (term2:terms)) duplicatedQubits 
  = collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermTuple term2 terms) duplicatedQubitsTmp
  where 
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermApply term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermCompose term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermTensorProduct term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermIfElse term1 term2 term3) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term3 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp' = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp'

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermLetSingle _ term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermLetMultiple _  _ term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermLetSugarSingle _ term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermLetSugarMultiple _  _ term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermLambda _ _ _ term) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermDollar term1 term2) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermCase term []) duplicatedQubits = collectDuplicatedTgtAndCtrl mode term duplicatedQubits
collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermCase term1 [GeneratedAbstractSyntax.CaseExpr term2 term3]) duplicatedQubits 
  = collectDuplicatedTgtAndCtrl mode term3 duplicatedQubitsTmp
  where
    duplicatedQubitsTmp' = collectDuplicatedTgtAndCtrl mode term1 duplicatedQubits
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubitsTmp'

collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermCase term1 ((GeneratedAbstractSyntax.CaseExpr term2 term3):caseExpressions)) duplicatedQubits 
  = collectDuplicatedTgtAndCtrl mode (GeneratedAbstractSyntax.TermCase term1 caseExpressions) duplicatedQubitsTmp
  where
    duplicatedQubitsTmp' = collectDuplicatedTgtAndCtrl mode term2 duplicatedQubits
    duplicatedQubitsTmp = collectDuplicatedTgtAndCtrl mode term3 duplicatedQubitsTmp'


getUnknownGates :: GeneratedAbstractSyntax.FunctionDeclaration -> [String]
getUnknownGates (GeneratedAbstractSyntax.FunDecl _ funDef) = collectUnknowns fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectUnknowns :: GeneratedAbstractSyntax.Term -> [String]
    collectUnknowns (GeneratedAbstractSyntax.TermListElement l _) = collectUnknowns (GeneratedAbstractSyntax.TermList l)
    collectUnknowns (GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil) = []
    collectUnknowns (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle term)) = collectUnknowns term
    collectUnknowns (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple term terms)) =
      collectUnknowns term ++ concatMap collectUnknowns terms
    collectUnknowns (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2)) =
      collectUnknowns (GeneratedAbstractSyntax.TermList l1) ++ collectUnknowns (GeneratedAbstractSyntax.TermList l2)
    collectUnknowns (GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons term list)) =
      collectUnknowns term ++ collectUnknowns (GeneratedAbstractSyntax.TermList list)
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknownSimple gateVar)) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknownInt gateVar _)) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknownVar gateVar _)) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknown1Angle gateVar _ )) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknown2Angle gateVar _ _ )) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermGate (GeneratedAbstractSyntax.GateUnknown3Angle gateVar _ _ _)) = [getGateName gateVar]
    collectUnknowns (GeneratedAbstractSyntax.TermTuple term terms) = collectUnknowns term ++ concatMap collectUnknowns terms
    collectUnknowns (GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm term) _) = collectUnknowns term
    collectUnknowns (GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms term terms) _) =
      collectUnknowns term ++ concatMap collectUnknowns terms
    collectUnknowns (GeneratedAbstractSyntax.TermClassicCtrlGate (GeneratedAbstractSyntax.CtrlTerm term) _) = collectUnknowns term
    collectUnknowns (GeneratedAbstractSyntax.TermClassicTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms term terms) _) =
      collectUnknowns term ++ concatMap collectUnknowns terms
    collectUnknowns (GeneratedAbstractSyntax.TermApply term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermCompose term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermTensorProduct term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermIfElse term1 term2 term3) = collectUnknowns term1 ++ collectUnknowns term2 ++ collectUnknowns term3
    collectUnknowns (GeneratedAbstractSyntax.TermLetSingle _ term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermLetMultiple _ _ term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermLetSugarSingle _ term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermLetSugarMultiple _ _ term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns (GeneratedAbstractSyntax.TermCase term []) = collectUnknowns term
    collectUnknowns (GeneratedAbstractSyntax.TermCase term1 [GeneratedAbstractSyntax.CaseExpr term2 term3]) =
      collectUnknowns term1 ++ collectUnknowns term2 ++ collectUnknowns term3
    collectUnknowns (GeneratedAbstractSyntax.TermCase term1 ((GeneratedAbstractSyntax.CaseExpr term2 term3):caseExpressions)) =
      collectUnknowns term2 ++ collectUnknowns term3 ++ collectUnknowns (GeneratedAbstractSyntax.TermCase term1 caseExpressions)
    collectUnknowns (GeneratedAbstractSyntax.TermLambda _ _ _ term) = collectUnknowns term
    collectUnknowns (GeneratedAbstractSyntax.TermDollar term1 term2) = collectUnknowns term1 ++ collectUnknowns term2
    collectUnknowns _ = []


-- Some helper functions --


getTermVariableName:: GeneratedAbstractSyntax.Term -> String
getTermVariableName (GeneratedAbstractSyntax.TermVariable ( GeneratedAbstractSyntax.Var (_, qubit))) = qubit
getTermVariableName _ = undefined


getVariableName:: GeneratedAbstractSyntax.Var -> String
getVariableName ( GeneratedAbstractSyntax.Var (_, qubit)) = qubit


getGateName:: GeneratedAbstractSyntax.GateVar -> String
getGateName ( GeneratedAbstractSyntax.GateVar (_, name)) = name


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


stripFirstSubstring :: String -> String -> String
stripFirstSubstring _ "" = ""
stripFirstSubstring substr str@(x:xs)
  | substr `isPrefixOf` str = drop (length substr) str
  | otherwise               = x : stripFirstSubstring substr xs


uniquify :: Ord a => [a] -> [a]
uniquify lst = toList $ fromList lst

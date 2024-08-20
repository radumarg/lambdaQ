-- TODO: add header with comments as per ASTtoIASTConverter example when are 100% sure which errors will be caught here

module Frontend.SemanticAnalyser (
  SemanticError,
  runSemanticAnalyser
) where

import qualified Data.Map as Map

import Data.List (intercalate)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import qualified Frontend.PrettyPrinter as PP

data SemanticError =
    DuplicatedFunctionName String                       |  -- function name is not uniquely defined
    MismatchedFunctionNameInTypeAndDeclaration String   |  -- function name in type declaration does not match function name in definition
    TooManyFunctionArguments String                     |  -- number of function arguments for a function exceeds the number of arguments in signature
    NoCtrlQubitsDifferentFromCtrlTerms String           |  -- number of control qubits different from control terms for quantum controlled gates
    NoCtrlQubitsDifferentFromCtrlBits String            |  -- number of control qubits different from control bits for classically controlles gates
    UnknownGate String                                  |  -- gate names should be recognized as belonging to the set of supported gates
    CaseTermsNotDistinct String


instance Show SemanticError where
    show (DuplicatedFunctionName err) 
      = "Function name is not unique: " ++ err
    show (MismatchedFunctionNameInTypeAndDeclaration err) 
      = "Function name in type definition does not match the function name in declaration, " ++ err
    show (TooManyFunctionArguments err) 
      = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (NoCtrlQubitsDifferentFromCtrlTerms err) 
      = "The number of control qubits is different from the number of control terms for some quantum controlled gates: " ++ err
    show (NoCtrlQubitsDifferentFromCtrlBits err) 
      = "The number of control qubits is different from the number of control bits for some classically controlled gates: " ++ err
    show (UnknownGate err) 
      = "Detected gate(s) which are not supported " ++ err
    show (CaseTermsNotDistinct err) 
      = "Some case terms are duplicated " ++ err


runSemanticAnalyser :: GeneratedAbstractSyntax.Program -> Either String GeneratedAbstractSyntax.Program
runSemanticAnalyser (GeneratedAbstractSyntax.ProgDef functions) =
  if null err then Right (GeneratedAbstractSyntax.ProgDef functions) else Left err
  where
    err1 = toString $ functionNamesAreUnique functions
    err2 = toString $ functionNameInTypeMatchesDefinition functions
    err3 = toString $ functionsHaveCorrectNumberOfArguments functions
    err4 = toString $ gateNamesAreValid functions
    err5 = toString $ caseTermsAreDistinct functions
    err6 = toString $ noCtrlQubitsMatchNoCtrTermsQuantumCtrlGates functions
    err7 = toString $ noCtrlQubitsMatchNoCtrTermsClassicCtrlGates functions
    
    err = err1 ++ err2 ++ err3 ++ err4 ++ err5 ++ err6 ++ err7
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


caseTermsAreDistinct :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
caseTermsAreDistinct functions = if null duplicatedCaseTerms then Right () else Left duplicatedCaseTerms
  where
    duplicatedCaseTerms = unlines $ reverse $ extractDuplicatedStrings $ extractCaseExprFstTerms functions []


noCtrlQubitsMatchNoCtrTermsQuantumCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
noCtrlQubitsMatchNoCtrTermsQuantumCtrlGates functions = if null incorrectCtrlTerms then Right () else Left incorrectCtrlTerms
  where
    incorrectCtrlTerms = unlines $ reverse $ verifyCtrlTermsAreCongruent "ModeQuantumCtrl" functions []

noCtrlQubitsMatchNoCtrTermsClassicCtrlGates :: [GeneratedAbstractSyntax.FunctionDeclaration] -> Either String ()
noCtrlQubitsMatchNoCtrTermsClassicCtrlGates functions = if null incorrectCtrlTerms then Right () else Left incorrectCtrlTerms
  where
    incorrectCtrlTerms = unlines $ reverse $ verifyCtrlTermsAreCongruent "ModeClassicCtrl" functions []


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
      innerErrorMessage = funInfo ++ ", the function has " ++ show noFunctionArgs ++ " arguments but expects at most " ++ show maxTypeArgs
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
      getNumberOfTypes GeneratedAbstractSyntax.TypeBasisState = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBool = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeBit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeInteger = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeQbit = 1
      getNumberOfTypes GeneratedAbstractSyntax.TypeUnit = 1


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


verifyCtrlTermsAreCongruent :: String -> [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
verifyCtrlTermsAreCongruent _ [] errorMessages = errorMessages
verifyCtrlTermsAreCongruent mode  (fun:funs)  errorMessages =
  if null nonCongruentCtrlTerms
    then
      verifyCtrlTermsAreCongruent mode funs errorMessages
    else
      verifyCtrlTermsAreCongruent mode funs (newErrorMessage : errorMessages)
    where
      nonCongruentCtrlTerms = extractNonCongruentCtrlTerms mode fun
      newErrorMessage = if mode == "ModeQuantumCtrl" 
        then "  " ++ funInfo ++ ", for some quantum controlled gates the number of control qubits differ from number of basis states controls: " ++    intercalate " and " nonCongruentCtrlTerms 
        else "  " ++ funInfo ++ ", for some classically controlled gates the number of control qubits differ from number of bits controls: " ++ intercalate " and " nonCongruentCtrlTerms 
      funInfo = getFunctionNameAndPosition fun    


extractCaseExprFstTerms :: [GeneratedAbstractSyntax.FunctionDeclaration] -> [String] -> [String]
extractCaseExprFstTerms [] caseExprFstTerms = caseExprFstTerms
extractCaseExprFstTerms (GeneratedAbstractSyntax.FunDecl _ (GeneratedAbstractSyntax.FunDef fname _ term) : funs) caseExprFstTerms
  = extractCaseExprFstTerms funs (extractFromTerm term caseExprFstTerms)
  where
    extractFromTerm :: GeneratedAbstractSyntax.Term -> [String] -> [String]
    extractFromTerm (GeneratedAbstractSyntax.TermCase _ caseExprs) termsList = foldr extractFromCaseExpr termsList caseExprs
    extractFromTerm _ termsList = termsList
    extractFromCaseExpr :: GeneratedAbstractSyntax.CaseExpression -> [String] -> [String]
    extractFromCaseExpr (GeneratedAbstractSyntax.CaseExpr term1 _) termsList
      = ("Duplicated case term: '" ++ PP.showTerm term1 ++ "' found in a case expression declared in the function named: " ++ getVariableName fname) : termsList


functionNamesMatch :: GeneratedAbstractSyntax.FunctionDeclaration -> Bool
functionNamesMatch (GeneratedAbstractSyntax.FunDecl funType funDef) = varName == functionName
  where
    functionName = getFunctionName (GeneratedAbstractSyntax.FunDecl funType funDef)
    (GeneratedAbstractSyntax.FunType fname _) = funType
    (GeneratedAbstractSyntax.Var ((_, _), varName)) = fname
  

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



extractNonCongruentCtrlTerms :: String -> GeneratedAbstractSyntax.FunctionDeclaration -> [String]
extractNonCongruentCtrlTerms mode (GeneratedAbstractSyntax.FunDecl _ funDef) = collectNonCongruentTerms mode fbody
  where
    (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var _) _ fbody) = funDef
    collectNonCongruentTerms :: String -> GeneratedAbstractSyntax.Term -> [String]
    collectNonCongruentTerms md term = case term of
        GeneratedAbstractSyntax.TermQuantumVCtrlsGate (GeneratedAbstractSyntax.CtrlVars _ vs) (GeneratedAbstractSyntax.CtrlBasisStates _ bs) 
            | md == "ModeQuantumCtrl" && length vs /= length bs -> [PP.showTerm term]
            | otherwise -> []
        GeneratedAbstractSyntax.TermQuantumTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms t ts) (GeneratedAbstractSyntax.CtrlBasisStates _ bs) 
            | md == "ModeQuantumCtrl" && length ts /= length bs
              -> [PP.showTerm term] ++ collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
            | otherwise -> collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
        GeneratedAbstractSyntax.TermClassicVCtrlsGate (GeneratedAbstractSyntax.CtrlVars _ vs) (GeneratedAbstractSyntax.CtrlBits _ bs) 
            | md == "ModeClassicCtrl" && length vs /= length bs -> [PP.showTerm term]
            | otherwise -> []
        GeneratedAbstractSyntax.TermClassicTCtrlsGate (GeneratedAbstractSyntax.CtrlTerms t ts) (GeneratedAbstractSyntax.CtrlBits _ bs)
            | md == "ModeClassicCtrl" && length ts /= length bs
              -> [PP.showTerm term] ++ collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
            | otherwise -> collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
        GeneratedAbstractSyntax.TermQuantumCtrlGate (GeneratedAbstractSyntax.CtrlTerm t) _ -> collectNonCongruentTerms md t
        GeneratedAbstractSyntax.TermClassicCtrlGate (GeneratedAbstractSyntax.CtrlTerm t) _ -> collectNonCongruentTerms md t
        GeneratedAbstractSyntax.TermListElement l _ -> collectNonCongruentTerms md (GeneratedAbstractSyntax.TermList l)
        GeneratedAbstractSyntax.TermList GeneratedAbstractSyntax.ListNil -> []
        GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListSingle t) -> collectNonCongruentTerms md t
        GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListMultiple t ts) ->
            collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
        GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListExpressionAdd l1 l2) ->
            collectNonCongruentTerms md (GeneratedAbstractSyntax.TermList l1) ++ collectNonCongruentTerms md (GeneratedAbstractSyntax.TermList l2)
        GeneratedAbstractSyntax.TermList (GeneratedAbstractSyntax.ListCons t l) ->
            collectNonCongruentTerms md t ++ collectNonCongruentTerms md (GeneratedAbstractSyntax.TermList l)
        GeneratedAbstractSyntax.TermTuple t ts -> collectNonCongruentTerms md t ++ concatMap (collectNonCongruentTerms md) ts
        GeneratedAbstractSyntax.TermApply t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermCompose t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermTensorProduct t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermIfElse t1 t2 t3 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2 ++ collectNonCongruentTerms md t3
        GeneratedAbstractSyntax.TermLetSingle _ t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermLetMultiple _ _ t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermLetSugarSingle _ t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermLetSugarMultiple _ _ t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermCase t [] -> collectNonCongruentTerms md t
        GeneratedAbstractSyntax.TermCase t1 (GeneratedAbstractSyntax.CaseExpr t2 t3 : caseExprs) ->
            collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2 ++ collectNonCongruentTerms md t3 ++ collectNonCongruentTerms md (GeneratedAbstractSyntax.TermCase t1 caseExprs)
        GeneratedAbstractSyntax.TermLambda _ _ _ t -> collectNonCongruentTerms md t
        GeneratedAbstractSyntax.TermDollar t1 t2 -> collectNonCongruentTerms md t1 ++ collectNonCongruentTerms md t2
        GeneratedAbstractSyntax.TermGate _ -> []
        GeneratedAbstractSyntax.TermUnit -> []
        GeneratedAbstractSyntax.TermBasisState _ -> []
        GeneratedAbstractSyntax.TermBit _ -> []
        GeneratedAbstractSyntax.TermBoolExpression _ -> []
        GeneratedAbstractSyntax.TermIntegerExpression _ -> []
        GeneratedAbstractSyntax.TermVariable _ -> []


-- Some helper functions --


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


countOccurrences :: [String] -> Map.Map String Int
countOccurrences = foldr (\s -> Map.insertWith (+) s 1) Map.empty


extractDuplicatedStrings :: [String] -> [String]
extractDuplicatedStrings strs = Map.keys $ Map.filter (> 1) occurences
  where
    occurences = countOccurrences strs
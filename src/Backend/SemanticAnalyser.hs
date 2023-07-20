
module Backend.SemanticAnalyer where

data SemanticError =
    DuplicatedFunName String  |                          -- function names must be unique
    DuplicateFunDeclaration String |                     -- there should be no duplicated function declaration
    MismatchedFunDefinitionAndDeclaration String |       -- function signature in declaration should match the signature of function in definition
    IncorrectNumberOfFunArguments String |               -- number of function arguments for a function call does not exceed number of arguments in signature
    ControlQbitsNotDistinct String |                     -- control qubits for any given gate must be distinct
    ControlAndTargetQbitsNotDistinct String |            -- for a controlled gate the control and target qubits must be distinct
    UnknownGate                                          -- gate names should be recognized as belonging to the set of supported gates

instance Show SemanticError where
    show (DuplicatedFunName err) = "???"
    show (DuplicateFunDeclaration err) = "???"  
    show (MismatchedFunDefinitionAndDeclaration err) = "???"
    show (IncorrectNumberOfFunArguments err) = "???"
    show (ControlQbitsNotDistinct err) = "???"
    show (ControlAndTargetQbitsNotDistinct err) = "???"
    show (UnknownGate err) = "???"

--performSemanticAnalysis :: Program -> Either SemanticError ()
--performSemanticAnalysis program = ()

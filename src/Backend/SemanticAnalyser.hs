
module Backend.SemanticAnalyer where

import Backend.IAST (Program)

data SemanticError =
    DuplicatedFunctionName String                     |  -- function names must be unique
    DuplicateFunctionDefinition String                |  -- there should be no duplicated function definition
    DuplicateFunctionDeclaration String               |  -- there should be no duplicated function declaration
    MismatchedFunctionDefinitionAndDeclaration String |  -- function signature in declaration should match the signature of function in definition
    IncorrectNumberOfFunctionArguments String         |  -- number of function arguments for a function call does not exceed number of arguments in signature
    ControlQbitsNotDistinct String                    |  -- control qubits for controlled gates must be distinct
    ControlBitsNotDistinct String                     |  -- control bits for classically controlled gates must be distinct
    ControlAndTargetQbitsNotDistinct String           |  -- for a controlled gate the control and target qubits must be distinct
    InvalidBitValue String                            |  -- the value of a bit must be either 0 or 1
    UnknownGate String                                   -- gate names should be recognized as belonging to the set of supported gates

instance Show SemanticError where
    show (DuplicatedFunctionName err) = "Function name is not unique: " ++ err
    show (DuplicateFunctionDefinition err) = "Function definition is duplicated: "  ++ err
    show (DuplicateFunctionDeclaration err) = "Function declaration is duplicated: "  ++ err
    show (MismatchedFunctionDefinitionAndDeclaration err) = "Function signature in declaration  does not match the signature in definition: " ++ err
    show (IncorrectNumberOfFunctionArguments err) = "Number of function arguments exceeds the number of arguments in signature: " ++ err
    show (ControlQbitsNotDistinct err) = "The control qubits for controlled gate are not distinct: " ++ err
    show (ControlBitsNotDistinct err) = "The control bits for classical controlled gate are not distinct: " ++ err
    show (ControlAndTargetQbitsNotDistinct err) = "The control and target qubits are not distinct: " ++ err
    show (InvalidBitValue err) = "The bit value should be either 0 or 1: " ++ err
    show (UnknownGate err) = "This gate is not supported: " ++ err

-- TODO: implement
performSemanticAnalysis :: Program -> Either SemanticError ()
performSemanticAnalysis program = Right ()

-- Here the (annotated??) intermediate abstract syntax tree is evaluated and OpenQASM programs will be generated.
-- Language lambdaQ implements classical control and call by value formal semantics.

module Backend.CodeGenerator (
  CodeGenerationError,
  runCodeGenerator
) where

import Backend.IAST (Program)

-- not all code errors can be caught during semantic analysis and type checking
data CodeGenerationError =
    SomeCodeGenerationError String      |
    SomeOtherCodeGenerationError String |
    EvenMoreCodeGenerationError String

instance Show CodeGenerationError where
    show (SomeCodeGenerationError error) = "?: " ++ error
    show (SomeOtherCodeGenerationError error) = "?: " ++ error
    show (EvenMoreCodeGenerationError error) = "?: " ++ error

runCodeGenerator :: Program -> Either String String
runCodeGenerator program = undefined
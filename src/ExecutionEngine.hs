module ExecutionEngine where

import Control.Monad.Except
  ( 
    ExceptT(..),
  )

import qualified Backend.SemanticAnalyser as SemanticAnalyser
import qualified Backend.TypeChecker as TypeChecker
import qualified Backend.CodeGenerator as CodeGenerator
 
data ExecutionError = 
    ParseError String                                    | 
    SemanticError SemanticAnalyser.SemanticError         | 
    TypeError TypeChecker.TypeError                      |
    CodeGeneratioError CodeGenerator.CodeGenerationError |
    FileDoesNotExist FilePath
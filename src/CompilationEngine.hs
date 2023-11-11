module CompilationEngine where

import Control.Monad.Except
  ( 
    MonadIO(liftIO),
    MonadError(throwError),
    ExceptT(..),
  )
import Control.Exception (Exception, try)

import Backend.IAST (Program)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import qualified Backend.SemanticAnalyser as SemanticAnalyser
import qualified Backend.TypeChecker as TypeChecker
import qualified Backend.CodeGenerator as CodeGenerator

 
data CompilationError =
    ParseError String                                    | 
    SemanticError SemanticAnalyser.SemanticError         | 
    TypeError TypeChecker.TypeError                      |
    CodeGenerationError CodeGenerator.CodeGenerationError |
    FileDoesNotExist FilePath

instance Show CompilationError where
  show (ParseError e) =
    "parse error:\n" ++ show e

  show (SemanticError e) =
    "semantic error:\n" ++ show e

  show (TypeError e) =
    "type error:\n" ++ show e

  show (CodeGenerationError e) =
    "code generation error " ++ show e

  show (FileDoesNotExist file) =
    "file not found: " ++ show file

-- ExceptT monad transformer can be used to 
-- add error handling to another monad
type Exec a = ExceptT CompilationError IO a

readTheFile :: FilePath -> Exec String
readTheFile = undefined

parseProgram :: String -> Exec GeneratedAbstractSyntax.Program
parseProgram = undefined

semanticAnalysis :: GeneratedAbstractSyntax.Program -> Exec GeneratedAbstractSyntax.Program
semanticAnalysis = undefined

convertAstToIast :: GeneratedAbstractSyntax.Program -> Exec Program
convertAstToIast = undefined

typeCheck :: Program -> Exec Program
typeCheck = undefined

generateCode  :: Program -> Exec String
generateCode = undefined

module CompilationEngine where

import Data.Bifunctor ( Bifunctor(bimap, first) )
import Control.Monad.Except
  (
    MonadIO(liftIO),
    MonadError(throwError),
    ExceptT(..),
  )
import Control.Exception (Exception, try)

import Backend.IAST (Program)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Backend.SemanticAnalyser (runSemanticAnalysis)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import qualified Backend.TypeChecker as TypeChecker
import qualified Backend.CodeGenerator as CodeGenerator

data CompilationError =
    ParseError String                                     |
    SemanticError String                                  |
    TypeError TypeChecker.TypeError                       |
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
readTheFile path = do
  read <- liftIO (try (readFile path) :: IO (Either IOError String))
  case read of
    Left  _ -> throwError $ FileDoesNotExist path
    Right str -> return str

parseProgram :: String -> Exec GeneratedAbstractSyntax.Program
parseProgram = ExceptT . return . first ParseError . pProgram . myLexer

semanticAnalysis :: GeneratedAbstractSyntax.Program -> Exec GeneratedAbstractSyntax.Program
semanticAnalysis = ExceptT . return . first SemanticError . runSemanticAnalysis

convertAstToIast :: GeneratedAbstractSyntax.Program -> Exec Program
convertAstToIast = undefined

typeCheck :: Program -> Exec Program
typeCheck = undefined

generateCode  :: Program -> Exec String
generateCode = undefined

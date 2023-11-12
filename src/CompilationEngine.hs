module CompilationEngine where

import Data.Bifunctor ( Bifunctor(bimap, first) )
import Control.Monad.Except
  (
    MonadIO(liftIO),
    MonadError(throwError),
    ExceptT(..),
  )
import Control.Exception (Exception, try)

import Backend.IAST (Program, runAstToIastConverter)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Backend.SemanticAnalyser (runSemanticAnalyser)
import Backend.TypeChecker (runTypeChecker)
import Backend.CodeGenerator (runCodeGenerator)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data CompilationError =
    ParseError String          |
    SemanticError String       |
    TypeError String           |
    CodeGenerationError String |
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
semanticAnalysis = ExceptT . return . first SemanticError . runSemanticAnalyser

convertAstToIast :: GeneratedAbstractSyntax.Program -> Exec Program
convertAstToIast = ExceptT . return . first undefined . runAstToIastConverter

typeCheck :: Program -> Exec Program
typeCheck = ExceptT . return . first TypeError . runTypeChecker

generateCode  :: Program -> Exec String
generateCode = ExceptT . return . first CodeGenerationError . runCodeGenerator

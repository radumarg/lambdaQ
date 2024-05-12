module CompilationEngine (
  Exec,
  convertAstToIast,
  generateCode,
  parseProgram,
  readFileContents,
  semanticAnalysis,
  typeCheck,
) where

import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad.Except
  (
    MonadIO(liftIO),
    MonadError(throwError),
    ExceptT(..),
  )
import Control.Exception (try)

import Common ( ErrorMessage )
import Backend.ASTtoIASTConverter (Program, runAstToIastConverter)
import Backend.SemanticAnalyser (runSemanticAnalyser)
import Backend.TypeChecker (runTypeChecker)
import Backend.CodeGenerator (runCodeGenerator)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Layout ( resolveLayout )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data CompilationError =
    ParseError ErrorMessage                |
    SemanticError ErrorMessage             |
    SyntaxTreeConversionError ErrorMessage |
    TypeCheckError ErrorMessage            |
    CodeGenerationError ErrorMessage       |
    FileDoesNotExist FilePath

instance Show CompilationError where
  show (ParseError e) =
    "parse error:\n" ++ show e

  show (SemanticError e) =
    "semantic error:\n" ++ show e

  show (SyntaxTreeConversionError e) =
    "syntax tree conversion error:\n" ++ show e

  show (TypeCheckError e) =
    "type error:\n" ++ show e

  show (CodeGenerationError e) =
    "code generation error " ++ show e

  show (FileDoesNotExist file) =
    "file not found: " ++ show file

-- ExceptT monad transformer can be used to 
-- add error handling to another monad
type Exec a = ExceptT CompilationError IO a

readFileContents :: FilePath -> Exec String
readFileContents path = do
  fileContents <- liftIO (try (readFile path) :: IO (Either IOError String))
  case fileContents of
    Left  _ -> throwError $ FileDoesNotExist path
    Right str -> return str

parseProgram :: String -> Exec GeneratedAbstractSyntax.Program
parseProgram = ExceptT . return . first ParseError . pProgram . resolveLayout True . myLexer

semanticAnalysis :: GeneratedAbstractSyntax.Program -> Exec GeneratedAbstractSyntax.Program
semanticAnalysis = ExceptT . return . first SemanticError . runSemanticAnalyser

convertAstToIast :: GeneratedAbstractSyntax.Program -> Exec Program
convertAstToIast = ExceptT . return . first SyntaxTreeConversionError . runAstToIastConverter

typeCheck :: Program -> Exec Program
typeCheck = ExceptT . return . first TypeCheckError . runTypeChecker

generateCode  :: Program -> Exec String
generateCode = ExceptT . return . first CodeGenerationError . runCodeGenerator

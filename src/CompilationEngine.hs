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
import Frontend.ASTtoIASTConverter (Program, runAstToIastConverter)
import Frontend.SemanticAnalyser (runSemanticAnalyser)
import Frontend.TypeChecker (runTypeChecker)
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
    "Parse error:\n" ++ show e

  show (SemanticError e) =
    "Semantic error:\n" ++ show e

  show (SyntaxTreeConversionError e) =
    "Syntax tree conversion error:\n" ++ show e

  show (TypeCheckError e) =
    "Type error:\n" ++ show e

  show (CodeGenerationError e) =
    "Code generation error:\n" ++ show e

  show (FileDoesNotExist file) =
    "File not found: " ++ show file

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

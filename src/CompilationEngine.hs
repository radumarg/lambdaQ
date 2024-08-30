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

import Frontend.ASTtoIASTConverter (Program, runAstToIastConverter)
import Frontend.SemanticAnalyser (runSemanticAnalyser)
import Frontend.TypeChecker (runTypeChecker)
import Backend.CodeGenerator (runCodeGenerator)
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Layout ( resolveLayout )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data CompilationError =
    ParseError String                |
    SemanticError String             |
    SyntaxTreeConversionError String |
    TypeCheckError String            |
    CodeGenerationError String       |
    FileDoesNotExist FilePath

instance Show CompilationError where
  show (ParseError e) =
    "Parse error: " ++ e

  show (SemanticError e) =
    "Semantic error(s):\n" ++ e

  show (SyntaxTreeConversionError e) =
    "Syntax tree conversion error(s):\n" ++ e

  show (TypeCheckError e) =
    "Type error(s):\n" ++ e

  show (CodeGenerationError e) =
    "Code generation error(s):\n" ++ e

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

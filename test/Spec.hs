module Main where

import Test.Hspec

import qualified Frontend.ParserSpec
import qualified Frontend.SemanticAnalyserSpec
import qualified Frontend.ASTtoIASTConverterSpec
import qualified Frontend.TypeCheckerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer and Parser" Frontend.ParserSpec.spec
  describe "Frontend.SemanticAnalyser" Frontend.SemanticAnalyserSpec.spec
  describe "Frontend.ASTtoIASTConverter" Frontend.ASTtoIASTConverterSpec.spec
  describe "Frontend.TypeChecker" Frontend.TypeCheckerSpec.spec
  



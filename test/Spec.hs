module Main where

import Test.Hspec

import qualified Frontend.ParserSpec
import qualified Backend.SemanticAnalyserSpec
import qualified Backend.ASTtoIASTConverterSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer and Parser" Frontend.ParserSpec.spec
  describe "Backend.SemanticAnalyser" Backend.SemanticAnalyserSpec.spec
  describe "Backend.ASTtoIASTConverter" Backend.ASTtoIASTConverterSpec.spec



module Main where

import Test.Hspec

import qualified Backend.SemanticAnalyserSpec
import qualified Backend.ASTtoIASTConverterSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Backend.SemanticAnalyser" Backend.SemanticAnalyserSpec.spec
  describe "Backend.ASTtoIASTConverter" Backend.ASTtoIASTConverterSpec.spec



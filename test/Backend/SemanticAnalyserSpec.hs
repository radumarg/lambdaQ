{-# LANGUAGE LambdaCase #-}

module Backend.SemanticAnalyserSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import Control.Monad.Except ( runExceptT)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runSemanticAnalysis :: FilePath -> Exec GeneratedAbstractSyntax.Program
runSemanticAnalysis filePath = readTheFile filePath
                              >>= parseProgram
                              >>= semanticAnalysis

testProgram :: FilePath -> IO String
testProgram filePath = runExceptT (runSemanticAnalysis filePath)  >>= \case
                Left err -> return (show err)
                Right _ -> return "OK"

spec :: Spec
spec =  do
  describe "SemanticAnalysis" $ do

    context "when provided with a valid coinflip program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example00-CoinFlip__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Deutsch algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example01-deutschAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Deutsch Jozsa algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example02-deutschJozsaAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid phase kick back program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example03-phaseKickBack__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Bernstein Vazirani algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example04-bernsteinVaziraniAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Simon algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example05-simonAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid teleportation protocol program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example06-teleportationProtocol__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid four qubit adder program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example07-fourQubitAdder__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Grover algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example08-groverAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid quantum phase estimation program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example09-quantumPhaseEstimation__Good.lq" `shouldReturn` "OK"

    context "when provided with a valid Shor algorithm program" $ do
      it "returns no error" $ do
        testProgram "test/programs/example10-shorAlgorithm__Good.lq" `shouldReturn` "OK"

    context "when provided with a coinflip program where function name does not match declaration" $ do
      it "returns an error" $ do
         result <- testProgram "test/programs/example00-CoinFlip__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
         result `shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)





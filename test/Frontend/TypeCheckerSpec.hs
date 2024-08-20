{-# LANGUAGE LambdaCase #-}

module Frontend.TypeCheckerSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.List (isInfixOf)
import Test.Hspec ( context, describe, it, shouldReturn, shouldSatisfy, Spec )

import CompilationEngine (Exec, readFileContents, parseProgram, semanticAnalysis, convertAstToIast, typeCheck)

import Frontend.ASTtoIASTConverter (Program)

runTypeChecker :: FilePath -> Exec Program
runTypeChecker filePath = readFileContents filePath
                            >>= parseProgram
                            >>= semanticAnalysis
                            >>= convertAstToIast
                            >>= typeCheck

testTypeCheckerReturnsOK :: FilePath -> IO String
testTypeCheckerReturnsOK filePath = runExceptT (runTypeChecker filePath)  >>= \case
                Left err -> return (show err)
                Right _ -> return "OK"

trimNewLines :: String -> String
trimNewLines ('\n':xs)     = trimNewLines (' ' : xs)
trimNewLines (' ':' ':xs)  = trimNewLines (' ' : xs)
trimNewLines (x:xs)        = x : trimNewLines xs
trimNewLines ""            = ""

spec :: Test.Hspec.Spec
spec =  do
  Test.Hspec.describe "Testing TypeChecker on example programs:" $ do

    -- EXAMPLE PROGRAMS, EXPECT NO ERRORS --

    -- Test.Hspec.context "when provided with a valid coinflip program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example00-CoinFlip.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Deutsch algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example01-deutschAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Deutsch Jozsa algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid phase kick back program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example03-phaseKickBack.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Bernstein Vazirani algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Simon algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example05-simonAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid teleportation protocol program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example06-teleportationProtocol.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid four qubit adder program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example07-fourQubitAdder.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Grover algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example08-groverAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid quantum phase estimation program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example09-quantumPhaseEstimation.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Shor algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example10-shorAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid QFT program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeCheckerReturnsOK "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"

    -- SMALL PROGRAMS, EXPECT NO ERRORS --

    Test.Hspec.context "when provided with a valid QFT program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeCheckerReturnsOK "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"


    -- SMALL PROGRAMS, EXPECT ERRORS --



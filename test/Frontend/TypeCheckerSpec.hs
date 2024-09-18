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

testTypeChecker :: FilePath -> IO String
testTypeChecker filePath = runExceptT (runTypeChecker filePath)  >>= \case
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
    --     testTypeChecker "test/programs/examples/example00-CoinFlip.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Deutsch algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example01-deutschAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Deutsch Jozsa algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid phase kick back program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example03-phaseKickBack.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Bernstein Vazirani algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Simon algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example05-simonAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid teleportation protocol program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example06-teleportationProtocol.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid four qubit adder program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example07-fourQubitAdder.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Grover algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example08-groverAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid quantum phase estimation program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example09-quantumPhaseEstimation.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid Shor algorithm program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example10-shorAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    -- Test.Hspec.context "when provided with a valid QFT program" $ do
    --   Test.Hspec.it "returns no error" $ do
    --     testTypeChecker "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"

    -- SMALL PROGRAMS, EXPECT NO ERRORS --

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/new_1.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/new_2.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/new_3.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/new_4.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/bit_1.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/bit_2.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/bit_3.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/bit_4.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_1.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_2.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_3.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_4.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_5.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_6.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_7.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/measr_8.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid program" $ do
      Test.Hspec.it "returns no error" $ do
        testTypeChecker "test/programs/good/typechecker/qbit_int_1.lq" `Test.Hspec.shouldReturn` "OK"

    -- SMALL PROGRAMS, EXPECT ERRORS --

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_1.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit' of the function named: 'main' defined at line: 2 cannot be matched with actual type: 'TypeQbit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_2.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit' of the function named: 'main' defined at line: 5 cannot be matched with actual type: 'TypeQbit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_3.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeNonLinear (TypeUnit :->: TypeBit)' of the function named: 'initQubit' defined at line: 2 cannot be matched with actual type: 'TypeNonLinear (TypeUnit :->: TypeQbit)'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_4.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeQbit :*: TypeBit' of the function named: 'main' defined at line: 5 cannot be matched with actual type: 'TypeQbit :**: 2'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_5.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "Type error(s):\nThe expected type 'TypeNonLinear (TypeUnit :->: TypeQbit)' of the function named: 'initBit' defined at line: 2 cannot be matched with actual type: 'TypeNonLinear (TypeUnit :->: TypeNonLinear (TypeBasisState :->: TypeQbit))'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/new_6.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "In the function named 'initBit' defined at line 2 the expected type 'TypeQbit' of term 'measr (line: 2, col: 13)' is not compatible with type 'TypeNonLinear (TypeBasisState :->: TypeQbit)' of term 'new (line: 2, col: 19)'." `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/bit_1.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit' of the function named: 'a' defined at line: 2 cannot be matched with actual type: 'TypeQbit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/bit_2.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeNonLinear TypeBit' of the function named: 'a' defined at line: 2 cannot be matched with actual type: 'TypeQbit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/bit_3.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit :*: TypeQbit' of the function named: 'main' defined at line: 8 cannot be matched with actual type: 'TypeQbit :*: TypeBit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_1.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The variable named 'a' in the function named: 'main' defined at line: 2 denotes a function which is not in scope" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_2.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeQbit' of the function named: 'main' defined at line: 5 cannot be matched with actual type: 'TypeBit'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_3.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit :*: TypeQbit' of the function named: 'main' defined at line: 5 cannot be matched with actual type: 'TypeBit :**: 2'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_4.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeQbit :->: TypeQbit' of the function named: 'initBit' defined at line: 2 cannot be matched with actual type: 'TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_5.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeBit :->: TypeQbit' of the function named: 'initBit' defined at line: 2 cannot be matched with actual type: 'TypeNonLinear (TypeQbit :->: TypeNonLinear TypeBit)'" `isInfixOf` str)

    Test.Hspec.context "when provided with an invalid program" $ do
      Test.Hspec.it "returns an error" $ do
        result <- testTypeChecker "test/programs/bad/typechecker/measr_6.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "The expected type 'TypeNonLinear (TypeUnit :->: TypeBit)' of the function named: 'main' defined at line: 8 cannot be matched with actual type: 'TypeUnit :->: TypeBit'" `isInfixOf` str)

    -- Test.Hspec.context "when provided with an invalid program" $ do
    --   Test.Hspec.it "returns an error" $ do
    --     result <- testTypeChecker "test/programs/bad/typechecker/measr_2.lq"
    --     result `Test.Hspec.shouldSatisfy`
    --       (\str -> "?" `isInfixOf` str)


    -- Test.Hspec.context "when provided with an invalid program" $ do
    --   Test.Hspec.it "returns an error" $ do
    --     result <- testTypeChecker "test/programs/bad/typechecker/bit_1.lq"
    --     result `Test.Hspec.shouldSatisfy`
    --       (\str -> "?" `isInfixOf` str)

    --  putStrLn result



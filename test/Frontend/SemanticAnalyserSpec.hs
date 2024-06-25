{-# LANGUAGE LambdaCase #-}

module Frontend.SemanticAnalyserSpec (spec) where

import Data.List (isInfixOf)
import Control.Monad.Except (runExceptT)
import Test.Hspec
    ( context, describe, it, shouldReturn, shouldSatisfy, Spec )

import CompilationEngine (Exec, readFileContents, parseProgram, semanticAnalysis)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runSemanticAnalysis :: FilePath -> Exec GeneratedAbstractSyntax.Program
runSemanticAnalysis filePath = readFileContents filePath
                              >>= parseProgram
                              >>= semanticAnalysis

testSemanticAnalyser :: FilePath -> IO String
testSemanticAnalyser filePath = runExceptT (runSemanticAnalysis filePath)  >>= \case
              Left err -> return (show err)
              Right _ -> return "OK"

spec :: Spec
spec =  do
  describe "Testing Semantic Analysis:" $ do

    -- EXPECT NO ERRORS --

    context "when provided with a valid coinflip program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example00-CoinFlip.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Deutsch algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example01-deutschAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Deutsch Jozsa algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid phase kick back program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example03-phaseKickBack.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Bernstein Vazirani algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Simon algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example05-simonAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid teleportation protocol program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example06-teleportationProtocol.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid four qubit adder program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example07-fourQubitAdder.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Grover algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example08-groverAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid quantum phase estimation program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example09-quantumPhaseEstimation.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid Shor algorithm program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example10-shorAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    context "when provided with a valid QFT program" $ do
      it "returns no error" $ do
        testSemanticAnalyser "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"

    -- EXPECT SOME ERRORS: FUNCTION NAMES ARE NOT UNIQUE  --

    context "when provided with a coinflip program where function names are not unique" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__FunctionNamesAreNotUnique.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name is not unique" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"coinFlip\" at line: 2 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"coinFlip\" at line: 5 and column: 1" `isInfixOf` str)

    context "when provided with a Grover algorithm program where function names are not unique" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__FunctionNamesAreNotUnique.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name is not unique" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"groverDiffusionOperator\" at line: 6 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"groverDiffusionOperator\" at line: 15 and column: 1" `isInfixOf` str)

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION NAME IN TYPE DECLARATION DO NOT MATCH  --

    context "when provided with a coinflip program where function name does not match type" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"coinFlip'\" at line: 2 and column: 1" `isInfixOf` str)

    context "when provided with a Grover algorithm program where two function names do not match declaration" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"oracle'\" at line: 2 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function: \"groverDiffusionOperator'\" at line: 6 and column: 1" `isInfixOf` str)
        
    -- EXPECT NO ERRORS: FUNCTION NAME AND FUNCTION ARGUMENTS IS NOT CORRECT  --

    context "when provided with a coinflip program where number of function arguments is correct" $ do
      it "returns an error" $ do
        testSemanticAnalyser "test/programs/bad/example00-CoinFlip__CorrectNumberOfFunctionArguments_1.lq" `Test.Hspec.shouldReturn` "OK"

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION ARGUMENTS IS NOT CORRECT  --

    context "when provided with a coinflip program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_1.lq"
        putStrLn result
    --     result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'other' at line: 5 and column: 1, the function has 2 arguments but expects as most 0" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'main' at line: 8 and column: 1, the function has 1 arguments but expects as most 0" `isInfixOf` str)

    -- context "when provided with a coinflip program where number of function arguments is incorrect" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_2.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'main' at line: 9 and column: 1, the function has 3 arguments but expects as most 0" `isInfixOf` str)

    -- context "when provided with a coinflip program where number of function arguments is incorrect" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_3.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'other'' at line: 10 and column: 1, the function has 6 arguments but expects as most 4" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'main' at line: 13 and column: 1, the function has 5 arguments but expects as most 4" `isInfixOf` str)

    -- -- EXPECT SOME ERRORS: CONTROL QUBITS ARE NOT DISTINCT  --

    -- context "when provided with a four qubit adder program where controls qubits are not distinct" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example07-fourQubitAdder__ControlQubitsNotDistinct.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "The control qubits for controlled gate(s) are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'carry' at line: 8 and column: 1" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for the following qubits: q1,q1, and ,q3,q3" `isInfixOf` str)

    -- -- EXPECT SOME ERRORS: CONTROL BITS ARE NOT DISTINCT  --

    -- context "when provided with a bogus program where controls bits are not distinct" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example-Bogus__ControlBitsNotDistinct.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "The control bits for classical controlled gate(s) are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'fun' at line: 3 and column: 1" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for the following bits: b1,b1, and ,b2,b1,b2" `isInfixOf` str)

    -- -- EXPECT SOME ERRORS: CONTROL QUBITS AND TARGET QUBITS ARE NOT DISTINCT  --

    -- context "when provided with a Grover algorithm program where control and target qubits are not different" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__ControlAnTargetQubitsNotDifferent.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "The control and target qubits are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'oracle' at line: 2 and column: 1 for qubits identified with names: q6" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function: 'oracle'' at line: 6 and column: 1 for qubits identified with names: q0" `isInfixOf` str)

    -- -- TODO add support and test for multiple controlled gates
    
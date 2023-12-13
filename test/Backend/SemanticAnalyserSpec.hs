{-# LANGUAGE LambdaCase #-}

module Backend.SemanticAnalyserSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec
import Control.Monad.Except (runExceptT)

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
  describe "Testing Semantic Analysis:" $ do

    -- EXPECT NO ERRORS --

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

    -- EXPECT SOME ERRORS: FUNCTION NAMES ARE NOT UNIQUE  --

    context "when provided with a coinflip program where function names are not unique" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example00-CoinFlip__FunctionNamesAreNotUnique.lq"
        result `shouldSatisfy` (\str -> "Function name is not unique" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'coinFlip' at line: 2 and column: 1" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'coinFlip' at line: 5 and column: 1" `isInfixOf` str)

    context "when provided with a Grover algorithm program where function names are not unique" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example08-groverAlgorithm__FunctionNamesAreNotUnique.lq"
        result `shouldSatisfy` (\str -> "Function name is not unique" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'groverDiffusionOperator' at line: 6 and column: 1" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'groverDiffusionOperator' at line: 15 and column: 1" `isInfixOf` str)

    context "when provided with a coinflip program where function name does not match declaration" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example00-CoinFlip__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "at line: 2 and column: 1" `isInfixOf` str)

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION TYPE DO NOT MATCH  --

    context "when provided with a Grover algorithm program where two function names do not match declaration" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example08-groverAlgorithm__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'oracle'' at line: 2 and column: 1" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'groverDiffusionOperator'' at line: 6 and column: 1" `isInfixOf` str)

    context "when provided with a coinflip program where number of function arguments is correct" $ do
      it "returns an error" $ do
        testProgram "test/programs/example00-CoinFlip__CorrectNumberOfFunctionArguments_1.lq" `shouldReturn` "OK"

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION ARGUMENTS IS NOT CORRECT  --

    context "when provided with a coinflip program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example00-CoinFlip__IncorrectNumberOfFunctionArguments_1.lq"
        result `shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'other' at line: 5 and column: 1, the function has 2 arguments but expects as most 0" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'main' at line: 8 and column: 1, the function has 1 arguments but expects as most 0" `isInfixOf` str)

    context "when provided with a coinflip program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example00-CoinFlip__IncorrectNumberOfFunctionArguments_2.lq"
        result `shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'main' at line: 9 and column: 1, the function has 3 arguments but expects as most 0" `isInfixOf` str)

    context "when provided with a coinflip program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example00-CoinFlip__IncorrectNumberOfFunctionArguments_3.lq"
        result `shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'other'' at line: 10 and column: 1, the function has 6 arguments but expects as most 4" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'main' at line: 13 and column: 1, the function has 5 arguments but expects as most 4" `isInfixOf` str)

    -- EXPECT SOME ERRORS: CONTROL QUBITS ARE NOT DISTINCT  --

    context "when provided with a four qubit adder program where controls qubits are not distinct" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example07-fourQubitAdder__ControlQubitsNotDistinct.lq"
        result `shouldSatisfy` (\str -> "The control qubits for controlled gate(s) are not distinct" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'carry' at line: 8 and column: 1" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for the following qubits: q1,q1, and ,q3,q3" `isInfixOf` str)

    -- EXPECT SOME ERRORS: CONTROL BITS ARE NOT DISTINCT  --

    context "when provided with a bogus program where controls bits are not distinct" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example-Bogus__ControlBitsNotDistinct.lq"
        result `shouldSatisfy` (\str -> "The control bits for classical controlled gate(s) are not distinct" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'fun' at line: 3 and column: 1" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for the following bits: b1,b1, and ,b2,b1,b2" `isInfixOf` str)

    -- EXPECT SOME ERRORS: CONTROL QUBITS AND TARGET QUBITS ARE NOT DISTINCT  --

    context "when provided with a Grover algorithm program where control and target qubits are not different" $ do
      it "returns an error" $ do
        result <- testProgram "test/programs/example08-groverAlgorithm__ControlAnTargetQubitsNotDifferent.lq"
        result `shouldSatisfy` (\str -> "The control and target qubits are not distinct" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'oracle' at line: 2 and column: 1 for qubits identified with names: q6" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "for function 'oracle'' at line: 6 and column: 1 for qubits identified with names: q0" `isInfixOf` str)

    -- TODO add support and test for multiple controlled gates
    
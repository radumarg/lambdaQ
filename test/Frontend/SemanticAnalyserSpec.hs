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
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"coinFlip\" at line: 2 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"coinFlip\" at line: 5 and column: 1" `isInfixOf` str)

    context "when provided with a Grover algorithm program where function names are not unique" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__FunctionNamesAreNotUnique.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name is not unique" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"groverDiffusionOperator\" at line: 6 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"groverDiffusionOperator\" at line: 15 and column: 1" `isInfixOf` str)

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION NAME IN TYPE DECLARATION DO NOT MATCH  --

    context "when provided with a coinflip program where function name does not match type" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"coinFlip'\" at line: 2 and column: 1" `isInfixOf` str)

    context "when provided with a Grover algorithm program where two function names do not match declaration" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__FunctionNamesDefinitionDoesNotMatchDeclaration.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"oracle'\" at line: 2 and column: 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"groverDiffusionOperator'\" at line: 6 and column: 1" `isInfixOf` str)
        
    -- EXPECT NO ERRORS: FUNCTION NAME AND FUNCTION ARGUMENTS IS NOT CORRECT  --

    context "when provided with a coinflip program where number of function arguments is correct" $ do
      it "returns an error" $ do
        testSemanticAnalyser "test/programs/good/example00-CoinFlip__NoArgsLessThanMaxFunctionArgs.lq" `Test.Hspec.shouldReturn` "OK"

    -- EXPECT SOME ERRORS: FUNCTION NAME AND FUNCTION ARGUMENTS IS NOT CORRECT  --

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_unit_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_bit_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_qubit_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_integer_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_integer_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_boolean_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_list_arg__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    -- TODO: FIX NOW
    -- context "when provided with a program where number of function arguments is incorrect" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/function_has_sum_arg__IncorrectNumberOfFunctionArguments.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 2 arguments but expects at most 1" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/function_has_exponential_args__IncorrectNumberOfFunctionArguments.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 2 and column: 1, the function has 4 arguments but expects at most 3" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"other\" at line: 5 and column: 1, the function has 2 arguments but expects at most 0" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"main\" at line: 8 and column: 1, the function has 1 arguments but expects at most 0" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"main\" at line: 9 and column: 1, the function has 3 arguments but expects at most 0" `isInfixOf` str)

    context "when provided with a program where number of function arguments is incorrect" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/example00-CoinFlip__IncorrectNumberOfFunctionArguments_3.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Number of function arguments exceeds the number of arguments in signature" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"other\" at line: 5 and column: 1, the function has 2 arguments but expects at most 0" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for function \"main\" at line: 8 and column: 1, the function has 1 arguments but expects at most 0" `isInfixOf` str)

    -- -- EXPECT SOME ERRORS: CONTROL QUBITS FOR QUANTUM CONTROLLED GATES ARE NOT DISTINCT  --

    -- context "when provided with a four qubit adder program where controls qubits are not distinct" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example07-fourQubitAdder__ControlQubitsNotDistinct.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "The control qubits for some quantum controlled gate(s) are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"sum\" at line: 2 and column: 1 for the following qubit sequence(s): q0, q0" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"carry\" at line: 8 and column: 1 for the following qubit sequence(s): q1, q1 and q3, q3" `isInfixOf` str)

    -- -- EXPECT SOME ERRORS: CONTROL QUBITS FOR CLASSICALLY CONTROLLED GATES ARE NOT DISTINCT  --

    -- context "when provided with a bogus program where controls bits are not distinct" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example-ClassicCtrlGates__ControlQbitsNotDistinct.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "The control qubits for some classically controlled gate(s) are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"fun\" at line: 3 and column: 1" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for the following qubit sequence(s): q1, q1 and q2, q1, q2" `isInfixOf` str)

    -- -- -- EXPECT SOME ERRORS: CONTROL QUBITS AND TARGET QUBITS ARE NOT DISTINCT  --

    -- context "when provided with a program where for a quantum controlled gate with single control where control and target qubits are not different" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__ControlAndTargetQubitNotDifferent_QuantumControlledGate.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "For some quantum controlled gate(s) the control and target qubits are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"oracle\" at line: 5 and column: 1 for qubit(s) identified with variable name(s): q0" `isInfixOf` str)

    -- context "when provided with a program where for a classically controlled gate with single control where control and target qubits are not different" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/example08-groverAlgorithm__ControlAndTargetQubitNotDifferent_ClassicControlledGate.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "For some classically controlled gate(s) the control and target qubits are not distinct" `isInfixOf` str)
    --     result `Test.Hspec.shouldSatisfy` (\str -> "for function \"oracle\" at line: 5 and column: 1 for qubit(s) identified with variable name(s): q0" `isInfixOf` str)

    -- context "when provided with a program where for a quantum controlled gate with single control where control and target qubits are not different" $ do
    --   it "returns an error" $ do
    --     result <- testSemanticAnalyser "test/programs/bad/sampleAlgorithm__ControlAndTargetQubitsNotDifferent_QuantumControlledGate.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "Function name in type definition does not match the function name in declaration, for function \"oracle'\" at line: 2 and column: 1" `isInfixOf` str)
    --     -- result `Test.Hspec.shouldSatisfy` (\str -> "For some quantum controlled gate(s) the control and target qubits are not distinct, for function \"oracle'\" at line: 2 and column: 1 for qubit(s) identified with variable name(s): q3" `isInfixOf` str)
    --     -- result `Test.Hspec.shouldSatisfy` (\str -> "For some quantum controlled gate(s) the control and target qubits are not distinct, for function \"oracle2\" at line: 8 and column: 1 for qubit(s) identified with variable name(s): q0" `isInfixOf` str)
    --     -- result `Test.Hspec.shouldSatisfy` (\str -> "For some classically controlled gate(s) the control and target qubits are not distinct, for function \"oracle'\" at line: 2 and column: 1 for qubit(s) identified with variable name(s): q0" `isInfixOf` str)
    --     -- result `Test.Hspec.shouldSatisfy` (\str -> "For some classically controlled gate(s) the control and target qubits are not distinct, for function \"oracle3\" at line: 13 and column: 1 for qubit(s) identified with variable name(s): q2" `isInfixOf` str)
    --     -- result `Test.Hspec.shouldSatisfy` (\str -> "" `isInfixOf` str)
    --     -- putStrLn result

    -- EXPECT SOME ERRORS: GATES NAMES ARE NOT SUPPORTED  --

    context "when provided with a program where for some gates names are not supported" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/gates_not_supported.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "Detected gate(s) which are not supported" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for gate(s) named: HH" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for gate(s) named: QFT2" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for gate(s) named: QFT3" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for gate(s) named: UU1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` (\str -> "for gate(s) named: UU2, UU3" `isInfixOf` str)

    -- EXPECT SOME ERRORS: CASE TERMS ARE NOT UNIQUE  --

    context "when provided with a program where for some case terms are duplicated" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/case_terms_not_unique.lq"
        result `Test.Hspec.shouldSatisfy` 
          (\str -> "Duplicated case term: 'a1' found in a case expression declared in the function named: fun1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy` 
          (\str -> "Duplicated case term: 'a b' found in a case expression declared in the function named: fun2" `isInfixOf` str)

    -- EXPECT SOME ERRORS: FOR QUANTUM CTRL GATES NO CONTROL TERM AND COBTROL BASIS STATES DIFFER --

    context "when provided with a program where for quantum controlled gates the number of control qubits differ from number of basis states controls" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/no_ctrl_qubits_and_ctrl_basis_state_differ.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "the number of control qubits differ from number of basis states controls: q1, q2 @ 1, 1, 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy`
          (\str -> "the number of control qubits differ from number of basis states controls: q1, q2 q3 @ 0, 0" `isInfixOf` str)

    -- EXPECT SOME ERRORS: FOR CLASSIC CTRL GATES NO CONTROL TERM AND COBTROL BITS DIFFER --

    context "when provided with a program where for quantum controlled gates the number of control qubits differ from number of basis states controls" $ do
      it "returns an error" $ do
        result <- testSemanticAnalyser "test/programs/bad/no_ctrl_qubits_and_ctrl_bits_differ.lq"
        result `Test.Hspec.shouldSatisfy`
          (\str -> "the number of control qubits differ from number of bits controls: q1, q2 values: 1, 1, 1" `isInfixOf` str)
        result `Test.Hspec.shouldSatisfy`
          (\str -> "the number of control qubits differ from number of bits controls: q1, q2 q3 values: 0, 0" `isInfixOf` str)

    -- putStrLn result
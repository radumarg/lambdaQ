{-# LANGUAGE LambdaCase #-}

module Frontend.ParserSpec (spec) where

import Test.Hspec
    ( context, describe, it, shouldReturn, shouldSatisfy, Spec )
import Control.Monad.Except (runExceptT)
import Data.List (isInfixOf)

import CompilationEngine (Exec, readFileContents, parseProgram)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import Frontend.LambdaQ.Print ( printTree )

runParser :: FilePath -> Exec GeneratedAbstractSyntax.Program
runParser filePath = readFileContents filePath
                              >>= parseProgram

testParserReturnsOK :: FilePath -> IO String
testParserReturnsOK filePath = runExceptT (runParser filePath)  >>= \case
                Left err -> return (show err)
                Right _ -> return "OK"

testParserReturnsTree :: FilePath -> IO String
testParserReturnsTree filePath = runExceptT (runParser filePath)  >>= \case
                Left err -> return (show err)
                Right program -> return (printTree program)

trimNewLines :: String -> String
trimNewLines ('\n':xs)     = trimNewLines (' ' : xs)
trimNewLines (' ':' ':xs)  = trimNewLines (' ' : xs)
trimNewLines (x:xs)        = x : trimNewLines xs
trimNewLines ""            = ""

spec :: Test.Hspec.Spec
spec =  do
  Test.Hspec.describe "Testing Lexer and Parser on example programs:" $ do

    -- EXPECT NO ERRORS --

    Test.Hspec.context "when provided with a valid coinflip program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example00-CoinFlip.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Deutsch algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example01-deutschAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Deutsch Jozsa algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid phase kick back program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example03-phaseKickBack.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Bernstein Vazirani algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Simon algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example05-simonAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid teleportation protocol program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example06-teleportationProtocol.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid four qubit adder program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example07-fourQubitAdder.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Grover algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example08-groverAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid quantum phase estimation program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example09-quantumPhaseEstimation.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Shor algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example10-shorAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid QFT program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnsOK "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"

  Test.Hspec.describe "Testing Lexer and Parser on various made up code fragments:" $ do

  -- EXPECT NO ERRORS --

    Test.Hspec.context "when provided with a program which verifies that '$' operator is right associative" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/dollar_is_right_associative_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b $ c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program which verifies that '$' operator is right associative" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/dollar_is_right_associative_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) $ c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and an if else term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ if c then t else f" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and an if else term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if c then t else f $ a" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and an let term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ let { var = term } in term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and let term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = let { var = term } in term' $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a let multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_multiple_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ let { (var1, var2) = term } in term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a let multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_multiple_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = let { (var1, var2) = term } in term' $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and an let sugar term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ var <- term; term'" `isInfixOf` trimNewLines str)
        
    Test.Hspec.context "when provided with a program containing '$' operator and let sugar term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var <- term; term' $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a let multiple sugar term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ var1, var2 <- term; term'" `isInfixOf` trimNewLines str)
        
    Test.Hspec.context "when provided with a program containing '$' operator and a let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term; term' $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a case term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ case term of { a -> b c -> d }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a case term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case term of { a -> b c -> d } $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a lambda term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ \\ var Bit . term" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a lambda term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . term $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a single qubit quantum control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/single_quantum_ctrl_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ with [q0] ctrl [@1]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a single qubit quantum control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/single_quantum_ctrl_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0] ctrl [@1] $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a multiple qubits quantum control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/multiple_quantum_ctrls_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "a $ with [q0, q1] ctrl [@1, @1]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a multiple qubits quantum control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/multiple_quantum_ctrls_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0, q1] ctrl [@1, @1] $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a single qubit classic control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/single_classic_ctrl_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ with [q0] ctrl [1]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a single qubit classic control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/single_classic_ctrl_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0] ctrl [1] $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a multiple qubits classic control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/multiple_classic_ctrls_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "a $ with [q0, q1] ctrl [1, 1]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and a multiple qubits classic control term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/multiple_classic_ctrls_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0, q1] ctrl [1, 1] $ a" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing '$' operator and term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/apply_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/apply_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and compose operator" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/compose_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) . c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and compose operator" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/compose_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b . c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and unit" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/unit_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) ()" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and unit" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/unit_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b ()" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a basis state term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/basis_state_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) @0" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a basis state term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/basis_state_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b @0" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a boolean expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/boolean_expression_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) True == True" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a boolean expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/boolean_expression_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b 2 > 1" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a integer expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/integer_expression_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) 1 + 1" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a integer expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/integer_expression_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b 1 + 1" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a gate term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a gate term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/tuple_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) (q1, q2)" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/tuple_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b (q1, q2)" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) []" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b []" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_3.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) [1]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_4.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b [1]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_5.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) [1, 2]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_6.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b [1, 2]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_7.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) [1] ++ [2, 3]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_8.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b [1, 2] ++ [3]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_9.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) x : [y, z]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_stronger_than_dollar_10.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b x : []" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list element expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_element_stronger_than_dollar_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) [x, y, z] !! 1" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' operator and a list element sxpression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_element_stronger_than_dollar_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b [x, y, z] !! 1" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing three lambda expressions" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_is_right_associative.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . \\ var Qbit . \\ var Bit . term" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_case_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . case term of { a -> b c -> d }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expressions followed by lambda" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_lambda.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case term of { a -> b c -> \\ var Bit . d }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by a let sugar multiple expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_let_sugar_multiple_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . var1, var2 <- term; term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a a let sugar multiple expression followed by lambdas" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_lambda.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- \\ var Bit . a; \\ var Bit . b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by a let sugar single expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_let_sugar_single_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . var1 <- term; term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar single expression followed by lambdas" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_single_followed_by_lambda.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- \\ var Bit . a; \\ var Bit . b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by a let multiple expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_let_sugar_multiple_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . var1, var2 <- term; term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by a let single expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_let_sugar_single_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . var1 <- term; term'" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let single expression followed by a lambda expressions" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_single_followed_by_labmdas.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = let { term = \\ var Bit . a } in \\ var Bit . b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by an if expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_if_else_associate_to_the_right.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . if c then t else f" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing an if expression followed by lambda expressions" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_lambdas.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if c then \\ var Bit . b else \\ var Bit . d" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by function composition expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_function_composition.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . term1 . term2" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expressions followed by function application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_followed_by_term_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . term1 term2" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing by function application follwowed by a lambda expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_application_followed_by_lambda_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term (\\ var Bit . term1 term2)" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with variable list argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_variable_list_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . v1, v2" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with multiple controls argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_multiple_controls_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . with [q0, q1] ctrl [@+, @-] gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with multiple classicaly controlled argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_multiple_classically_controlled_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . with [q0, q1] ctrl [0, 1] gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with single control argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_single_control_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . with [q0] ctrl [@+] gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with single classicaly controlled argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_single_classically_controlled_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . with [q0] ctrl [0] gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with tuple of variables argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_tuple_of_variables_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . (v1, v2)" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with tuple of terms argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_tuple_of_terms_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . (t1 t2, t)" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with variable argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_variable_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . v" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with list argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_list1_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . []" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with list argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_list2_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . [v]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with list argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_list3_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . [v1, v2]" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with gate argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_gate_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . gate H" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with integer argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_integer_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . 1 + 2" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with boolean expression argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_boolean_expression_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . True || False" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with unit argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_unit_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . ()" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with basis state argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_basis_state_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . @0" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing a lambda expression with list element argument" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/lambda_with_list_element_argument.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = \\ var Bit . [v1, v2] !! 2" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing two chained case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/two_chained_case_expressions.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> case y of { c1 -> c2 c3 -> c4 } }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by a let sugar multiple expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> var1, var2 <- term; term' }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple expression followed by a case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term; case x of { a1 -> a2 b1 -> b2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by a let sugar single expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_let_sugar_single.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> var <- term; term' }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar single expression followed by a case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_single_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var <- term; case x of { a1 -> a2 b1 -> b2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by an if else expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_if_else.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> if t then a else b }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by an if else expression followed by a case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "if t then a else case x of { a1 -> a2 b1 -> b2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by function composition" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_function_composition.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> f1 . f2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by an if else expression followed by a case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/function_composition_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = f . (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_term_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> t1 t2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by a term application expression followed by a case" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_application_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by variable list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_variable_list.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> v1, v2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by a variable list followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_list_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = v1, v2 (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by variable tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_variable_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> (v1, v2) }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by a variable tuple followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_tuple_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (v1, v2) (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by term tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_term_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> (t1 t2, t3)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing by a term tuple followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_tuple_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (t1 t2, t3) (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by classic gate controlled by vars" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_classic_ctrl_gate_controlled_by_vars.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [q0, q1] ctrl [1, 1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing classic gate controlled by vars followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_classic_vars_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0, q1] ctrl [1, 1] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by classic gate controlled by terms" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_classic_ctrl_gate_controlled_by_terms.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [t1 t2, t3] ctrl [1, 1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing classic gate controlled by terms followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_classic_terms_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [t1 t2, t3] ctrl [1, 1] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by classic gate controlled by term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_classic_ctrl_gate_controlled_by_term.lq"
        print (trimNewLines result)
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [t] ctrl [1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing classic gate controlled by term followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_classic_ctrl_term_followed_by_case.lq"
        print (trimNewLines result)
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [t] ctrl [1] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by quantum gate controlled by vars" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_quantum_ctrl_gate_controlled_by_vars.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [q0, q1] ctrl [@1, @1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing quantum gate controlled by vars followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_quantum_vars_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [q0, q1] ctrl [@1, @1] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by quantum gate controlled by terms" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_quantum_ctrl_gate_controlled_by_terms.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [t1 t2, t3] ctrl [@1, @1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing quantum gate controlled by terms followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_quantum_terms_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [t1 t2, t3] ctrl [@1, @1] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by quantum gate controlled by term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_quantum_ctrl_gate_controlled_by_term.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> with [t] ctrl [@1] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing quantum gate controlled by term followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_controlled_by_quantum_ctrl_term_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = with [t] ctrl [@+] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_term.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> b2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a term followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)
    
    Test.Hspec.context "when provided with a program containing a case expression followed by list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_list1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> [] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_list2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> [x] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_list3.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> [x, y] }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_followed_by_case1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_followed_by_case2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_followed_by_case3.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x, y] (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)   

    Test.Hspec.context "when provided with a program containing a case expression followed by gate" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_gate1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> gate H }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by gate" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_gate2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (case x of { a1 -> a2 b1 -> b2 } ) gate H" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a gate followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = gate H (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by integer expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_integer_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> 2 - 3 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a integer expression followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/integer_expression_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 3 * 7 (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by boolean expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_boolean_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> 2 > 3 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a boolean expression followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/boolean_expression_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 3 < 7 (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by basis state" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_basis_state.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> @-i }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a basis state followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/basis_state_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = @+i (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by unit" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_unit.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> () }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a unit followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/unit_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = () (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a case expression followed by list element" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_followed_by_list_element.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = case x of { a1 -> a2 b1 -> [] !! 2 }" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list element followed by a case expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_element_followed_by_case.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x] !! 1 (case x of { a1 -> a2 b1 -> b2 } )" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by let sugar multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_associativity.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; var3, var4 <- term2; term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by let sugar simple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; var3 <- term2; term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by let sugar multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; var2, var3 <- term2; term3" `isInfixOf` trimNewLines str)
  
    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by let multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_let_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; let { (var3, var4) = term2 } in term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by let single" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_let_single.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; let { var3 = term2 } in term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by let multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_let_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; let { (var2, var3) = term2 } in term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by let single" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_let_single.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; let { var2 = term2 } in term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by if else" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_if_else.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; if term then a else b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by if else" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_if_else.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; if term then a else b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a if else followed by let sugar multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_let_sugar_multiple1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if term then a else var1, var2 <- term1; term2;" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a if else followed by let sugar multiple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_let_sugar_multiple2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if term then var1, var2 <- term1; term2 else b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a if else followed by let sugar simple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_let_sugar_single1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if term then a else var <- term1; term2" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a if else followed by let sugar simple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else_followed_by_let_sugar_single2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = if term then var <- term1; term2 else b" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by function application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_function_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; term2 . term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by function application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_function_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; term2 . term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a function application followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/function_application_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term . (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a function application followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/function_application_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term . (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_term_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; term2 term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_term_application.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; term2 term3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a term application followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_application_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a term application followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_application_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = term (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by variable list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_variable_list.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; v1, v2" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by variable list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_variable_list.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; v1, v2" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable list followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_list_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = v1, v2 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable list followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_list_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = v1, v2 (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by variable tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_variable_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; (v1, v2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by variable tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_variable_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; (v1, v2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable tuple followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_tuple_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (v1, v2) (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable tuple followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_tuple_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (v1, v2) (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by term tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_term_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; (t1 t2, t3)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by term tuple" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_term_tuple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; (t1 t2, t3)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a term tuple followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_tuple_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (t1 t2, t3) (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a term tuple followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/term_tuple_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (t1 t2, t3) (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by variable" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_variable.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; var" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by variable" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_variable.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; var" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a variable followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/variable_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_list.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; [x, y]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by list" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_list.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; [x, y]" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x, y] (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x, y] (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by gate" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_gate.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; gate H" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by gate" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_gate.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; gate H" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a gate followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = gate H (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a gate followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/gate_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = gate H (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by unit" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_unit.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; ()" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by unit" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_unit.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; ()" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a unit followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/unit_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = () (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a unit followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/unit_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = () (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by list element" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_list_element.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; [x] !! 1" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by list element" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_list_element.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; [] !! 0" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list element followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_element_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [] !! 3 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a list element followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/list_element_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = [x, y] !! 2 (var1, var2 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by basis state" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_basis_state.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; @-" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by basis state" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_basis_state.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; @+" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a basis state followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/basis_state_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun :: Qbit; fun = @-i (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a basis state followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/basis_state_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = @+i (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by boolean expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_boolean_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; 2 /= 3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by boolean expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_boolean_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; 2 == 3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a boolean expression followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/boolean_expression_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 2 <= 3 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a boolean expression followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/boolean_expression_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 3 >= 2 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar simple term followed by integer expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_simple_followed_by_integer_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1 <- term1; 8 / 2" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a let sugar multiple term followed by integer expression" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/let_sugar_multiple_followed_by_integer_expression.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term1; 2 + 3" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a integer expression followed let sugar simple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/integer_expression_followed_by_let_sugar_simple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 2 * 3 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    Test.Hspec.context "when provided with a program containing a integer expression followed let sugar multiple term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/integer_expression_followed_by_let_sugar_multiple.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = 3 - 2 (var1 <- term1; term2)" `isInfixOf` trimNewLines str)

    --print (trimNewLines result)

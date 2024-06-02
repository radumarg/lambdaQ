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
        --result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ var1, var2 <- term; term'" `isInfixOf` trimNewLines str)
        print (trimNewLines result)

    Test.Hspec.context "when provided with a program containing '$' operator and a case term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
        result <- testParserReturnsTree "test/programs/good/check-terms-precedence/case_stronger_than_dollar_2.lq"
        --result `Test.Hspec.shouldSatisfy` (\str -> "fun = var1, var2 <- term; term' $ a" `isInfixOf` trimNewLines str)
        print (trimNewLines result)

    -- print (trimNewLines result)
    -- Test.Hspec.context "when provided with a program containing '$' operator and term application" $ do
    --   Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
    --     result <- testParserReturnsTree "test/programs/good/check-terms-precedence/dollar_less_than_apply_1.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) c" `isInfixOf` str)

    -- Test.Hspec.context "when provided with a program containing '$' operator and term application" $ do
    --   Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
    --     result <- testParserReturnsTree "test/programs/good/check-terms-precedence/dollar_less_than_apply_2.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b c" `isInfixOf` str)

    -- Test.Hspec.context "when provided with a valid coinflip program" $ do
    --   Test.Hspec.it "returns a parsed abstract syntax tree with redundant paranthesis removed" $ do
    --     result <- testParserReturnsTree "test/programs/good/check-terms-precedence/if_else.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "if a then b else c d" `isInfixOf` str)
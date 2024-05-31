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

testParserReturnOK :: FilePath -> IO String
testParserReturnOK filePath = runExceptT (runParser filePath)  >>= \case
                Left err -> return (show err)
                Right _ -> return "OK"

testParserReturnTree :: FilePath -> IO String
testParserReturnTree filePath = runExceptT (runParser filePath)  >>= \case
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
        testParserReturnOK "test/programs/examples/example00-CoinFlip.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Deutsch algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example01-deutschAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Deutsch Jozsa algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid phase kick back program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example03-phaseKickBack.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Bernstein Vazirani algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Simon algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example05-simonAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid teleportation protocol program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example06-teleportationProtocol.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid four qubit adder program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example07-fourQubitAdder.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Grover algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example08-groverAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid quantum phase estimation program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example09-quantumPhaseEstimation.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid Shor algorithm program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example10-shorAlgorithm.lq" `Test.Hspec.shouldReturn` "OK"

    Test.Hspec.context "when provided with a valid QFT program" $ do
      Test.Hspec.it "returns no error" $ do
        testParserReturnOK "test/programs/examples/example11-qft.lq" `Test.Hspec.shouldReturn` "OK"

  Test.Hspec.describe "Testing Lexer and Parser on various made up code fragments:" $ do

  -- EXPECT NO ERRORS --

    Test.Hspec.context "when provided with a program containing '$' and term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree containing: fun = (a $ b) c" $ do
        result <- testParserReturnTree "test/programs/good/check-terms-precedence/dollar_less_than_apply_1.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = (a $ b) c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' and term application" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree containing: fun = a $ b c" $ do
        result <- testParserReturnTree "test/programs/good/check-terms-precedence/dollar_less_than_apply_2.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ b c" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' follwed by an if else term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree containing: fun = a $ if c then t else f" $ do
        result <- testParserReturnTree "test/programs/good/check-terms-precedence/dollar_less_than_if_else.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ if c then t else f" `isInfixOf` str)

    Test.Hspec.context "when provided with a program containing '$' follwed by an let term" $ do
      Test.Hspec.it "returns a parsed abstract syntax tree containing: fun = a $ let { var = term } in term" $ do
        result <- testParserReturnTree "test/programs/good/check-terms-precedence/dollar_less_than_let.lq"
        result `Test.Hspec.shouldSatisfy` (\str -> "fun = a $ let { var = term } in term'" `isInfixOf` trimNewLines str)

    -- Test.Hspec.context "when provided with a valid coinflip program" $ do
    --   Test.Hspec.it "returns a parsed abstract syntax tree containing: if a then b else c d" $ do
    --     result <- testParserReturnTree "test/programs/good/check-terms-precedence/if_else.lq"
    --     result `Test.Hspec.shouldSatisfy` (\str -> "if a then b else c d" `isInfixOf` str)
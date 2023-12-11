{-# LANGUAGE LambdaCase #-}

module Backend.ASTtoIASTConverterSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec
import Control.Monad.Except (runExceptT)

import Backend.ASTtoIASTConverter (Program)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runIASTConversion :: FilePath -> Exec Program
runIASTConversion filePath = readTheFile filePath
                            >>= parseProgram
                            >>= semanticAnalysis
                            >>= convertAstToIast

testProgram :: FilePath -> IO String
testProgram filePath = runExceptT (runIASTConversion filePath)  >>= \case
                Left err -> return (show err)
                Right _ -> return "OK"

testProgramAndShowResults :: FilePath -> IO String
testProgramAndShowResults filePath = runExceptT (runIASTConversion filePath)  >>= \case
                Left err -> return (show err)
                Right program -> return $ cleanUpShowResults (show program)

cleanUpShowResults :: String -> String
cleanUpShowResults = filter (`notElem` "[]\"")

spec :: Spec
spec =  do
  describe "Testing AST to IAST Conversion:" $ do

    -- PROCESSING EXAMPLE PROGRAMS: EXPECT NO ERRORS --

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

    -- TESTING VARIOUS CODE FRAGMENTS: RETURNS VARIOUS SYNTAX TREES  --

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_0.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeUnit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermUnit" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_1.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "BitValue 1" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_2.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "BitValue 1" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_3.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermNew (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStatePlus" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_4.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStatePlusI" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_5.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear (TypeNonLinear TypeBit)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateZero" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_6.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_7.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

-- TODO: fix
    -- context "when provided with a small program fragment" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_8.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_9.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple (TermTuple (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit (BitValue 0)" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_10.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple (TermTuple (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,38),gate))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,45),q2)))" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_11.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit (BitValue 1)" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_12.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,38),gate))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,45),q2)))" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_13.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,27)) (TermBasisState BasisStateOne)) (TermTuple (TermApply (TermNew (2,35)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,43)) (TermBasisState BasisStateOne))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermQuantumCtrlsGate TermVariable (Var ((3,22),q1)),TermVariable (Var ((3,28),q2)) BasisStateOne,BasisStatePlusI)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,58),q3)))" `isInfixOf` str)

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_14.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,34)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,42)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,50)) (TermBasisState BasisStateZero)))))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermQuantumCtrlsGate TermVariable (Var ((3,22),q1)),TermVariable (Var ((3,28),q3)),TermVariable (Var ((3,34),q4)) BasisStateOne,BasisStateOne,BasisStateOne)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateY)) (TermVariable (Var ((3,67),q2)))" `isInfixOf` str)

  -- do let single
  -- do let sugar single
  -- do let multiple

    context "when provided with a small program fragment" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_18.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,7)) (TermBasisState BasisStateZero)" `isInfixOf` str)


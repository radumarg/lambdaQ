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

    context "when provided with a small program fragment(0)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_0.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeUnit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermUnit" `isInfixOf` str)

    context "when provided with a small program fragment(1)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_1.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "BitValue 1" `isInfixOf` str)

    context "when provided with a small program fragment(2)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_2.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "BitValue 1" `isInfixOf` str)

    context "when provided with a small program fragment(3)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_3.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermNew (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStatePlus" `isInfixOf` str)

    context "when provided with a small program fragment(4)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_4.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStatePlusI" `isInfixOf` str)

    context "when provided with a small program fragment(5)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_5.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear (TypeNonLinear TypeBit)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateZero" `isInfixOf` str)

    context "when provided with a small program fragment(6)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_6.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

    context "when provided with a small program fragment(7)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_7.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

-- TODO: fix
    -- context "when provided with a small program fragment(8)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_8.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

    context "when provided with a small program fragment(9)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_9.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple (TermTuple (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit (BitValue 0)" `isInfixOf` str)

    context "when provided with a small program fragment(10)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_10.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetMultiple (TermTuple (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,38),gate))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,45),q2)))" `isInfixOf` str)

    context "when provided with a small program fragment(11)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_11.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBit (BitValue 1)" `isInfixOf` str)

    context "when provided with a small program fragment(12)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_12.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,38),gate))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,45),q2)))" `isInfixOf` str)

    context "when provided with a small program fragment(13)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_13.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,27)) (TermBasisState BasisStateOne)) (TermTuple (TermApply (TermNew (2,35)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,43)) (TermBasisState BasisStateOne))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermQuantumCtrlsGate TermVariable (Var ((3,22),q1)),TermVariable (Var ((3,28),q2)) BasisStateOne,BasisStatePlusI)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateX)) (TermVariable (Var ((3,58),q3)))" `isInfixOf` str)

    context "when provided with a small program fragment(14)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_14.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,34)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,42)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,50)) (TermBasisState BasisStateZero)))))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermQuantumCtrlsGate TermVariable (Var ((3,22),q1)),TermVariable (Var ((3,28),q3)),TermVariable (Var ((3,34),q4)) BasisStateOne,BasisStateOne,BasisStateOne)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermGate GateY)) (TermVariable (Var ((3,67),q2)))" `isInfixOf` str)


    context "when provided with a small program fragment(15)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_15.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,16)) (TermBasisState BasisStateMinusI)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermMeasure (3,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermVariable (Var ((3,14),gate))) (TermGate GateX))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,21),q)))" `isInfixOf` str)

    context "when provided with a small program fragment(16)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_16.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermApply (TermNew (2,12)) (TermBasisState BasisStateZero)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermMeasure (3,7)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermVariable (Var ((3,14),gate))) (TermGate GateZ))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermVariable (Var ((3,21),q)))" `isInfixOf` str)

    context "when provided with a small program fragment(17)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_17.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (2,7)) (TermBasisState BasisStateZero)" `isInfixOf` str)

    context "when provided with a small program fragment(18)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_18.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit :**: 3" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermBasisState BasisStateZero" `isInfixOf` str)

    context "when provided with a small program fragment(19)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_19.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit :*: TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermTuple" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermApply (TermVariable (Var ((2,8),gate))) (TermGate GateH)) (TermNew (2,15))) (TermBasisState BasisStateZero))" `isInfixOf` str)

    context "when provided with a small program fragment(20)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_20.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermLetSingle" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "(TermMeasure (2,17)) (TermVariable (Var ((2,23),gate)))) (TermGate GateH)) (TermNew (2,30))) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermIfElse (TermVariable (Var ((3,10),bit))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermApply (TermApply (TermVariable (Var ((3,19),gate))) (TermGate GateH)) (TermNew (3,26))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermApply (TermNew (3,38)) (TermBasisState BasisStateOne)" `isInfixOf` str)

    context "when provided with a small program fragment(21)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_21.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeUnit TermUnit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "Function main (5,1) TypeUnit (TermVariable (Var ((5,8),fun)))" `isInfixOf` str)

    context "when provided with a small program fragment(22)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_22.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) TypeQbit (TermApply (TermNew (2,7)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "Function main (5,1) TypeQbit (TermVariable (Var ((5,8),fun)))" `isInfixOf` str)

    context "when provided with a small program fragment(23)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_23.lq"
        result `shouldSatisfy` (\str -> "Function fun (2,1) (TypeBit :->: TypeQbit)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermIfElse (TermVariable (Var ((2,14),bit))) (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,35)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "Function main (5,1) TypeQbit (TermApply (TermApply (TermVariable (Var ((5,8),fun))) (TermMeasure (5,12))) (TermApply (TermApply (TermApply (TermVariable (Var ((5,19),gate))) (TermGate GateH)) (TermNew (5,26))) (TermBasisState BasisStateZero)))" `isInfixOf` str)

    context "when provided with a small program fragment(24)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_24.lq"
        result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeBit :->: TypeBit)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeBit :->: TypeQbit)" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "Function main (8,1) TypeQbit" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TermIfElse (TermApply (TermVariable (Var ((5,15),fun1))) (TermBit (BitValue 0))) (TermApply (TermNew (5,27)) (TermBasisState BasisStateOne)) (TermApply (TermNew (5,39)) (TermBasisState BasisStateZero))" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "TypeQbit (TermApply (TermApply (TermVariable (Var ((8,8),fun2))) (TermMeasure (8,13))) (TermApply (TermApply (TermApply (TermVariable (Var ((8,20),gate))) (TermGate GateH)) (TermNew (8,27))) (TermBasisState BasisStateZero)))" `isInfixOf` str)

    -- TESTING VARIOUS CODE FRAGMENTS: FOCUS ON LAMBDA EXPRESSION MAPPING  --

    context "when provided with a small program fragment(25)" $ do
      it "returns a syntax tree processed in the intermediate syntax tree format" $ do
        result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_25.lq"
        result `shouldSatisfy` (\str -> "?" `isInfixOf` str)
        result `shouldSatisfy` (\str -> "?" `isInfixOf` str)
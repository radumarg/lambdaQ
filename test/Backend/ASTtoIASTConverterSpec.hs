{-# LANGUAGE LambdaCase #-}

module Backend.ASTtoIASTConverterSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec
import Control.Monad.Except (runExceptT)

import Backend.ASTtoIASTConverter (Program)
import CompilationEngine (Exec, readFileContents, parseProgram, semanticAnalysis, convertAstToIast)

-- import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runIASTConversion :: FilePath -> Exec Program
runIASTConversion filePath = readFileContents filePath
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
        testProgram "test/programs/examples/example00-CoinFlip.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Deutsch algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example01-deutschAlgorithm.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Deutsch Jozsa algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example02-deutschJozsaAlgorithm.lq" `shouldReturn` "OK"

    -- context "when provided with a valid phase kick back program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example03-phaseKickBack.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Bernstein Vazirani algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example04-bernsteinVaziraniAlgorithm.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Simon algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example05-simonAlgorithm.lq" `shouldReturn` "OK"

    -- context "when provided with a valid teleportation protocol program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example06-teleportationProtocol.lq" `shouldReturn` "OK"

    -- context "when provided with a valid four qubit adder program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example07-fourQubitAdder.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Grover algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example08-groverAlgorithm.lq" `shouldReturn` "OK"

    -- context "when provided with a valid quantum phase estimation program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example09-quantumPhaseEstimation.lq" `shouldReturn` "OK"

    -- context "when provided with a valid Shor algorithm program" $ do
    --   it "returns no error" $ do
    --     testProgram "test/programs/examples/example10-shorAlgorithm.lq" `shouldReturn` "OK"

    -- -- TESTING VARIOUS CODE FRAGMENTS: RETURNS VARIOUS SYNTAX TREES  --

    -- context "when provided with a small program fragment(0)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_0.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeUnit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermUnit" `isInfixOf` str)

    -- context "when provided with a small program fragment(1)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_1.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBit BitOne" `isInfixOf` str)

    -- context "when provided with a small program fragment(2)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_2.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBit BitZero" `isInfixOf` str)

    -- context "when provided with a small program fragment(3)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_3.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,7)) (TermBasisState BasisStatePlus)" `isInfixOf` str)

    -- context "when provided with a small program fragment(4)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_4.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)) (TermBasisState BasisStatePlusI))" `isInfixOf` str)

    -- context "when provided with a small program fragment(5)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_5.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear (TypeNonLinear TypeBit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,14)) (TermBasisState BasisStateZero)" `isInfixOf` str)

    -- context "when provided with a small program fragment(6)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_6.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermGate GateH) (TermApply (TermNew (2,22)) (TermBasisState BasisStateOne))" `isInfixOf` str)

    -- context "when provided with a small program fragment(7)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_7.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermGate GateH) (TermApply (TermNew (2,22)) (TermBasisState BasisStateMinus)))" `isInfixOf` str)

    -- -- TODO: fix
    -- -- context "when provided with a small program fragment(8)" $ do
    -- --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    -- --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_8.lq"
    -- --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    -- --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    -- --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (2,7)" `isInfixOf` str)
    -- --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermVariable (Var ((2,14),gate))) (TermGate GateH)" `isInfixOf` str)
    -- --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,22)" `isInfixOf` str)
    -- --     result `shouldSatisfy` (\str -> "TermBasisState BasisStateOne" `isInfixOf` str)

    -- context "when provided with a small program fragment(9)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_9.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBit BitZero" `isInfixOf` str)

    -- context "when provided with a small program fragment(10)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_10.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetMultiple" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,31)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (3,7)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermQuantumCtrlGate (TermBoundVariable 1) BasisStateOne) (TermGate GateX)) (TermBoundVariable 0)" `isInfixOf` str)

    -- context "when provided with a small program fragment(11)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_11.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermBit BitOne" `isInfixOf` str)
                                         
    -- context "when provided with a small program fragment(12)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_12.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarMultiple" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,18)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (3,7)) (TermApply (TermApply (TermQuantumCtrlGate (TermBoundVariable 1) BasisStateOne) (TermGate GateX)) (TermBoundVariable 0))" `isInfixOf` str)

    -- context "when provided with a small program fragment(13)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_13.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetMultiple (TermTuple (TermApply (TermNew (2,27)) (TermBasisState BasisStateOne)) (TermTuple (TermApply (TermNew (2,35)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,43)) (TermBasisState BasisStateOne)))) (TermApply (TermMeasure (3,7)) (TermApply (TermApply (TermQuantumCtrlsGate TermBoundVariable 2,TermBoundVariable 1 BasisStateOne,BasisStatePlusI) (TermGate GateX)) (TermBoundVariable 0)))" `isInfixOf` str)

    -- context "when provided with a small program fragment(14)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_14.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarMultiple" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,26)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,34)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,42)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,50)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (3,7))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermQuantumCtrlsGate TermBoundVariable 3,TermBoundVariable 1,TermBoundVariable 0 BasisStateOne,BasisStateOne,BasisStateOne) (TermGate GateY)) (TermBoundVariable 2)" `isInfixOf` str)

    -- context "when provided with a small program fragment(15)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_15.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,16)) (TermBasisState BasisStateMinusI)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSingle (TermApply (TermNew (2,16)) (TermBasisState BasisStateMinusI))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (3,7)) (TermApply (TermGate GateX) (TermBoundVariable 0))" `isInfixOf` str)

    -- context "when provided with a small program fragment(16)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_16.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeNonLinear (TypeNonLinear TypeBit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarSingle (TermApply (TermNew (2,12)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (3,7)) (TermApply (TermGate GateZ) (TermBoundVariable 0)))" `isInfixOf` str)

    -- context "when provided with a small program fragment(17)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_17.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermNew (2,7)) (TermBasisState BasisStateZero)" `isInfixOf` str)

    -- context "when provided with a small program fragment(18)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_18.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeQbit :**: 3" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,8)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,16)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,24)) (TermBasisState BasisStateZero)))" `isInfixOf` str)

    -- context "when provided with a small program fragment(19)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_19.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeQbit :*: TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermApply (TermGate GateH) (TermNew (2,15))) (TermBasisState BasisStateZero)) (TermApply (TermApply (TermGate GateH) (TermNew (2,30))) (TermBasisState BasisStateZero))" `isInfixOf` str)

    -- context "when provided with a small program fragment(20)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_20.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSingle (TermApply (TermApply (TermApply (TermMeasure (2,17)) (TermGate GateH)) (TermNew (2,30))) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermIfElse (TermBoundVariable 0) (TermApply (TermApply (TermGate GateH) (TermNew (3,26))) (TermBasisState BasisStateZero)) (TermApply (TermNew (3,38)) (TermBasisState BasisStateOne))" `isInfixOf` str)

    -- context "when provided with a small program fragment(21)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_21.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) TypeUnit TermUnit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (5,1) TypeUnit (TermFreeVariable fun)" `isInfixOf` str)

    -- context "when provided with a small program fragment(22)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_22.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) TypeQbit (TermApply (TermNew (2,7)) (TermBasisState BasisStateZero))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (5,1) TypeQbit (TermFreeVariable fun)" `isInfixOf` str)

    -- context "when provided with a small program fragment(23)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_23.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) (TypeBit :->: TypeQbit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermIfElse (TermBoundVariable 0) (TermApply (TermNew (2,23)) (TermBasisState BasisStateOne)) (TermApply (TermNew (2,35)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (5,1) TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermFreeVariable fun) (TermMeasure (5,12))) (TermApply (TermApply (TermGate GateH) (TermNew (5,26))) (TermBasisState BasisStateZero))" `isInfixOf` str)

    -- context "when provided with a small program fragment(24)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_24.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeBit :->: TypeBit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermIfElse (TermBoundVariable 0) (TermBit BitZero) (TermBit BitOne))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeBit :->: TypeQbit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermIfElse (TermApply (TermFreeVariable fun1) (TermBit BitZero)) (TermApply (TermNew (5,27)) (TermBasisState BasisStateOne)) (TermApply (TermNew (5,39)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (8,1) TypeQbit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermApply (TermFreeVariable fun2) (TermMeasure (8,13))) (TermApply (TermApply (TermGate GateH) (TermNew (8,27))) (TermBasisState BasisStateZero))" `isInfixOf` str)

    -- context "when provided with a small program fragment(25)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_25.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) (TypeBit :->: (TypeBit :->: TypeBit))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermLambda TypeBit (TermIfElse (TermBoundVariable 1) (TermBoundVariable 0) (TermBoundVariable 1)))" `isInfixOf` str)

    -- context "when provided with a small program fragment(26)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_26.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeBit :->: (TypeBit :->: TypeBit))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermLambda TypeBit (TermIfElse (TermBoundVariable 1) (TermBoundVariable 0) (TermBoundVariable 1)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeBit :->: TypeQbit :*: TypeQbit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermLetMultiple (TermIfElse (TermBoundVariable 0) (TermTuple (TermApply (TermNew (5,40)) (TermBasisState BasisStateZero)) (TermApply (TermNew (5,48)) (TermBasisState BasisStateZero))) (TermTuple (TermApply (TermNew (5,62)) (TermBasisState BasisStateOne)) (TermApply (TermNew (5,70)) (TermBasisState BasisStateOne)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermGate GateH) (TermBoundVariable 1)) (TermApply (TermGate GateH) (TermBoundVariable 0))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (9,1) (TypeQbit :*: TypeQbit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermFreeVariable fun2) (TermApply (TermApply (TermFreeVariable fun1) (TermBit BitZero)) (TermBit BitOne))" `isInfixOf` str)

    -- context "when provided with a small program fragment(27)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_27.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeBit :->: (TypeBit :->: TypeBit))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermLambda TypeBit (TermIfElse (TermBoundVariable 1) (TermBoundVariable 0) (TermBoundVariable 1)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeBit :->: (TypeQbit :->: TypeQbit))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermLetSugarMultiple (TermIfElse (TermBoundVariable 0) (TermTuple (TermApply (TermNew (5,35)) (TermBasisState BasisStateZero)) (TermApply (TermNew (5,43)) (TermBasisState BasisStateZero))) (TermTuple (TermApply (TermNew (5,57)) (TermBasisState BasisStateOne)) (TermApply (TermNew (5,65)) (TermBasisState BasisStateOne)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermGate GateH) (TermBoundVariable 1)) (TermApply (TermGate GateH) (TermBoundVariable 0))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (9,1) (TypeQbit :**: 2)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermFreeVariable fun2) (TermApply (TermApply (TermFreeVariable fun1) (TermBit BitZero)) (TermBit BitOne))" `isInfixOf` str)

    -- context "when provided with a small program fragment(28)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_28.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "(TermTuple (TermApply (TermNew (2,9)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,17)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,25)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeBit :->: TypeBit)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda TypeBit (TermBoundVariable 0)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (8,1) (TypeBit :**: 4)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarMultiple (TermFreeVariable fun1)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarSingle (TermApply (TermMeasure (9,14)) (TermBoundVariable 2)) (TermLetSugarSingle (TermApply (TermMeasure (10,14)) (TermBoundVariable 2)) (TermLetSugarSingle (TermApply (TermFreeVariable fun2) (TermBit BitZero)) (TermLetSugarSingle (TermApply (TermMeasure (12,14)) (TermBoundVariable 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermBoundVariable 3) (TermTuple (TermBoundVariable 2) (TermTuple (TermBoundVariable 1) (TermBoundVariable 0))" `isInfixOf` str)

    -- context "when provided with a small program fragment(29)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_29.lq"
    --     result `shouldSatisfy` (\str -> "Function fun (2,1) TypeBit" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermApply (TermGate GateH) (TermNew (2,33))) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermApply (TermGate GateH) (TermNew (2,48))) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermApply (TermGate GateH) (TermNew (2,63))) (TermBasisState BasisStateZero)) (TermApply (TermApply (TermGate GateH) (TermNew (2,78))) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLetSugarSingle (TermApply (TermMeasure (3,13)) (TermBoundVariable 3)) (TermLetSugarSingle (TermApply (TermMeasure (4,13)) (TermBoundVariable 3)) (TermLetSugarSingle (TermApply (TermMeasure (5,13)) (TermBoundVariable 3))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermApply (TermMeasure (6,7)) (TermApply (TermApply (TermClassicCtrlsGate TermBoundVariable 2,TermBoundVariable 1,TermBoundVariable 0 0,0,1) (TermGate GateY)) (TermBoundVariable 5))" `isInfixOf` str)

    -- context "when provided with a small program fragment(30)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_30.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,9)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,17)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,25)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeQbit :**: 3 :->: TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda (TypeQbit :**: 3) (TermApply (TermApply (TermFreeVariable qbit3) (TermFreeVariable qbit2)) (TermBoundVariable 0))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (8,1) (TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermCompose (TermFreeVariable fun2) (TermFreeVariable fun1)" `isInfixOf` str)

    -- context "when provided with a small program fragment(31)" $ do
    --   it "returns a syntax tree processed in the intermediate syntax tree format" $ do
    --     result <- testProgramAndShowResults "test/programs/code-fragments/program_fragment_31.lq"
    --     result `shouldSatisfy` (\str -> "Function fun1 (2,1) (TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermTuple (TermApply (TermNew (2,9)) (TermBasisState BasisStateZero)) (TermTuple (TermApply (TermNew (2,17)) (TermBasisState BasisStateZero)) (TermApply (TermNew (2,25)) (TermBasisState BasisStateZero)))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function fun2 (5,1) (TypeQbit :**: 3 :->: TypeQbit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda (TypeQbit :**: 3) (TermApply (TermApply (TermFreeVariable q3) (TermFreeVariable q2)) (TermBoundVariable 0))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function measureall (8,1) (TypeQbit :**: 3 :->: TypeBit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermLambda (TypeQbit :**: 3) (TermTuple (TermApply (TermMeasure (8,24)) (TermBoundVariable 0)) (TermTuple (TermApply (TermMeasure (8,34)) (TermFreeVariable q2)) (TermApply (TermMeasure (8,44)) (TermFreeVariable q3))))" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "Function main (11,1) (TypeBit :**: 3)" `isInfixOf` str)
    --     result `shouldSatisfy` (\str -> "TermDollar (TermFreeVariable measureall) (TermApply (TermFreeVariable fun2) (TermFreeVariable fun1))" `isInfixOf` str)


        

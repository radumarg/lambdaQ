-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Frontend.LambdaQ.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Frontend.LambdaQ.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transGateVar :: Frontend.LambdaQ.Abs.GateVar -> Result
transGateVar x = case x of
  Frontend.LambdaQ.Abs.GateVar string -> failure x

transVar :: Frontend.LambdaQ.Abs.Var -> Result
transVar x = case x of
  Frontend.LambdaQ.Abs.Var string -> failure x

transBit :: Frontend.LambdaQ.Abs.Bit -> Result
transBit x = case x of
  Frontend.LambdaQ.Abs.Bit string -> failure x

transLambda :: Frontend.LambdaQ.Abs.Lambda -> Result
transLambda x = case x of
  Frontend.LambdaQ.Abs.Lambda string -> failure x

transProgram :: Frontend.LambdaQ.Abs.Program -> Result
transProgram x = case x of
  Frontend.LambdaQ.Abs.ProgDef functiondeclarations -> failure x

transIntegerExpression :: Frontend.LambdaQ.Abs.IntegerExpression -> Result
transIntegerExpression x = case x of
  Frontend.LambdaQ.Abs.ArithmExprMinus integerexpression -> failure x
  Frontend.LambdaQ.Abs.ArithmExprAdd integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.ArithmExprSub integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.ArithmExprMul integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.ArithmExprDiv integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.ArithmExprInt integer -> failure x

transBoolValue :: Frontend.LambdaQ.Abs.BoolValue -> Result
transBoolValue x = case x of
  Frontend.LambdaQ.Abs.BoolValueTrue -> failure x
  Frontend.LambdaQ.Abs.BoolValueFalse -> failure x

transBoolExpression :: Frontend.LambdaQ.Abs.BoolExpression -> Result
transBoolExpression x = case x of
  Frontend.LambdaQ.Abs.BoolExpressionAnd boolexpression1 boolexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionOr boolexpression1 boolexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionNot boolexpression -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionEq boolexpression1 boolexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionDif boolexpression1 boolexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionEqInt integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionDifInt integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionGt integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionGe integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionLt integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionLe integerexpression1 integerexpression2 -> failure x
  Frontend.LambdaQ.Abs.BoolExpressionVal boolvalue -> failure x

transType :: Frontend.LambdaQ.Abs.Type -> Result
transType x = case x of
  Frontend.LambdaQ.Abs.TypeFunction type_1 type_2 -> failure x
  Frontend.LambdaQ.Abs.TypeTensorProd type_1 type_2 -> failure x
  Frontend.LambdaQ.Abs.TypeExp type_ integer -> failure x
  Frontend.LambdaQ.Abs.TypeNonLinear type_ -> failure x
  Frontend.LambdaQ.Abs.TypeBool -> failure x
  Frontend.LambdaQ.Abs.TypeBit -> failure x
  Frontend.LambdaQ.Abs.TypeInteger -> failure x
  Frontend.LambdaQ.Abs.TypeQbit -> failure x
  Frontend.LambdaQ.Abs.TypeBasisState -> failure x
  Frontend.LambdaQ.Abs.TypeUnit -> failure x
  Frontend.LambdaQ.Abs.TypeList type_ -> failure x

transAngle :: Frontend.LambdaQ.Abs.Angle -> Result
transAngle x = case x of
  Frontend.LambdaQ.Abs.AngleValue double -> failure x

transBasisState :: Frontend.LambdaQ.Abs.BasisState -> Result
transBasisState x = case x of
  Frontend.LambdaQ.Abs.BasisStateZero -> failure x
  Frontend.LambdaQ.Abs.BasisStateOne -> failure x
  Frontend.LambdaQ.Abs.BasisStatePlus -> failure x
  Frontend.LambdaQ.Abs.BasisStateMinus -> failure x
  Frontend.LambdaQ.Abs.BasisStatePlusI -> failure x
  Frontend.LambdaQ.Abs.BasisStateMinusI -> failure x

transGate :: Frontend.LambdaQ.Abs.Gate -> Result
transGate x = case x of
  Frontend.LambdaQ.Abs.GateH -> failure x
  Frontend.LambdaQ.Abs.GateX -> failure x
  Frontend.LambdaQ.Abs.GateY -> failure x
  Frontend.LambdaQ.Abs.GateZ -> failure x
  Frontend.LambdaQ.Abs.GateID -> failure x
  Frontend.LambdaQ.Abs.GateXRootInt integer -> failure x
  Frontend.LambdaQ.Abs.GateXRootVar var -> failure x
  Frontend.LambdaQ.Abs.GateXRootDagInt integer -> failure x
  Frontend.LambdaQ.Abs.GateXRootDagVar var -> failure x
  Frontend.LambdaQ.Abs.GateYRootInt integer -> failure x
  Frontend.LambdaQ.Abs.GateYRootVar var -> failure x
  Frontend.LambdaQ.Abs.GateYRootDagInt integer -> failure x
  Frontend.LambdaQ.Abs.GateYRootDagVar var -> failure x
  Frontend.LambdaQ.Abs.GateZRootInt integer -> failure x
  Frontend.LambdaQ.Abs.GateZRootVar var -> failure x
  Frontend.LambdaQ.Abs.GateZRootDagInt integer -> failure x
  Frontend.LambdaQ.Abs.GateZRootDagVar var -> failure x
  Frontend.LambdaQ.Abs.GateS -> failure x
  Frontend.LambdaQ.Abs.GateSDag -> failure x
  Frontend.LambdaQ.Abs.GateT -> failure x
  Frontend.LambdaQ.Abs.GateTDag -> failure x
  Frontend.LambdaQ.Abs.GateSqrtX -> failure x
  Frontend.LambdaQ.Abs.GateSqrtXDag -> failure x
  Frontend.LambdaQ.Abs.GateSqrtY -> failure x
  Frontend.LambdaQ.Abs.GateSqrtYDag -> failure x
  Frontend.LambdaQ.Abs.GateRxTheta angle -> failure x
  Frontend.LambdaQ.Abs.GateRyTheta angle -> failure x
  Frontend.LambdaQ.Abs.GateRzTheta angle -> failure x
  Frontend.LambdaQ.Abs.GateU1 angle -> failure x
  Frontend.LambdaQ.Abs.GateU2 angle1 angle2 -> failure x
  Frontend.LambdaQ.Abs.GateU3 angle1 angle2 angle3 -> failure x
  Frontend.LambdaQ.Abs.GateSwp -> failure x
  Frontend.LambdaQ.Abs.GateSqrtSwp -> failure x
  Frontend.LambdaQ.Abs.GateSqrtSwpDag -> failure x
  Frontend.LambdaQ.Abs.GateISwp -> failure x
  Frontend.LambdaQ.Abs.GateFSwp -> failure x
  Frontend.LambdaQ.Abs.GateSwpTheta angle -> failure x
  Frontend.LambdaQ.Abs.GateSwpRtInt integer -> failure x
  Frontend.LambdaQ.Abs.GateSwpRtVar var -> failure x
  Frontend.LambdaQ.Abs.GateSwpRtDagInt integer -> failure x
  Frontend.LambdaQ.Abs.GateSwpRtDagVar var -> failure x
  Frontend.LambdaQ.Abs.GateQftInt integer -> failure x
  Frontend.LambdaQ.Abs.GateQftVar var -> failure x
  Frontend.LambdaQ.Abs.GateQftDagInt integer -> failure x
  Frontend.LambdaQ.Abs.GateQftDagVar var -> failure x
  Frontend.LambdaQ.Abs.GateUnknown3Angle gatevar angle1 angle2 angle3 -> failure x
  Frontend.LambdaQ.Abs.GateUnknown2Angle gatevar angle1 angle2 -> failure x
  Frontend.LambdaQ.Abs.GateUnknown1Angle gatevar angle -> failure x
  Frontend.LambdaQ.Abs.GateUnknownInt gatevar integer -> failure x
  Frontend.LambdaQ.Abs.GateUnknownVar gatevar var -> failure x
  Frontend.LambdaQ.Abs.GateUnknownSimple gatevar -> failure x

transControlBasisState :: Frontend.LambdaQ.Abs.ControlBasisState -> Result
transControlBasisState x = case x of
  Frontend.LambdaQ.Abs.CtrlBasisState basisstate -> failure x

transControlBasisStates :: Frontend.LambdaQ.Abs.ControlBasisStates -> Result
transControlBasisStates x = case x of
  Frontend.LambdaQ.Abs.CtrlBasisStates basisstate basisstates -> failure x

transControlBit :: Frontend.LambdaQ.Abs.ControlBit -> Result
transControlBit x = case x of
  Frontend.LambdaQ.Abs.CtrlBit bit -> failure x

transControlBits :: Frontend.LambdaQ.Abs.ControlBits -> Result
transControlBits x = case x of
  Frontend.LambdaQ.Abs.CtrlBits bit bits -> failure x

transControlTerm :: Frontend.LambdaQ.Abs.ControlTerm -> Result
transControlTerm x = case x of
  Frontend.LambdaQ.Abs.CtrlTerm term -> failure x

transControlTerms :: Frontend.LambdaQ.Abs.ControlTerms -> Result
transControlTerms x = case x of
  Frontend.LambdaQ.Abs.CtrlTerms term terms -> failure x

transControlVar :: Frontend.LambdaQ.Abs.ControlVar -> Result
transControlVar x = case x of
  Frontend.LambdaQ.Abs.CtrlVar var -> failure x

transControlVars :: Frontend.LambdaQ.Abs.ControlVars -> Result
transControlVars x = case x of
  Frontend.LambdaQ.Abs.CtrlVars var vars -> failure x

transTerm :: Frontend.LambdaQ.Abs.Term -> Result
transTerm x = case x of
  Frontend.LambdaQ.Abs.TermListElement list integer -> failure x
  Frontend.LambdaQ.Abs.TermUnit -> failure x
  Frontend.LambdaQ.Abs.TermBit bit -> failure x
  Frontend.LambdaQ.Abs.TermBasisState basisstate -> failure x
  Frontend.LambdaQ.Abs.TermBoolExpression boolexpression -> failure x
  Frontend.LambdaQ.Abs.TermIntegerExpression integerexpression -> failure x
  Frontend.LambdaQ.Abs.TermGate gate -> failure x
  Frontend.LambdaQ.Abs.TermList list -> failure x
  Frontend.LambdaQ.Abs.TermVariable var -> failure x
  Frontend.LambdaQ.Abs.TermTuple term terms -> failure x
  Frontend.LambdaQ.Abs.TermGateQuantumCtrl controlterm controlbasisstate -> failure x
  Frontend.LambdaQ.Abs.TermGateQuantumTCtrls controlterms controlbasisstates -> failure x
  Frontend.LambdaQ.Abs.TermGateQuantumVCtrls controlvars controlbasisstates -> failure x
  Frontend.LambdaQ.Abs.TermGateClassicCtrl controlterm controlbit -> failure x
  Frontend.LambdaQ.Abs.TermGateClassicTCtrls controlterms controlbits -> failure x
  Frontend.LambdaQ.Abs.TermGateClassicVCtrls controlvars controlbits -> failure x
  Frontend.LambdaQ.Abs.TermApply term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermCompose term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermTensorProduct term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermIfElse term1 term2 term3 -> failure x
  Frontend.LambdaQ.Abs.TermLetSingle var term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermLetMultiple var vars term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermLetSugarSingle var term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermLetSugarMultiple var vars term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TermCase term caseexpressions -> failure x
  Frontend.LambdaQ.Abs.TermLambda lambda var type_ term -> failure x
  Frontend.LambdaQ.Abs.TermDollar term1 term2 -> failure x

transList :: Frontend.LambdaQ.Abs.List -> Result
transList x = case x of
  Frontend.LambdaQ.Abs.ListNil -> failure x
  Frontend.LambdaQ.Abs.ListSingle term -> failure x
  Frontend.LambdaQ.Abs.ListMultiple term terms -> failure x
  Frontend.LambdaQ.Abs.ListExpressionAdd list1 list2 -> failure x
  Frontend.LambdaQ.Abs.ListCons term list -> failure x

transCaseExpression :: Frontend.LambdaQ.Abs.CaseExpression -> Result
transCaseExpression x = case x of
  Frontend.LambdaQ.Abs.CaseExpr term1 term2 -> failure x

transArg :: Frontend.LambdaQ.Abs.Arg -> Result
transArg x = case x of
  Frontend.LambdaQ.Abs.FunArg var -> failure x

transFunctionDefinition :: Frontend.LambdaQ.Abs.FunctionDefinition -> Result
transFunctionDefinition x = case x of
  Frontend.LambdaQ.Abs.FunDef var args term -> failure x

transFunctionType :: Frontend.LambdaQ.Abs.FunctionType -> Result
transFunctionType x = case x of
  Frontend.LambdaQ.Abs.FunType var type_ -> failure x

transFunctionDeclaration :: Frontend.LambdaQ.Abs.FunctionDeclaration -> Result
transFunctionDeclaration x = case x of
  Frontend.LambdaQ.Abs.FunDecl functiontype functiondefinition -> failure x

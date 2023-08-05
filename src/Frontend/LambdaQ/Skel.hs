-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Frontend.LambdaQ.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Frontend.LambdaQ.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transBit :: Frontend.LambdaQ.Abs.Bit -> Result
transBit x = case x of
  Frontend.LambdaQ.Abs.Bit string -> failure x

transGateIdent :: Frontend.LambdaQ.Abs.GateIdent -> Result
transGateIdent x = case x of
  Frontend.LambdaQ.Abs.GateIdent string -> failure x

transVar :: Frontend.LambdaQ.Abs.Var -> Result
transVar x = case x of
  Frontend.LambdaQ.Abs.Var string -> failure x

transLambda :: Frontend.LambdaQ.Abs.Lambda -> Result
transLambda x = case x of
  Frontend.LambdaQ.Abs.Lambda string -> failure x

transProgram :: Frontend.LambdaQ.Abs.Program -> Result
transProgram x = case x of
  Frontend.LambdaQ.Abs.ProgDef functiondeclarations -> failure x

transType :: Frontend.LambdaQ.Abs.Type -> Result
transType x = case x of
  Frontend.LambdaQ.Abs.TypeBit -> failure x
  Frontend.LambdaQ.Abs.TypeQbit -> failure x
  Frontend.LambdaQ.Abs.TypeUnit -> failure x
  Frontend.LambdaQ.Abs.TypeNonLin type_ -> failure x
  Frontend.LambdaQ.Abs.TypeExp type_ integer -> failure x
  Frontend.LambdaQ.Abs.TypeTensr type_1 type_2 -> failure x
  Frontend.LambdaQ.Abs.TypeFunc type_1 type_2 -> failure x

transAngle :: Frontend.LambdaQ.Abs.Angle -> Result
transAngle x = case x of
  Frontend.LambdaQ.Abs.AAngl double -> failure x

transControlState :: Frontend.LambdaQ.Abs.ControlState -> Result
transControlState x = case x of
  Frontend.LambdaQ.Abs.CStateZero -> failure x
  Frontend.LambdaQ.Abs.CStateOne -> failure x
  Frontend.LambdaQ.Abs.CStatePlus -> failure x
  Frontend.LambdaQ.Abs.CStateMinus -> failure x
  Frontend.LambdaQ.Abs.CStatePlusI -> failure x
  Frontend.LambdaQ.Abs.CStateMinusI -> failure x

transGate :: Frontend.LambdaQ.Abs.Gate -> Result
transGate x = case x of
  Frontend.LambdaQ.Abs.GateH -> failure x
  Frontend.LambdaQ.Abs.GateX -> failure x
  Frontend.LambdaQ.Abs.GateY -> failure x
  Frontend.LambdaQ.Abs.GateZ -> failure x
  Frontend.LambdaQ.Abs.GateI -> failure x
  Frontend.LambdaQ.Abs.GateXRt integer -> failure x
  Frontend.LambdaQ.Abs.GateXRtDag integer -> failure x
  Frontend.LambdaQ.Abs.GateYRt integer -> failure x
  Frontend.LambdaQ.Abs.GateYRtDag integer -> failure x
  Frontend.LambdaQ.Abs.GateZRt integer -> failure x
  Frontend.LambdaQ.Abs.GateZRtDag integer -> failure x
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
  Frontend.LambdaQ.Abs.GateSwpRt integer -> failure x
  Frontend.LambdaQ.Abs.GateSwpRtDag integer -> failure x
  Frontend.LambdaQ.Abs.GateGeneric gateident -> failure x

transLetVariable :: Frontend.LambdaQ.Abs.LetVariable -> Result
transLetVariable x = case x of
  Frontend.LambdaQ.Abs.LetVar var -> failure x

transTuple :: Frontend.LambdaQ.Abs.Tuple -> Result
transTuple x = case x of
  Frontend.LambdaQ.Abs.Tup term terms -> failure x

transControls :: Frontend.LambdaQ.Abs.Controls -> Result
transControls x = case x of
  Frontend.LambdaQ.Abs.Ctrls term terms -> failure x

transControlStates :: Frontend.LambdaQ.Abs.ControlStates -> Result
transControlStates x = case x of
  Frontend.LambdaQ.Abs.CtrlStates controlstate controlstates -> failure x

transTerm :: Frontend.LambdaQ.Abs.Term -> Result
transTerm x = case x of
  Frontend.LambdaQ.Abs.TVar var -> failure x
  Frontend.LambdaQ.Abs.TBit bit -> failure x
  Frontend.LambdaQ.Abs.TTupl tuple -> failure x
  Frontend.LambdaQ.Abs.TUnit -> failure x
  Frontend.LambdaQ.Abs.TIfElse term1 term2 term3 -> failure x
  Frontend.LambdaQ.Abs.TLet letvariable letvariables term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TCase term caseexpression caseexpressions -> failure x
  Frontend.LambdaQ.Abs.TLambda lambda functiontype term -> failure x
  Frontend.LambdaQ.Abs.TGate gate -> failure x
  Frontend.LambdaQ.Abs.TCtrl controls controlstates -> failure x
  Frontend.LambdaQ.Abs.TApp term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TDollar term1 term2 -> failure x

transCaseExpression :: Frontend.LambdaQ.Abs.CaseExpression -> Result
transCaseExpression x = case x of
  Frontend.LambdaQ.Abs.CaseExp term var -> failure x

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

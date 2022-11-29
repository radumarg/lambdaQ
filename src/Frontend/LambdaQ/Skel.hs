-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Frontend.LambdaQ.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Frontend.LambdaQ.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transGateIdent :: Frontend.LambdaQ.Abs.GateIdent -> Result
transGateIdent x = case x of
  Frontend.LambdaQ.Abs.GateIdent string -> failure x

transVar :: Frontend.LambdaQ.Abs.Var -> Result
transVar x = case x of
  Frontend.LambdaQ.Abs.Var string -> failure x

transFunVariable :: Frontend.LambdaQ.Abs.FunVariable -> Result
transFunVariable x = case x of
  Frontend.LambdaQ.Abs.FunVariable string -> failure x

transLambda :: Frontend.LambdaQ.Abs.Lambda -> Result
transLambda x = case x of
  Frontend.LambdaQ.Abs.Lambda string -> failure x

transType :: Frontend.LambdaQ.Abs.Type -> Result
transType x = case x of
  Frontend.LambdaQ.Abs.TypeBit -> failure x
  Frontend.LambdaQ.Abs.TypeQbit -> failure x
  Frontend.LambdaQ.Abs.TypeUnit -> failure x
  Frontend.LambdaQ.Abs.TypeExp type_ -> failure x
  Frontend.LambdaQ.Abs.TypeTensrs type_ integer -> failure x
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
  Frontend.LambdaQ.Abs.CStateIPlus -> failure x
  Frontend.LambdaQ.Abs.CStateIMinus -> failure x

transControl :: Frontend.LambdaQ.Abs.Control -> Result
transControl x = case x of
  Frontend.LambdaQ.Abs.CCtrl integer controlstate -> failure x

transGate :: Frontend.LambdaQ.Abs.Gate -> Result
transGate x = case x of
  Frontend.LambdaQ.Abs.GH controls -> failure x
  Frontend.LambdaQ.Abs.GX controls -> failure x
  Frontend.LambdaQ.Abs.GY controls -> failure x
  Frontend.LambdaQ.Abs.GZ controls -> failure x
  Frontend.LambdaQ.Abs.GI controls -> failure x
  Frontend.LambdaQ.Abs.GXRt integer controls -> failure x
  Frontend.LambdaQ.Abs.GXRtDag integer controls -> failure x
  Frontend.LambdaQ.Abs.GYRt integer controls -> failure x
  Frontend.LambdaQ.Abs.GYRtDag integer controls -> failure x
  Frontend.LambdaQ.Abs.GZRt integer controls -> failure x
  Frontend.LambdaQ.Abs.GZRtDag integer controls -> failure x
  Frontend.LambdaQ.Abs.GS controls -> failure x
  Frontend.LambdaQ.Abs.GSDag controls -> failure x
  Frontend.LambdaQ.Abs.GT controls -> failure x
  Frontend.LambdaQ.Abs.GTDag controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtX controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtXDag controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtY controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtYDag controls -> failure x
  Frontend.LambdaQ.Abs.GRxTheta angle controls -> failure x
  Frontend.LambdaQ.Abs.GRyTheta angle controls -> failure x
  Frontend.LambdaQ.Abs.GRzTheta angle controls -> failure x
  Frontend.LambdaQ.Abs.GU1 angle controls -> failure x
  Frontend.LambdaQ.Abs.GU2 angle1 angle2 controls -> failure x
  Frontend.LambdaQ.Abs.GU3 angle1 angle2 angle3 controls -> failure x
  Frontend.LambdaQ.Abs.GSwp controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtSwp controls -> failure x
  Frontend.LambdaQ.Abs.GSqrtSwpDag controls -> failure x
  Frontend.LambdaQ.Abs.GISwp controls -> failure x
  Frontend.LambdaQ.Abs.GFSwp controls -> failure x
  Frontend.LambdaQ.Abs.GSwpRt integer controls -> failure x
  Frontend.LambdaQ.Abs.GSwpRtDag integer controls -> failure x
  Frontend.LambdaQ.Abs.GGate gateident -> failure x

transLetVariable :: Frontend.LambdaQ.Abs.LetVariable -> Result
transLetVariable x = case x of
  Frontend.LambdaQ.Abs.LVar var -> failure x

transTuple :: Frontend.LambdaQ.Abs.Tuple -> Result
transTuple x = case x of
  Frontend.LambdaQ.Abs.Tup term terms -> failure x

transBit :: Frontend.LambdaQ.Abs.Bit -> Result
transBit x = case x of
  Frontend.LambdaQ.Abs.BBit integer -> failure x

transTerm :: Frontend.LambdaQ.Abs.Term -> Result
transTerm x = case x of
  Frontend.LambdaQ.Abs.TVar var -> failure x
  Frontend.LambdaQ.Abs.TBit bit -> failure x
  Frontend.LambdaQ.Abs.TGate gate -> failure x
  Frontend.LambdaQ.Abs.TTup tuple -> failure x
  Frontend.LambdaQ.Abs.TUnit -> failure x
  Frontend.LambdaQ.Abs.TApp term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TIfEl term1 term2 term3 -> failure x
  Frontend.LambdaQ.Abs.TLet letvariable letvariables term1 term2 -> failure x
  Frontend.LambdaQ.Abs.TCase term1 term2 var1 term3 var2 -> failure x
  Frontend.LambdaQ.Abs.TLmbd lambda funvariable type_ term -> failure x
  Frontend.LambdaQ.Abs.TDollr term1 term2 -> failure x

transArg :: Frontend.LambdaQ.Abs.Arg -> Result
transArg x = case x of
  Frontend.LambdaQ.Abs.FunArg var -> failure x

transFunction :: Frontend.LambdaQ.Abs.Function -> Result
transFunction x = case x of
  Frontend.LambdaQ.Abs.FunDef var args term -> failure x

transFunDeclaration :: Frontend.LambdaQ.Abs.FunDeclaration -> Result
transFunDeclaration x = case x of
  Frontend.LambdaQ.Abs.FunDecl funvariable type_ function -> failure x

transProgram :: Frontend.LambdaQ.Abs.Program -> Result
transProgram x = case x of
  Frontend.LambdaQ.Abs.ProgDef fundeclarations -> failure x

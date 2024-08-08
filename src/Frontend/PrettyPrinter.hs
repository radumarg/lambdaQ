module Frontend.PrettyPrinter (
    showTerm,
    showTerms
) where

import Frontend.LambdaQ.Abs

-- Helper function to show a list of terms
showTerms :: [Term] -> String
showTerms [] = ""
showTerms [t] = showTerm t
showTerms (t:ts) = showTerm t ++ ", " ++ showTerms ts

-- Pretty printer for Term data type
showTerm :: Term -> String
showTerm (TermListElement lst idx) = showTermList lst ++ " !! " ++ show idx
showTerm TermUnit = "()"
showTerm (TermBasisState bs) = showBasisState bs
showTerm (TermBoolExpression be) = showBoolExpression be
showTerm (TermIntegerExpression ie) = showIntegerExpression ie
showTerm (TermGate g) = showGate g
showTerm (TermList lst) = showTermList lst
showTerm (TermVariable v) = showVar v
showTerm (TermTuple t ts) = "(" ++ showTerm t ++ ", " ++ showTerms ts ++ ")"
showTerm (TermQuantumCtrlGate ct cbs) = showControlTerm ct ++ " @ " ++ showControlBasisState cbs
showTerm (TermQuantumTCtrlsGate cts cbss) = showControlTerms cts ++ " @ " ++ showControlBasisStates cbss
showTerm (TermQuantumVCtrlsGate cvs cbss) = showControlVars cvs ++ " @ " ++ showControlBasisStates cbss
showTerm (TermClassicCtrlGate ct cb) = showControlTerm ct ++ " # " ++ showControlBit cb
showTerm (TermClassicTCtrlsGate cts cbs) = showControlTerms cts ++ " # " ++ showControlBits cbs
showTerm (TermClassicVCtrlsGate cvs cbs) = showControlVars cvs ++ " # " ++ showControlBits cbs
showTerm (TermApply t1 t2) = showTerm t1 ++ " " ++ showTerm t2
showTerm (TermCompose t1 t2) = showTerm t1 ++ " . " ++ showTerm t2
showTerm (TermTensorProduct t1 t2) = showTerm t1 ++ " * " ++ showTerm t2
showTerm (TermIfElse t1 t2 t3) = "if " ++ showTerm t1 ++ " then " ++ showTerm t2 ++ " else " ++ showTerm t3
showTerm (TermLetSingle v t1 t2) = "let " ++ showVar v ++ " = " ++ showTerm t1 ++ " in " ++ showTerm t2
showTerm (TermLetMultiple v vs t1 t2) = "let " ++ showVar v ++ " " ++ showVars vs ++ " = " ++ showTerm t1 ++ " in " ++ showTerm t2
showTerm (TermLetSugarSingle v t1 t2) = "let " ++ showVar v ++ " = " ++ showTerm t1 ++ " in " ++ showTerm t2
showTerm (TermLetSugarMultiple v vs t1 t2) = "let " ++ showVar v ++ " " ++ showVars vs ++ " = " ++ showTerm t1 ++ " in " ++ showTerm t2
showTerm (TermCase t ces) = "case " ++ showTerm t ++ " of " ++ showCaseExpressions ces
showTerm (TermLambda (Lambda l) v typ t) = "\\" ++ l ++ " " ++ showVar v ++ " : " ++ showType typ ++ " -> " ++ showTerm t
showTerm (TermDollar t1 t2) = showTerm t1 ++ " $ " ++ showTerm t2

-- Helper functions for other data types
showTermList :: List -> String
showTermList ListNil = "[]"
showTermList (ListSingle t) = "[" ++ showTerm t ++ "]"
showTermList (ListMultiple t ts) = "[" ++ showTerm t ++ ", " ++ showTerms ts ++ "]"
showTermList (ListExpressionAdd lst1 lst2) = showTermList lst1 ++ " ++ " ++ showTermList lst2
showTermList (ListCons t lst) = showTerm t ++ " : " ++ showTermList lst

showBasisState :: BasisState -> String
showBasisState BasisStateZero = "0"
showBasisState BasisStateOne = "1"
showBasisState BasisStatePlus = "+"
showBasisState BasisStateMinus = "-"
showBasisState BasisStatePlusI = "+i"
showBasisState BasisStateMinusI = "-i"

showBoolExpression :: BoolExpression -> String
showBoolExpression (BoolExpressionAnd be1 be2) = showBoolExpression be1 ++ " && " ++ showBoolExpression be2
showBoolExpression (BoolExpressionOr be1 be2) = showBoolExpression be1 ++ " || " ++ showBoolExpression be2
showBoolExpression (BoolExpressionNot be) = "not " ++ showBoolExpression be
showBoolExpression (BoolExpressionEq be1 be2) = showBoolExpression be1 ++ " == " ++ showBoolExpression be2
showBoolExpression (BoolExpressionDif be1 be2) = showBoolExpression be1 ++ " /= " ++ showBoolExpression be2
showBoolExpression (BoolExpressionEqInt ie1 ie2) = showIntegerExpression ie1 ++ " == " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionDifInt ie1 ie2) = showIntegerExpression ie1 ++ " /= " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionGt ie1 ie2) = showIntegerExpression ie1 ++ " > " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionGe ie1 ie2) = showIntegerExpression ie1 ++ " >= " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionLt ie1 ie2) = showIntegerExpression ie1 ++ " < " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionLe ie1 ie2) = showIntegerExpression ie1 ++ " <= " ++ showIntegerExpression ie2
showBoolExpression (BoolExpressionVal BoolValueTrue) = "true"
showBoolExpression (BoolExpressionVal BoolValueFalse) = "false"

showIntegerExpression :: IntegerExpression -> String
showIntegerExpression (ArithmExprMinus ie) = "-" ++ showIntegerExpression ie
showIntegerExpression (ArithmExprAdd ie1 ie2) = showIntegerExpression ie1 ++ " + " ++ showIntegerExpression ie2
showIntegerExpression (ArithmExprSub ie1 ie2) = showIntegerExpression ie1 ++ " - " ++ showIntegerExpression ie2
showIntegerExpression (ArithmExprMul ie1 ie2) = showIntegerExpression ie1 ++ " * " ++ showIntegerExpression ie2
showIntegerExpression (ArithmExprDiv ie1 ie2) = showIntegerExpression ie1 ++ " / " ++ showIntegerExpression ie2
showIntegerExpression (ArithmExprInt i) = show i

showGate :: Gate -> String
showGate GateH = "H"
showGate GateX = "X"
showGate GateY = "Y"
showGate GateZ = "Z"
showGate GateID = "ID"
showGate (GateXRootInt i) = "XRoot(" ++ show i ++ ")"
showGate (GateXRootVar v) = "XRoot(" ++ showVar v ++ ")"
showGate (GateXRootDagInt i) = "XRootDag(" ++ show i ++ ")"
showGate (GateXRootDagVar v) = "XRootDag(" ++ showVar v ++ ")"
showGate (GateYRootInt i) = "YRoot(" ++ show i ++ ")"
showGate (GateYRootVar v) = "YRoot(" ++ showVar v ++ ")"
showGate (GateYRootDagInt i) = "YRootDag(" ++ show i ++ ")"
showGate (GateYRootDagVar v) = "YRootDag(" ++ showVar v ++ ")"
showGate (GateZRootInt i) = "ZRoot(" ++ show i ++ ")"
showGate (GateZRootVar v) = "ZRoot(" ++ showVar v ++ ")"
showGate (GateZRootDagInt i) = "ZRootDag(" ++ show i ++ ")"
showGate (GateZRootDagVar v) = "ZRootDag(" ++ showVar v ++ ")"
showGate GateS = "S"
showGate GateSDag = "SDag"
showGate GateT = "T"
showGate GateTDag = "TDag"
showGate GateSqrtX = "SqrtX"
showGate GateSqrtXDag = "SqrtXDag"
showGate GateSqrtY = "SqrtY"
showGate GateSqrtYDag = "SqrtYDag"
showGate (GateRxTheta a) = "Rx(" ++ showAngle a ++ ")"
showGate (GateRyTheta a) = "Ry(" ++ showAngle a ++ ")"
showGate (GateRzTheta a) = "Rz(" ++ showAngle a ++ ")"
showGate (GateU1 a) = "U1(" ++ showAngle a ++ ")"
showGate (GateU2 a1 a2) = "U2(" ++ showAngle a1 ++ ", " ++ showAngle a2 ++ ")"
showGate (GateU3 a1 a2 a3) = "U3(" ++ showAngle a1 ++ ", " ++ showAngle a2 ++ ", " ++ showAngle a3 ++ ")"
showGate GateSwp = "Swp"
showGate GateSqrtSwp = "SqrtSwp"
showGate GateSqrtSwpDag = "SqrtSwpDag"
showGate GateISwp = "ISwp"
showGate GateFSwp = "FSwp"
showGate (GateSwpTheta a) = "Swp(" ++ showAngle a ++ ")"
showGate (GateSwpRtInt i) = "SwpRoot(" ++ show i ++ ")"
showGate (GateSwpRtVar v) = "SwpRoot(" ++ showVar v ++ ")"
showGate (GateSwpRtDagInt i) = "SwpRootDag(" ++ show i ++ ")"
showGate (GateSwpRtDagVar v) = "SwpRootDag(" ++ showVar v ++ ")"
showGate (GateQftInt i) = "Qft(" ++ show i ++ ")"
showGate (GateQftVar v) = "Qft(" ++ showVar v ++ ")"
showGate (GateQftDagInt i) = "QftDag(" ++ show i ++ ")"
showGate (GateQftDagVar v) = "QftDag(" ++ showVar v ++ ")"
showGate (GateUnknown3Angle gv a1 a2 a3) = showGateVar gv ++ "(" ++ showAngle a1 ++ ", " ++ showAngle a2 ++ ", " ++ showAngle a3 ++ ")"
showGate (GateUnknown2Angle gv a1 a2) = showGateVar gv ++ "(" ++ showAngle a1 ++ ", " ++ showAngle a2 ++ ")"
showGate (GateUnknown1Angle gv a) = showGateVar gv ++ "(" ++ showAngle a ++ ")"
showGate (GateUnknownInt gv i) = showGateVar gv ++ "(" ++ show i ++ ")"
showGate (GateUnknownVar gv v) = showGateVar gv ++ "(" ++ showVar v ++ ")"
showGate (GateUnknownSimple gv) = showGateVar gv

showVar :: Var -> String
showVar (Var (_, s)) = s

showVars :: [Var] -> String
showVars = unwords . map showVar

showControlTerm :: ControlTerm -> String
showControlTerm (CtrlTerm t) = showTerm t

showControlTerms :: ControlTerms -> String
showControlTerms (CtrlTerms t ts) = showTerm t ++ " " ++ showTerms ts

showControlVar :: ControlVar -> String
showControlVar (CtrlVar v) = showVar v

showControlVars :: ControlVars -> String
showControlVars (CtrlVars v vs) = showVar v ++ " " ++ showVars vs

showControlBasisState :: ControlBasisState -> String
showControlBasisState (CtrlBasisState bs) = showBasisState bs

showControlBasisStates :: ControlBasisStates -> String
showControlBasisStates (CtrlBasisStates bs bss) = showBasisState bs ++ " " ++ unwords (map showBasisState bss)

showControlBit :: ControlBit -> String
showControlBit (CtrlBit i) = show i

showControlBits :: ControlBits -> String
showControlBits (CtrlBits i is) = show i ++ " " ++ unwords (map show is)

showGateVar :: GateVar -> String
showGateVar (GateVar (_, s)) = s

showCaseExpression :: CaseExpression -> String
showCaseExpression (CaseExpr t1 t2) = showTerm t1 ++ " -> " ++ showTerm t2

showCaseExpressions :: [CaseExpression] -> String
showCaseExpressions = unlines . map showCaseExpression

showType :: Type -> String
showType TypeBool = "Bool"
showType TypeBit = "Bit"
showType TypeInteger = "Int"
showType TypeQbit = "Qbit"
showType TypeUnit = "Unit"
showType (TypeFunction t1 t2) = showType t1 ++ " -> " ++ showType t2
showType (TypeTensorProd t1 t2) = showType t1 ++ " * " ++ showType t2
showType (TypeExp t i) = showType t ++ "^" ++ show i
showType (TypeNonLinear t) = "!" ++ showType t
showType (TypeList t) = "[" ++ showType t ++ "]"

showAngle :: Angle -> String
showAngle (AngleValue a) = show a

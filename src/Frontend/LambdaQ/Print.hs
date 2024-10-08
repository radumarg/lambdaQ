-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Frontend.

module Frontend.LambdaQ.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Frontend.LambdaQ.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Frontend.LambdaQ.Abs.GateVar where
  prt _ (Frontend.LambdaQ.Abs.GateVar (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Var where
  prt _ (Frontend.LambdaQ.Abs.Var (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Bit where
  prt _ (Frontend.LambdaQ.Abs.Bit i) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Lambda where
  prt _ (Frontend.LambdaQ.Abs.Lambda i) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Program where
  prt i = \case
    Frontend.LambdaQ.Abs.ProgDef functiondeclarations -> prPrec i 0 (concatD [prt 0 functiondeclarations])

instance Print Frontend.LambdaQ.Abs.IntegerExpression where
  prt i = \case
    Frontend.LambdaQ.Abs.ArithmExprMinus integerexpression -> prPrec i 1 (concatD [doc (showString "-"), prt 0 integerexpression])
    Frontend.LambdaQ.Abs.ArithmExprAdd integerexpression1 integerexpression2 -> prPrec i 1 (concatD [prt 1 integerexpression1, doc (showString "+"), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.ArithmExprSub integerexpression1 integerexpression2 -> prPrec i 1 (concatD [prt 1 integerexpression1, doc (showString "-"), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.ArithmExprMul integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "*"), prt 3 integerexpression2])
    Frontend.LambdaQ.Abs.ArithmExprDiv integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "/"), prt 3 integerexpression2])
    Frontend.LambdaQ.Abs.ArithmExprInt n -> prPrec i 3 (concatD [prt 0 n])

instance Print Frontend.LambdaQ.Abs.BoolValue where
  prt i = \case
    Frontend.LambdaQ.Abs.BoolValueTrue -> prPrec i 0 (concatD [doc (showString "True")])
    Frontend.LambdaQ.Abs.BoolValueFalse -> prPrec i 0 (concatD [doc (showString "False")])

instance Print Frontend.LambdaQ.Abs.BoolExpression where
  prt i = \case
    Frontend.LambdaQ.Abs.BoolExpressionAnd boolexpression1 boolexpression2 -> prPrec i 0 (concatD [prt 0 boolexpression1, doc (showString "&&"), prt 1 boolexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionOr boolexpression1 boolexpression2 -> prPrec i 0 (concatD [prt 0 boolexpression1, doc (showString "||"), prt 1 boolexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionNot boolexpression -> prPrec i 0 (concatD [doc (showString "not"), prt 1 boolexpression])
    Frontend.LambdaQ.Abs.BoolExpressionEq boolexpression1 boolexpression2 -> prPrec i 1 (concatD [prt 1 boolexpression1, doc (showString "=="), prt 2 boolexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionDif boolexpression1 boolexpression2 -> prPrec i 1 (concatD [prt 1 boolexpression1, doc (showString "/="), prt 2 boolexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionEqInt integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "=="), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionDifInt integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "/="), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionGt integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString ">"), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionGe integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString ">="), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionLt integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "<"), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionLe integerexpression1 integerexpression2 -> prPrec i 2 (concatD [prt 2 integerexpression1, doc (showString "<="), prt 2 integerexpression2])
    Frontend.LambdaQ.Abs.BoolExpressionVal boolvalue -> prPrec i 2 (concatD [prt 0 boolvalue])

instance Print Frontend.LambdaQ.Abs.Type where
  prt i = \case
    Frontend.LambdaQ.Abs.TypeFunction type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
    Frontend.LambdaQ.Abs.TypeTensorProd type_1 type_2 -> prPrec i 2 (concatD [prt 2 type_1, doc (showString "*"), prt 3 type_2])
    Frontend.LambdaQ.Abs.TypeExp type_ n -> prPrec i 3 (concatD [prt 4 type_, doc (showString "**"), prt 0 n])
    Frontend.LambdaQ.Abs.TypeNonLinear type_ -> prPrec i 3 (concatD [doc (showString "!"), prt 4 type_])
    Frontend.LambdaQ.Abs.TypeBool -> prPrec i 5 (concatD [doc (showString "Bool")])
    Frontend.LambdaQ.Abs.TypeBit -> prPrec i 5 (concatD [doc (showString "Bit")])
    Frontend.LambdaQ.Abs.TypeInteger -> prPrec i 5 (concatD [doc (showString "Int")])
    Frontend.LambdaQ.Abs.TypeQbit -> prPrec i 5 (concatD [doc (showString "Qbit")])
    Frontend.LambdaQ.Abs.TypeBasisState -> prPrec i 5 (concatD [doc (showString "BasisState")])
    Frontend.LambdaQ.Abs.TypeUnit -> prPrec i 5 (concatD [doc (showString "()")])
    Frontend.LambdaQ.Abs.TypeList type_ -> prPrec i 5 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.Angle where
  prt i = \case
    Frontend.LambdaQ.Abs.AngleValue d -> prPrec i 0 (concatD [prt 0 d])

instance Print Frontend.LambdaQ.Abs.BasisState where
  prt i = \case
    Frontend.LambdaQ.Abs.BasisStateZero -> prPrec i 0 (concatD [doc (showString "@0")])
    Frontend.LambdaQ.Abs.BasisStateOne -> prPrec i 0 (concatD [doc (showString "@1")])
    Frontend.LambdaQ.Abs.BasisStatePlus -> prPrec i 0 (concatD [doc (showString "@+")])
    Frontend.LambdaQ.Abs.BasisStateMinus -> prPrec i 0 (concatD [doc (showString "@-")])
    Frontend.LambdaQ.Abs.BasisStatePlusI -> prPrec i 0 (concatD [doc (showString "@+i")])
    Frontend.LambdaQ.Abs.BasisStateMinusI -> prPrec i 0 (concatD [doc (showString "@-i")])

instance Print Frontend.LambdaQ.Abs.Gate where
  prt i = \case
    Frontend.LambdaQ.Abs.GateH -> prPrec i 0 (concatD [doc (showString "H")])
    Frontend.LambdaQ.Abs.GateX -> prPrec i 0 (concatD [doc (showString "X")])
    Frontend.LambdaQ.Abs.GateY -> prPrec i 0 (concatD [doc (showString "Y")])
    Frontend.LambdaQ.Abs.GateZ -> prPrec i 0 (concatD [doc (showString "Z")])
    Frontend.LambdaQ.Abs.GateID -> prPrec i 0 (concatD [doc (showString "ID")])
    Frontend.LambdaQ.Abs.GateXRootInt n -> prPrec i 0 (concatD [doc (showString "ROOT_X"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateXRootVar var -> prPrec i 0 (concatD [doc (showString "ROOT_X"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateXRootDagInt n -> prPrec i 0 (concatD [doc (showString "ROOT_X_DAG"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateXRootDagVar var -> prPrec i 0 (concatD [doc (showString "ROOT_X_DAG"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRootInt n -> prPrec i 0 (concatD [doc (showString "ROOT_Y"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRootVar var -> prPrec i 0 (concatD [doc (showString "ROOT_Y"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRootDagInt n -> prPrec i 0 (concatD [doc (showString "ROOT_Y_DAG"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRootDagVar var -> prPrec i 0 (concatD [doc (showString "ROOT_Y_DAG"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRootInt n -> prPrec i 0 (concatD [doc (showString "ROOT_Z"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRootVar var -> prPrec i 0 (concatD [doc (showString "ROOT_Z"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRootDagInt n -> prPrec i 0 (concatD [doc (showString "ROOT_Z_DAG"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRootDagVar var -> prPrec i 0 (concatD [doc (showString "ROOT_Z_DAG"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateS -> prPrec i 0 (concatD [doc (showString "S")])
    Frontend.LambdaQ.Abs.GateSDag -> prPrec i 0 (concatD [doc (showString "S_DAG")])
    Frontend.LambdaQ.Abs.GateT -> prPrec i 0 (concatD [doc (showString "T")])
    Frontend.LambdaQ.Abs.GateTDag -> prPrec i 0 (concatD [doc (showString "T_DAG")])
    Frontend.LambdaQ.Abs.GateSqrtX -> prPrec i 0 (concatD [doc (showString "SQRT_X")])
    Frontend.LambdaQ.Abs.GateSqrtXDag -> prPrec i 0 (concatD [doc (showString "SQRT_X_DAG")])
    Frontend.LambdaQ.Abs.GateSqrtY -> prPrec i 0 (concatD [doc (showString "SQRT_Y")])
    Frontend.LambdaQ.Abs.GateSqrtYDag -> prPrec i 0 (concatD [doc (showString "SQRT_Y_DAG")])
    Frontend.LambdaQ.Abs.GateRxTheta angle -> prPrec i 0 (concatD [doc (showString "RX"), doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateRyTheta angle -> prPrec i 0 (concatD [doc (showString "RY"), doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateRzTheta angle -> prPrec i 0 (concatD [doc (showString "RZ"), doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU1 angle -> prPrec i 0 (concatD [doc (showString "U1"), doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU2 angle1 angle2 -> prPrec i 0 (concatD [doc (showString "U2"), doc (showString "["), prt 0 angle1, doc (showString ","), prt 0 angle2, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU3 angle1 angle2 angle3 -> prPrec i 0 (concatD [doc (showString "U3"), doc (showString "["), prt 0 angle1, doc (showString ","), prt 0 angle2, doc (showString ","), prt 0 angle3, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwp -> prPrec i 0 (concatD [doc (showString "SWAP")])
    Frontend.LambdaQ.Abs.GateSqrtSwp -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP")])
    Frontend.LambdaQ.Abs.GateSqrtSwpDag -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP_DAG")])
    Frontend.LambdaQ.Abs.GateISwp -> prPrec i 0 (concatD [doc (showString "ISWAP")])
    Frontend.LambdaQ.Abs.GateFSwp -> prPrec i 0 (concatD [doc (showString "FSWAP")])
    Frontend.LambdaQ.Abs.GateSwpTheta angle -> prPrec i 0 (concatD [doc (showString "SWAP_THETA"), doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpRtInt n -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpRtVar var -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpRtDagInt n -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP_DAG"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpRtDagVar var -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP_DAG"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateQftInt n -> prPrec i 0 (concatD [doc (showString "QFT"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateQftVar var -> prPrec i 0 (concatD [doc (showString "QFT"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateQftDagInt n -> prPrec i 0 (concatD [doc (showString "QFT_DAG"), doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateQftDagVar var -> prPrec i 0 (concatD [doc (showString "QFT_DAG"), doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknown3Angle gatevar angle1 angle2 angle3 -> prPrec i 0 (concatD [prt 0 gatevar, doc (showString "["), prt 0 angle1, doc (showString ","), prt 0 angle2, doc (showString ","), prt 0 angle3, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknown2Angle gatevar angle1 angle2 -> prPrec i 0 (concatD [prt 0 gatevar, doc (showString "["), prt 0 angle1, doc (showString ","), prt 0 angle2, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknown1Angle gatevar angle -> prPrec i 0 (concatD [prt 0 gatevar, doc (showString "["), prt 0 angle, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknownInt gatevar n -> prPrec i 0 (concatD [prt 0 gatevar, doc (showString "["), prt 0 n, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknownVar gatevar var -> prPrec i 0 (concatD [prt 0 gatevar, doc (showString "["), prt 0 var, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateUnknownSimple gatevar -> prPrec i 0 (concatD [prt 0 gatevar])

instance Print [Frontend.LambdaQ.Abs.Var] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Frontend.LambdaQ.Abs.Bit] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.ControlBasisState where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlBasisState basisstate -> prPrec i 0 (concatD [doc (showString "["), prt 0 basisstate, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.ControlBasisStates where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlBasisStates basisstate basisstates -> prPrec i 0 (concatD [doc (showString "["), prt 0 basisstate, doc (showString ","), prt 0 basisstates, doc (showString "]")])

instance Print [Frontend.LambdaQ.Abs.BasisState] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.ControlBit where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlBit bit -> prPrec i 0 (concatD [doc (showString "["), prt 0 bit, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.ControlBits where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlBits bit bits -> prPrec i 0 (concatD [doc (showString "["), prt 0 bit, doc (showString ","), prt 0 bits, doc (showString "]")])

instance Print [Integer] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.ControlTerm where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlTerm term -> prPrec i 0 (concatD [doc (showString "["), prt 0 term, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.ControlTerms where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlTerms term terms -> prPrec i 0 (concatD [doc (showString "["), prt 0 term, doc (showString ","), prt 0 terms, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.ControlVar where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlVar var -> prPrec i 0 (concatD [doc (showString "["), prt 0 var, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.ControlVars where
  prt i = \case
    Frontend.LambdaQ.Abs.CtrlVars var vars -> prPrec i 0 (concatD [doc (showString "["), prt 0 var, doc (showString ","), prt 0 vars, doc (showString "]")])

instance Print [Frontend.LambdaQ.Abs.Term] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.Term where
  prt i = \case
    Frontend.LambdaQ.Abs.TermListElement list n -> prPrec i 4 (concatD [prt 0 list, doc (showString "!!"), prt 0 n])
    Frontend.LambdaQ.Abs.TermUnit -> prPrec i 3 (concatD [doc (showString "()")])
    Frontend.LambdaQ.Abs.TermBit bit -> prPrec i 3 (concatD [prt 0 bit])
    Frontend.LambdaQ.Abs.TermBasisState basisstate -> prPrec i 3 (concatD [prt 0 basisstate])
    Frontend.LambdaQ.Abs.TermBoolExpression boolexpression -> prPrec i 3 (concatD [prt 0 boolexpression])
    Frontend.LambdaQ.Abs.TermIntegerExpression integerexpression -> prPrec i 3 (concatD [prt 0 integerexpression])
    Frontend.LambdaQ.Abs.TermGate gate -> prPrec i 3 (concatD [doc (showString "gate"), prt 0 gate])
    Frontend.LambdaQ.Abs.TermList list -> prPrec i 3 (concatD [prt 0 list])
    Frontend.LambdaQ.Abs.TermVariable var -> prPrec i 3 (concatD [prt 0 var])
    Frontend.LambdaQ.Abs.TermTuple term terms -> prPrec i 3 (concatD [doc (showString "("), prt 0 term, doc (showString ","), prt 0 terms, doc (showString ")")])
    Frontend.LambdaQ.Abs.TermGateQuantumCtrl controlterm controlbasisstate -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlterm, doc (showString "ctrl"), prt 0 controlbasisstate])
    Frontend.LambdaQ.Abs.TermGateQuantumTCtrls controlterms controlbasisstates -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlterms, doc (showString "ctrl"), prt 0 controlbasisstates])
    Frontend.LambdaQ.Abs.TermGateQuantumVCtrls controlvars controlbasisstates -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlvars, doc (showString "ctrl"), prt 0 controlbasisstates])
    Frontend.LambdaQ.Abs.TermGateClassicCtrl controlterm controlbit -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlterm, doc (showString "ctrl"), prt 0 controlbit])
    Frontend.LambdaQ.Abs.TermGateClassicTCtrls controlterms controlbits -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlterms, doc (showString "ctrl"), prt 0 controlbits])
    Frontend.LambdaQ.Abs.TermGateClassicVCtrls controlvars controlbits -> prPrec i 2 (concatD [doc (showString "with"), prt 0 controlvars, doc (showString "ctrl"), prt 0 controlbits])
    Frontend.LambdaQ.Abs.TermApply term1 term2 -> prPrec i 2 (concatD [prt 2 term1, prt 3 term2])
    Frontend.LambdaQ.Abs.TermCompose term1 term2 -> prPrec i 2 (concatD [prt 2 term1, doc (showString "."), prt 3 term2])
    Frontend.LambdaQ.Abs.TermTensorProduct term1 term2 -> prPrec i 2 (concatD [prt 2 term1, doc (showString "*"), prt 3 term2])
    Frontend.LambdaQ.Abs.TermIfElse term1 term2 term3 -> prPrec i 1 (concatD [doc (showString "if"), prt 0 term1, doc (showString "then"), prt 0 term2, doc (showString "else"), prt 0 term3])
    Frontend.LambdaQ.Abs.TermLetSingle var term1 term2 -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "{"), prt 0 var, doc (showString "="), prt 0 term1, doc (showString "}"), doc (showString "in"), prt 0 term2])
    Frontend.LambdaQ.Abs.TermLetMultiple var vars term1 term2 -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "{"), doc (showString "("), prt 0 var, doc (showString ","), prt 0 vars, doc (showString ")"), doc (showString "="), prt 0 term1, doc (showString "}"), doc (showString "in"), prt 0 term2])
    Frontend.LambdaQ.Abs.TermLetSugarSingle var term1 term2 -> prPrec i 1 (concatD [prt 0 var, doc (showString "<-"), prt 0 term1, doc (showString ";"), prt 0 term2])
    Frontend.LambdaQ.Abs.TermLetSugarMultiple var vars term1 term2 -> prPrec i 1 (concatD [doc (showString "|"), prt 0 var, doc (showString ","), prt 0 vars, doc (showString "|"), doc (showString "<-"), prt 0 term1, doc (showString ";"), prt 0 term2])
    Frontend.LambdaQ.Abs.TermCase term caseexpressions -> prPrec i 1 (concatD [doc (showString "case"), prt 0 term, doc (showString "of"), doc (showString "{"), prt 0 caseexpressions, doc (showString "}")])
    Frontend.LambdaQ.Abs.TermLambda lambda var type_ term -> prPrec i 1 (concatD [prt 0 lambda, prt 0 var, prt 0 type_, doc (showString "."), prt 0 term])
    Frontend.LambdaQ.Abs.TermDollar term1 term2 -> prPrec i 0 (concatD [prt 1 term1, doc (showString "$"), prt 0 term2])

instance Print Frontend.LambdaQ.Abs.List where
  prt i = \case
    Frontend.LambdaQ.Abs.ListNil -> prPrec i 1 (concatD [doc (showString "[]")])
    Frontend.LambdaQ.Abs.ListSingle term -> prPrec i 1 (concatD [doc (showString "["), prt 0 term, doc (showString "]")])
    Frontend.LambdaQ.Abs.ListMultiple term terms -> prPrec i 1 (concatD [doc (showString "["), prt 0 term, doc (showString ","), prt 0 terms, doc (showString "]")])
    Frontend.LambdaQ.Abs.ListExpressionAdd list1 list2 -> prPrec i 0 (concatD [prt 0 list1, doc (showString "++"), prt 1 list2])
    Frontend.LambdaQ.Abs.ListCons term list -> prPrec i 0 (concatD [prt 3 term, doc (showString ":"), prt 1 list])

instance Print Frontend.LambdaQ.Abs.CaseExpression where
  prt i = \case
    Frontend.LambdaQ.Abs.CaseExpr term1 term2 -> prPrec i 0 (concatD [prt 0 term1, doc (showString "->"), prt 0 term2])

instance Print [Frontend.LambdaQ.Abs.CaseExpression] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Frontend.LambdaQ.Abs.Arg where
  prt i = \case
    Frontend.LambdaQ.Abs.FunArg var -> prPrec i 0 (concatD [prt 0 var])

instance Print [Frontend.LambdaQ.Abs.Arg] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.FunctionDefinition where
  prt i = \case
    Frontend.LambdaQ.Abs.FunDef var args term -> prPrec i 0 (concatD [prt 0 var, prt 0 args, doc (showString "="), prt 0 term])

instance Print Frontend.LambdaQ.Abs.FunctionType where
  prt i = \case
    Frontend.LambdaQ.Abs.FunType var type_ -> prPrec i 0 (concatD [prt 0 var, doc (showString "::"), prt 0 type_])

instance Print Frontend.LambdaQ.Abs.FunctionDeclaration where
  prt i = \case
    Frontend.LambdaQ.Abs.FunDecl functiontype functiondefinition -> prPrec i 0 (concatD [prt 0 functiontype, doc (showString ";"), prt 0 functiondefinition, doc (showString ";")])

instance Print [Frontend.LambdaQ.Abs.FunctionDeclaration] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

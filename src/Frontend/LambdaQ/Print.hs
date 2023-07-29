-- File generated by the BNF Converter (bnfc 2.9.4.1).

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
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
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

instance Print Frontend.LambdaQ.Abs.GateIdent where
  prt _ (Frontend.LambdaQ.Abs.GateIdent (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Var where
  prt _ (Frontend.LambdaQ.Abs.Var (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Lambda where
  prt _ (Frontend.LambdaQ.Abs.Lambda (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Bit where
  prt _ (Frontend.LambdaQ.Abs.Bit (_,i)) = doc $ showString i
instance Print Frontend.LambdaQ.Abs.Program where
  prt i = \case
    Frontend.LambdaQ.Abs.ProgDef functiondeclarations -> prPrec i 0 (concatD [prt 0 functiondeclarations])

instance Print Frontend.LambdaQ.Abs.Type where
  prt i = \case
    Frontend.LambdaQ.Abs.TypeBit -> prPrec i 3 (concatD [doc (showString "Bit")])
    Frontend.LambdaQ.Abs.TypeQbit -> prPrec i 3 (concatD [doc (showString "Qbit")])
    Frontend.LambdaQ.Abs.TypeUnit -> prPrec i 3 (concatD [doc (showString "()")])
    Frontend.LambdaQ.Abs.TypeNonLin type_ -> prPrec i 2 (concatD [doc (showString "!"), prt 3 type_])
    Frontend.LambdaQ.Abs.TypeExp type_ n -> prPrec i 1 (concatD [prt 2 type_, doc (showString "**"), prt 0 n])
    Frontend.LambdaQ.Abs.TypeTensr type_1 type_2 -> prPrec i 1 (concatD [prt 2 type_1, doc (showString "*"), prt 1 type_2])
    Frontend.LambdaQ.Abs.TypeFunc type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print Frontend.LambdaQ.Abs.Angle where
  prt i = \case
    Frontend.LambdaQ.Abs.AAngl d -> prPrec i 0 (concatD [prt 0 d])

instance Print Frontend.LambdaQ.Abs.ControlState where
  prt i = \case
    Frontend.LambdaQ.Abs.CStateZero -> prPrec i 0 (concatD [doc (showString "@0")])
    Frontend.LambdaQ.Abs.CStateOne -> prPrec i 0 (concatD [doc (showString "@1")])
    Frontend.LambdaQ.Abs.CStatePlus -> prPrec i 0 (concatD [doc (showString "@+")])
    Frontend.LambdaQ.Abs.CStateMinus -> prPrec i 0 (concatD [doc (showString "@-")])
    Frontend.LambdaQ.Abs.CStatePlusI -> prPrec i 0 (concatD [doc (showString "@+i")])
    Frontend.LambdaQ.Abs.CStateMinusI -> prPrec i 0 (concatD [doc (showString "@-i")])

instance Print Frontend.LambdaQ.Abs.Control where
  prt i = \case
    Frontend.LambdaQ.Abs.CCtrl controlstate term -> prPrec i 0 (concatD [prt 0 controlstate, doc (showString "->"), prt 0 term])

instance Print [Frontend.LambdaQ.Abs.Control] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.Gate where
  prt i = \case
    Frontend.LambdaQ.Abs.GateH -> prPrec i 0 (concatD [doc (showString "H")])
    Frontend.LambdaQ.Abs.GateHC controls -> prPrec i 0 (concatD [doc (showString "H"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateX -> prPrec i 0 (concatD [doc (showString "X")])
    Frontend.LambdaQ.Abs.GateXC controls -> prPrec i 0 (concatD [doc (showString "X"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateY -> prPrec i 0 (concatD [doc (showString "Y")])
    Frontend.LambdaQ.Abs.GateYC controls -> prPrec i 0 (concatD [doc (showString "Y"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZ -> prPrec i 0 (concatD [doc (showString "Z")])
    Frontend.LambdaQ.Abs.GateZC controls -> prPrec i 0 (concatD [doc (showString "Z"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateI -> prPrec i 0 (concatD [doc (showString "ID")])
    Frontend.LambdaQ.Abs.GateXRt n -> prPrec i 0 (concatD [doc (showString "ROOT_X"), prt 0 n])
    Frontend.LambdaQ.Abs.GateXRtC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_X"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateXRtDag n -> prPrec i 0 (concatD [doc (showString "ROOT_X_DAG"), prt 0 n])
    Frontend.LambdaQ.Abs.GateXRtDagC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_X_DAG"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRt n -> prPrec i 0 (concatD [doc (showString "ROOT_Y"), prt 0 n])
    Frontend.LambdaQ.Abs.GateYRtC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_Y"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateYRtDag n -> prPrec i 0 (concatD [doc (showString "ROOT_Y_DAG"), prt 0 n])
    Frontend.LambdaQ.Abs.GateYRtDagC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_Y_DAG"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRt n -> prPrec i 0 (concatD [doc (showString "ROOT_Z"), prt 0 n])
    Frontend.LambdaQ.Abs.GateZRtC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_Z"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateZRtDag n -> prPrec i 0 (concatD [doc (showString "ROOT_Z_DAG"), prt 0 n])
    Frontend.LambdaQ.Abs.GateZRtDagC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_Z_DAG"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateS -> prPrec i 0 (concatD [doc (showString "S")])
    Frontend.LambdaQ.Abs.GateSC controls -> prPrec i 0 (concatD [doc (showString "S"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSDag -> prPrec i 0 (concatD [doc (showString "S_DAG")])
    Frontend.LambdaQ.Abs.GateSDagC controls -> prPrec i 0 (concatD [doc (showString "S_DAG"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateT -> prPrec i 0 (concatD [doc (showString "T")])
    Frontend.LambdaQ.Abs.GateTC controls -> prPrec i 0 (concatD [doc (showString "T"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateTDag -> prPrec i 0 (concatD [doc (showString "T_DAG")])
    Frontend.LambdaQ.Abs.GateTDagC controls -> prPrec i 0 (concatD [doc (showString "T_DAG"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtX -> prPrec i 0 (concatD [doc (showString "SQRT_X")])
    Frontend.LambdaQ.Abs.GateSqrtXC controls -> prPrec i 0 (concatD [doc (showString "SQRT_X"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtXDag -> prPrec i 0 (concatD [doc (showString "SQRT_X_DAG")])
    Frontend.LambdaQ.Abs.GateSqrtXDagC controls -> prPrec i 0 (concatD [doc (showString "SQRT_X_DAG"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtY -> prPrec i 0 (concatD [doc (showString "SQRT_Y")])
    Frontend.LambdaQ.Abs.GateSqrtYC controls -> prPrec i 0 (concatD [doc (showString "SQRT_Y"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtYDag -> prPrec i 0 (concatD [doc (showString "SQRT_Y_DAG")])
    Frontend.LambdaQ.Abs.GateSqrtYDagC controls -> prPrec i 0 (concatD [doc (showString "SQRT_Y_DAG"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateRxTheta angle -> prPrec i 0 (concatD [doc (showString "RX"), prt 0 angle])
    Frontend.LambdaQ.Abs.GateRxThetaC angle controls -> prPrec i 0 (concatD [doc (showString "RX"), prt 0 angle, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateRyTheta angle -> prPrec i 0 (concatD [doc (showString "RY"), prt 0 angle])
    Frontend.LambdaQ.Abs.GateRyThetaC angle controls -> prPrec i 0 (concatD [doc (showString "RY"), prt 0 angle, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateRzTheta angle -> prPrec i 0 (concatD [doc (showString "RZ"), prt 0 angle])
    Frontend.LambdaQ.Abs.GateRzThetaC angle controls -> prPrec i 0 (concatD [doc (showString "RZ"), prt 0 angle, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU1 angle -> prPrec i 0 (concatD [doc (showString "U1"), prt 0 angle])
    Frontend.LambdaQ.Abs.GateU1C angle controls -> prPrec i 0 (concatD [doc (showString "U1"), prt 0 angle, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU2 angle1 angle2 -> prPrec i 0 (concatD [doc (showString "U2"), prt 0 angle1, prt 0 angle2])
    Frontend.LambdaQ.Abs.GateU2C angle1 angle2 controls -> prPrec i 0 (concatD [doc (showString "U2"), prt 0 angle1, prt 0 angle2, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateU3 angle1 angle2 angle3 -> prPrec i 0 (concatD [doc (showString "U3"), prt 0 angle1, prt 0 angle2, prt 0 angle3])
    Frontend.LambdaQ.Abs.GateU3C angle1 angle2 angle3 controls -> prPrec i 0 (concatD [doc (showString "U3"), prt 0 angle1, prt 0 angle2, prt 0 angle3, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwp -> prPrec i 0 (concatD [doc (showString "SWAP")])
    Frontend.LambdaQ.Abs.GateSwpC controls -> prPrec i 0 (concatD [doc (showString "SWAP"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtSwp -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP")])
    Frontend.LambdaQ.Abs.GateSqrtSwpC controls -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSqrtSwpDag -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP_DAG")])
    Frontend.LambdaQ.Abs.GateSqrtSwpDagC controls -> prPrec i 0 (concatD [doc (showString "SQRT_SWAP_DAG"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateISwp -> prPrec i 0 (concatD [doc (showString "ISWAP")])
    Frontend.LambdaQ.Abs.GateISwpC controls -> prPrec i 0 (concatD [doc (showString "ISWAP"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateFSwp -> prPrec i 0 (concatD [doc (showString "FSWAP")])
    Frontend.LambdaQ.Abs.GateFSwpC controls -> prPrec i 0 (concatD [doc (showString "FSWAP"), doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpTheta angle -> prPrec i 0 (concatD [doc (showString "SWAP_THETA"), prt 0 angle])
    Frontend.LambdaQ.Abs.GateSwpRt n -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP"), prt 0 n])
    Frontend.LambdaQ.Abs.GateSwpRtC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateSwpRtDag n -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP_DAG"), prt 0 n])
    Frontend.LambdaQ.Abs.GateSwpRtDagC n controls -> prPrec i 0 (concatD [doc (showString "ROOT_SWAP_DAG"), prt 0 n, doc (showString "["), prt 0 controls, doc (showString "]")])
    Frontend.LambdaQ.Abs.GateGeneric gateident -> prPrec i 0 (concatD [prt 0 gateident])
    Frontend.LambdaQ.Abs.GateGenericC gateident controls -> prPrec i 0 (concatD [prt 0 gateident, doc (showString "["), prt 0 controls, doc (showString "]")])

instance Print Frontend.LambdaQ.Abs.LetVariable where
  prt i = \case
    Frontend.LambdaQ.Abs.LetVar var -> prPrec i 0 (concatD [prt 0 var])

instance Print [Frontend.LambdaQ.Abs.LetVariable] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.LambdaVariable where
  prt i = \case
    Frontend.LambdaQ.Abs.LambdaVar var -> prPrec i 0 (concatD [prt 0 var])

instance Print [Frontend.LambdaQ.Abs.LambdaVariable] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.Tuple where
  prt i = \case
    Frontend.LambdaQ.Abs.Tup term terms -> prPrec i 0 (concatD [doc (showString "("), prt 0 term, doc (showString ","), prt 0 terms, doc (showString ")")])

instance Print [Frontend.LambdaQ.Abs.Term] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Frontend.LambdaQ.Abs.Term where
  prt i = \case
    Frontend.LambdaQ.Abs.TVar var -> prPrec i 3 (concatD [prt 0 var])
    Frontend.LambdaQ.Abs.TBit bit -> prPrec i 3 (concatD [prt 0 bit])
    Frontend.LambdaQ.Abs.TGate gate -> prPrec i 3 (concatD [prt 0 gate])
    Frontend.LambdaQ.Abs.TTup tuple -> prPrec i 3 (concatD [prt 0 tuple])
    Frontend.LambdaQ.Abs.TUnit -> prPrec i 3 (concatD [doc (showString "()")])
    Frontend.LambdaQ.Abs.TIfEl term1 term2 term3 -> prPrec i 1 (concatD [doc (showString "if"), prt 0 term1, doc (showString "then"), prt 0 term2, doc (showString "else"), prt 0 term3])
    Frontend.LambdaQ.Abs.TLet1 letvariable term1 term2 -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "{"), prt 0 letvariable, doc (showString "="), prt 0 term1, doc (showString "}"), doc (showString "in"), prt 0 term2])
    Frontend.LambdaQ.Abs.TLet2 letvariable letvariables term1 term2 -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "{"), doc (showString "("), prt 0 letvariable, doc (showString ","), prt 0 letvariables, doc (showString ")"), doc (showString "="), prt 0 term1, doc (showString "}"), doc (showString "in"), prt 0 term2])
    Frontend.LambdaQ.Abs.TCase term caseexpression caseexpressions -> prPrec i 1 (concatD [doc (showString "case"), prt 0 term, doc (showString "of"), prt 0 caseexpression, prt 0 caseexpressions])
    Frontend.LambdaQ.Abs.TLmbd lambda lambdavariable lambdavariables term -> prPrec i 1 (concatD [prt 0 lambda, prt 0 lambdavariable, prt 0 lambdavariables, doc (showString "->"), prt 0 term])
    Frontend.LambdaQ.Abs.TApp term1 term2 -> prPrec i 2 (concatD [prt 2 term1, prt 3 term2])
    Frontend.LambdaQ.Abs.TDollr term1 term2 -> prPrec i 1 (concatD [prt 2 term1, doc (showString "$"), prt 1 term2])

instance Print Frontend.LambdaQ.Abs.CaseExpression where
  prt i = \case
    Frontend.LambdaQ.Abs.CaseExp term var -> prPrec i 0 (concatD [prt 0 term, doc (showString "->"), prt 0 var])

instance Print [Frontend.LambdaQ.Abs.CaseExpression] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x, doc (showString " ")]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

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

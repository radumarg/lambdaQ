-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Grammar.

module Grammar.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Grammar.Abs

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

instance Print Grammar.Abs.GateGeneric where
  prt _ (Grammar.Abs.GateGeneric i) = doc $ showString i
instance Print Grammar.Abs.Var where
  prt _ (Grammar.Abs.Var (_,i)) = doc $ showString i
instance Print Grammar.Abs.FunVariable where
  prt _ (Grammar.Abs.FunVariable (_,i)) = doc $ showString i
instance Print Grammar.Abs.Lambda where
  prt _ (Grammar.Abs.Lambda (_,i)) = doc $ showString i
instance Print Grammar.Abs.Type where
  prt i = \case
    Grammar.Abs.TypeBit -> prPrec i 1 (concatD [doc (showString "Bit")])
    Grammar.Abs.TypeQbit -> prPrec i 1 (concatD [doc (showString "Qbit")])
    Grammar.Abs.TypeUnit -> prPrec i 1 (concatD [doc (showString "()")])
    Grammar.Abs.TypeExp type_ -> prPrec i 1 (concatD [doc (showString "!"), prt 1 type_])
    Grammar.Abs.TypeTens type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "*"), prt 0 type_2])
    Grammar.Abs.TypeFunc type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print Grammar.Abs.Angle where
  prt i = \case
    Grammar.Abs.AAngl d -> prPrec i 0 (concatD [prt 0 d])

instance Print Grammar.Abs.ControlState where
  prt i = \case
    Grammar.Abs.CStateZero -> prPrec i 0 (concatD [doc (showString "0")])
    Grammar.Abs.CStateOne -> prPrec i 0 (concatD [doc (showString "1")])
    Grammar.Abs.CStatePlus -> prPrec i 0 (concatD [doc (showString "+")])
    Grammar.Abs.CStateMinus -> prPrec i 0 (concatD [doc (showString "-")])
    Grammar.Abs.CStateIPlus -> prPrec i 0 (concatD [doc (showString "+i")])
    Grammar.Abs.CStateIMinus -> prPrec i 0 (concatD [doc (showString "-i")])

instance Print Grammar.Abs.Control where
  prt i = \case
    Grammar.Abs.CCtrl n controlstate -> prPrec i 0 (concatD [doc (showString "Ctrl"), prt 0 n, prt 0 controlstate])

instance Print [Grammar.Abs.Control] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Grammar.Abs.Gate where
  prt i = \case
    Grammar.Abs.GH controls -> prPrec i 0 (concatD [doc (showString "H"), prt 0 controls])
    Grammar.Abs.GX controls -> prPrec i 0 (concatD [doc (showString "X"), prt 0 controls])
    Grammar.Abs.GY controls -> prPrec i 0 (concatD [doc (showString "Y"), prt 0 controls])
    Grammar.Abs.GZ controls -> prPrec i 0 (concatD [doc (showString "Z"), prt 0 controls])
    Grammar.Abs.GI controls -> prPrec i 0 (concatD [doc (showString "I"), prt 0 controls])
    Grammar.Abs.GXRt n controls -> prPrec i 0 (concatD [doc (showString "RootX"), prt 0 n, prt 0 controls])
    Grammar.Abs.GXRtDag n controls -> prPrec i 0 (concatD [doc (showString "RootXDagger"), prt 0 n, prt 0 controls])
    Grammar.Abs.GYRt n controls -> prPrec i 0 (concatD [doc (showString "RootY"), prt 0 n, prt 0 controls])
    Grammar.Abs.GYRtDag n controls -> prPrec i 0 (concatD [doc (showString "RootYDagger"), prt 0 n, prt 0 controls])
    Grammar.Abs.GZRt n controls -> prPrec i 0 (concatD [doc (showString "RootZ"), prt 0 n, prt 0 controls])
    Grammar.Abs.GZRtDag n controls -> prPrec i 0 (concatD [doc (showString "RootZDagger"), prt 0 n, prt 0 controls])
    Grammar.Abs.GS controls -> prPrec i 0 (concatD [doc (showString "S"), prt 0 controls])
    Grammar.Abs.GSDag controls -> prPrec i 0 (concatD [doc (showString "SDagger"), prt 0 controls])
    Grammar.Abs.GT controls -> prPrec i 0 (concatD [doc (showString "T"), prt 0 controls])
    Grammar.Abs.GTDag controls -> prPrec i 0 (concatD [doc (showString "TDagger"), prt 0 controls])
    Grammar.Abs.GSqrtX controls -> prPrec i 0 (concatD [doc (showString "SqrtX"), prt 0 controls])
    Grammar.Abs.GSqrtXDag controls -> prPrec i 0 (concatD [doc (showString "SqrtXDagger"), prt 0 controls])
    Grammar.Abs.GSqrtY controls -> prPrec i 0 (concatD [doc (showString "SqrtY"), prt 0 controls])
    Grammar.Abs.GSqrtYDag controls -> prPrec i 0 (concatD [doc (showString "SqrtYDagger"), prt 0 controls])
    Grammar.Abs.GRxTheta angle controls -> prPrec i 0 (concatD [doc (showString "RxTheta"), prt 0 angle, prt 0 controls])
    Grammar.Abs.GRyTheta angle controls -> prPrec i 0 (concatD [doc (showString "RyTheta"), prt 0 angle, prt 0 controls])
    Grammar.Abs.GRzTheta angle controls -> prPrec i 0 (concatD [doc (showString "RzTheta"), prt 0 angle, prt 0 controls])
    Grammar.Abs.GU1 angle controls -> prPrec i 0 (concatD [doc (showString "U1"), prt 0 angle, prt 0 controls])
    Grammar.Abs.GU2 angle1 angle2 controls -> prPrec i 0 (concatD [doc (showString "U2"), prt 0 angle1, prt 0 angle2, prt 0 controls])
    Grammar.Abs.GU3 angle1 angle2 angle3 controls -> prPrec i 0 (concatD [doc (showString "U3"), prt 0 angle1, prt 0 angle2, prt 0 angle3, prt 0 controls])
    Grammar.Abs.GSwp controls -> prPrec i 0 (concatD [doc (showString "Swap"), prt 0 controls])
    Grammar.Abs.GSqrtSwp controls -> prPrec i 0 (concatD [doc (showString "SqrtSwap"), prt 0 controls])
    Grammar.Abs.GSqrtSwpDag controls -> prPrec i 0 (concatD [doc (showString "SqrtSwapDagger"), prt 0 controls])
    Grammar.Abs.GISwp controls -> prPrec i 0 (concatD [doc (showString "ISwap"), prt 0 controls])
    Grammar.Abs.GFSwp controls -> prPrec i 0 (concatD [doc (showString "FSwap"), prt 0 controls])
    Grammar.Abs.GSwpRt n controls -> prPrec i 0 (concatD [doc (showString "RootSwap"), prt 0 n, prt 0 controls])
    Grammar.Abs.GSwpRtDag controls -> prPrec i 0 (concatD [doc (showString "RootSwapDagger"), prt 0 controls])
    Grammar.Abs.GGeneric gategeneric -> prPrec i 0 (concatD [prt 0 gategeneric])

instance Print Grammar.Abs.LetVariable where
  prt i = \case
    Grammar.Abs.LVar var -> prPrec i 0 (concatD [prt 0 var])

instance Print [Grammar.Abs.LetVariable] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Grammar.Abs.Tuple where
  prt i = \case
    Grammar.Abs.Tup term terms -> prPrec i 0 (concatD [doc (showString "("), prt 0 term, doc (showString ","), prt 0 terms, doc (showString ")")])

instance Print [Grammar.Abs.Term] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Grammar.Abs.Bit where
  prt i = \case
    Grammar.Abs.BBit n -> prPrec i 0 (concatD [prt 0 n])

instance Print Grammar.Abs.Term where
  prt i = \case
    Grammar.Abs.TVar var -> prPrec i 3 (concatD [prt 0 var])
    Grammar.Abs.TBit bit -> prPrec i 3 (concatD [prt 0 bit])
    Grammar.Abs.TGate gate -> prPrec i 3 (concatD [prt 0 gate])
    Grammar.Abs.TTup tuple -> prPrec i 3 (concatD [prt 0 tuple])
    Grammar.Abs.TUnit -> prPrec i 3 (concatD [doc (showString "()")])
    Grammar.Abs.TApp term1 term2 -> prPrec i 2 (concatD [prt 2 term1, prt 3 term2])
    Grammar.Abs.TIfEl term1 term2 term3 -> prPrec i 1 (concatD [doc (showString "if"), prt 0 term1, doc (showString "then"), prt 0 term2, doc (showString "else"), prt 0 term3])
    Grammar.Abs.TLet letvariable letvariables term1 term2 -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "("), prt 0 letvariable, doc (showString ","), prt 0 letvariables, doc (showString ")"), doc (showString "="), prt 0 term1, doc (showString "in"), prt 0 term2])
    Grammar.Abs.TCase term1 term2 var1 term3 var2 -> prPrec i 1 (concatD [doc (showString "case"), prt 0 term1, doc (showString "of"), prt 0 term2, doc (showString "->"), prt 0 var1, prt 0 term3, doc (showString "->"), prt 0 var2])
    Grammar.Abs.TLmbd lambda funvariable type_ term -> prPrec i 1 (concatD [prt 0 lambda, prt 0 funvariable, prt 0 type_, doc (showString "."), prt 0 term])
    Grammar.Abs.TDollr term1 term2 -> prPrec i 1 (concatD [prt 2 term1, doc (showString "$"), prt 1 term2])

instance Print Grammar.Abs.Arg where
  prt i = \case
    Grammar.Abs.FunArg var -> prPrec i 0 (concatD [prt 0 var])

instance Print [Grammar.Abs.Arg] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Grammar.Abs.Function where
  prt i = \case
    Grammar.Abs.FunDef var args term -> prPrec i 0 (concatD [prt 0 var, prt 0 args, doc (showString "="), prt 0 term])

instance Print Grammar.Abs.FunDeclaration where
  prt i = \case
    Grammar.Abs.FunDecl funvariable type_ function -> prPrec i 0 (concatD [prt 0 funvariable, prt 0 type_, prt 0 function])

instance Print [Grammar.Abs.FunDeclaration] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Grammar.Abs.Program where
  prt i = \case
    Grammar.Abs.ProgDef fundeclarations -> prPrec i 0 (concatD [prt 0 fundeclarations])

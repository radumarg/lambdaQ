-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Lexer definition for use with Alex 3
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module Frontend.LambdaQ.Lex where

import Prelude

import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
}

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \+ | \- | \* | \/ | \( | \) | \& \& | \| \| | \= \= | \/ \= | \> | \> \= | \< | \< \= | \- \> | \* \* | \! | \( \) | \[ | \] | \@ "0" | \@ "1" | \@ \+ | \@ \- | \@ \+ "i" | \@ \- "i" | \, | \{ | \= | \} | \< \- | \; | \. | \$ | \[ \] | \: | \: \:

:-

-- Line comment "--"
"--" [.]* ;

-- Block comment "{-" "-}"
\{ \- [$u # \-]* \- ([$u # [\- \}]] [$u # \-]* \- | \-)* \} ;

-- Whitespace (skipped)
$white+ ;

-- Symbols
@rsyms
    { tok (eitherResIdent TV) }

-- token BitVariable
[0 1]
    { tok (eitherResIdent T_BitVariable) }

-- token Var
(\_ | $s)([\' \_]| ($d | $l)) *
    { tok (eitherResIdent T_Var) }

-- token Lambda
\\
    { tok (eitherResIdent T_Lambda) }

-- Keywords and Ident
$l $i*
    { tok (eitherResIdent TV) }

-- Integer
$d+
    { tok TI }

-- Double
$d+ \. $d+ (e (\-)? $d+)?
    { tok TD }

{
-- | Create a token with position.
tok :: (String -> Tok) -> (Posn -> String -> Token)
tok f p = PT p . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !String                    -- ^ String literal.
  | TI !String                    -- ^ Integer literal.
  | TV !String                    -- ^ Identifier.
  | TD !String                    -- ^ Float literal.
  | TC !String                    -- ^ Character literal.
  | T_BitVariable !String
  | T_Var !String
  | T_Lambda !String
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: String -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: String
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p)  = p

-- | Get line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t = (tokenLineCol t, tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_BitVariable s) -> s
  PT _ (T_Var s) -> s
  PT _ (T_Lambda s) -> s

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = tokenText t

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B String Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b "ROOT_X" 45
    (b "==" 23
       (b "->" 12
          (b ")" 6
             (b "&&" 3 (b "$" 2 (b "!" 1 N N) N) (b "()" 5 (b "(" 4 N N) N))
             (b "+" 9 (b "**" 8 (b "*" 7 N N) N) (b "-" 11 (b "," 10 N N) N)))
          (b ";" 18
             (b "/=" 15
                (b "/" 14 (b "." 13 N N) N) (b "::" 17 (b ":" 16 N N) N))
             (b "<=" 21 (b "<-" 20 (b "<" 19 N N) N) (b "=" 22 N N))))
       (b "FSWAP" 34
          (b "@-i" 29
             (b "@+" 26
                (b ">=" 25 (b ">" 24 N N) N) (b "@-" 28 (b "@+i" 27 N N) N))
             (b "Bit" 32 (b "@1" 31 (b "@0" 30 N N) N) (b "Bool" 33 N N)))
          (b "QFT" 40
             (b "ID" 37
                (b "H" 36 (b "False" 35 N N) N) (b "Int" 39 (b "ISWAP" 38 N N) N))
             (b "ROOT_SWAP" 43
                (b "Qbit" 42 (b "QFT_DAG" 41 N N) N) (b "ROOT_SWAP_DAG" 44 N N)))))
    (b "U2" 68
       (b "SQRT_X" 57
          (b "RX" 51
             (b "ROOT_Y_DAG" 48
                (b "ROOT_Y" 47 (b "ROOT_X_DAG" 46 N N) N)
                (b "ROOT_Z_DAG" 50 (b "ROOT_Z" 49 N N) N))
             (b "S" 54
                (b "RZ" 53 (b "RY" 52 N N) N)
                (b "SQRT_SWAP_DAG" 56 (b "SQRT_SWAP" 55 N N) N)))
          (b "S_DAG" 63
             (b "SQRT_Y_DAG" 60
                (b "SQRT_Y" 59 (b "SQRT_X_DAG" 58 N N) N)
                (b "SWAP_THETA" 62 (b "SWAP" 61 N N) N))
             (b "True" 66 (b "T_DAG" 65 (b "T" 64 N N) N) (b "U1" 67 N N))))
       (b "gate" 79
          (b "[]" 74
             (b "Y" 71 (b "X" 70 (b "U3" 69 N N) N) (b "[" 73 (b "Z" 72 N N) N))
             (b "ctrl" 77 (b "case" 76 (b "]" 75 N N) N) (b "else" 78 N N)))
          (b "then" 85
             (b "let" 82
                (b "in" 81 (b "if" 80 N N) N) (b "of" 84 (b "not" 83 N N) N))
             (b "||" 88 (b "{" 87 (b "with" 86 N N) N) (b "}" 89 N N)))))
  where
  b s n = B bs (TS bs n)
    where
    bs = s

-- | Unquote string literal.
unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}

-- File generated by the BNF Converter (bnfc 2.9.5).

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.LambdaQ.Layout where

import Prelude
import Data.Maybe ( fromMaybe, listToMaybe, mapMaybe )
import qualified Data.List as List

import Frontend.LambdaQ.Lex
  ( Posn(..), Tok(..), Token(..), TokSymbol(..)
  , prToken, tokenLineCol, tokenPos, tokenPosn
  )

-- local parameters

data LayoutDelimiters
  = LayoutDelimiters
    { delimSep   :: TokSymbol
    , delimOpen  :: Maybe TokSymbol  -- ^ Nothing for toplevel layout.
    , delimClose :: Maybe TokSymbol  -- ^ Nothing for toplevel layout.
    }

layoutWords :: [(TokSymbol, LayoutDelimiters)]
layoutWords = [( TokSymbol "let" 72
               , LayoutDelimiters (TokSymbol ";" 15) (Just (TokSymbol "{" 76)) (Just (TokSymbol "}" 77))
               )]

layoutStopWords :: [TokSymbol]
layoutStopWords = [TokSymbol "in" 71]

-- layout separators

layoutOpen, layoutClose, layoutSep :: [TokSymbol]
layoutOpen  = List.nub $ mapMaybe (delimOpen  . snd) layoutWords
layoutClose = List.nub $ mapMaybe (delimClose . snd) layoutWords
layoutSep   = List.nub $ map (delimSep . snd) layoutWords

parenOpen, parenClose :: [TokSymbol]
parenOpen  =
  [ TokSymbol "(" 3
  , TokSymbol "[" 64
  ]
parenClose =
  [ TokSymbol ")" 5
  , TokSymbol "]" 65
  ]

-- | Report an error during layout resolution.
layoutError
   :: [Token]  -- ^ Remaining tokens.
   -> String   -- ^ Error message.
   -> a
layoutError ts msg
  | null ts   = error $ concat [ "Layout error: ", msg, "." ]
  | otherwise = error $ unlines
      [ concat [ "Layout error at ", tokenPos ts, ": ", msg, "." ]
      , unwords $ concat
         [ [ "Remaining tokens:" ]
         , map prToken $ take 10 ts
         , [ "..." | not $ null $ drop 10 ts ]
         ]
      ]

-- | Replace layout syntax with explicit layout tokens.
resolveLayout
  :: Bool      -- ^ Whether to use top-level layout.
  -> [Token]   -- ^ Token stream before layout resolution.
  -> [Token]   -- ^ Token stream after layout resolution.
resolveLayout _topLayout = res Nothing [Explicit]
  where

  res :: Maybe Token -- ^ The previous token, if any.
      -> [Block]     -- ^ A stack of layout blocks.
      -> [Token] -> [Token]

  -- The stack should never be empty.
  res _ [] ts = layoutError ts "layout stack empty"

  -- Handling explicit blocks:
  res _ st (t0 : ts)
    -- We found an open brace in the input,
    -- put an explicit layout block on the stack.
    -- This is done even if there was no layout word,
    -- to keep opening and closing braces.
    | isLayoutOpen t0 || isParenOpen t0
      = t0 : res (Just t0) (Explicit : st) ts

    -- If we encounter a closing brace, exit the first explicit layout block.
    | isLayoutClose t0 || isParenClose t0
      , let (imps, rest) = span isImplicit st
      , let st' = drop 1 rest
      = if null st'
        then layoutError ts $ unwords
          [ "found", prToken t0, "at" , tokenPos [t0]
          , "without an explicit layout block"
          ]
        else map (closingToken ts (tokenPosn t0)) imps ++ t0 : res (Just t0) st' ts

  -- Ending or confirming implicit layout blocks:
  res pt (b@(Implicit delim status col) : bs) (t0 : ts)

      -- Do not end top-level layout block by layout stop word.
    | isStop t0,  col <= 1
      = t0 : res (Just t0) (b : bs) ts

      -- End of implicit block by a layout stop word.
    | isStop t0
           -- Exit the current block and all implicit blocks
           -- more indented than the current token.
      , let (ebs, st') = span ((column t0 <) . indentation) bs
           -- Insert block-closers after the previous token.
      = map (closingToken ts (afterPrev pt)) (b : ebs) ++ t0 : res (Just t0) st' ts

    -- End of an implicit layout block by dedentation.
    | newLine pt t0
      , column t0 < col
           -- Insert a block closer after the previous token.
           -- Repeat, with the current block removed from the stack.
      , let c = closingToken ts (afterPrev pt) b
      = c : res (Just c) bs (t0 : ts)

    -- If we are on a newline, confirm the last tentative blocks.
    | newLine pt t0, Tentative{} <- status
      = res pt (Implicit delim Definitive col : confirm col bs) (t0 : ts)

  -- Starting and processing implicit layout blocks:
  res pt st (t0 : ts)
    -- Start a new layout block if the first token is a layout word.
    | Just delim@(LayoutDelimiters _ mopen _) <- isLayout t0
      = maybeInsertSeparator pt t0 st $
        case ts of
          -- Explicit layout, just move on. The next step
          -- will push an explicit layout block.
          t1 : _ | isLayoutOpen t1 ->
            t0 : res (Just t0) st ts
          -- Otherwise, insert an open brace after the layout word
          _ ->
            t0 : b : res (Just b) (addImplicit delim (tokenPosn t0) pos st) ts
            where
            b   = sToken (nextPos t0) $ fromMaybe undefined mopen
            -- At the end of the file, the start column does not matter.
            -- So if there is no token t1 after t0, just use the position of t0.
            pos = tokenPosn $ fromMaybe t0 $ listToMaybe ts

    -- Insert separator if necessary.
    | otherwise
      = maybeInsertSeparator pt t0 st $
        t0 : res (Just t0) st ts

  -- At EOF: skip explicit blocks.
  res (Just _) [Explicit]      [] = []
  res (Just t) (Explicit : bs) [] = res (Just t) bs []

  -- If we are using top-level layout, insert a semicolon after
  -- the last token, if there isn't one already
  res (Just t) [Implicit (LayoutDelimiters sep _ _) _ _] []
    | isLayoutSep t = []
    | otherwise     = [sToken (nextPos t) sep]

  -- At EOF in an implicit, non-top-level block: close the block
  res (Just t) (Implicit (LayoutDelimiters _ _ (Just close)) _ _ : bs) []
      = b : res (Just b) bs []
        where b = sToken (nextPos t) close

  -- This should only happen if the input is empty.
  res Nothing _st []
      = []

  -- | Insert a 'layoutSep' if we are on a new line on the current
  --   implicit layout column.
  maybeInsertSeparator
    :: Maybe Token  -- ^ The previous token.
    -> Token        -- ^ The current token.
    -> [Block]      -- ^ The layout stack.
    -> [Token]      -- ^ The result token stream.
    -> [Token]      -- ^ Maybe prepended with a 'layoutSep'.
  maybeInsertSeparator pt t0 = \case
    Implicit (LayoutDelimiters sep _ _) _ n : _
      | newLine pt t0
      , column t0 == n
      , maybe False (not . isTokenIn (layoutSep ++ layoutOpen)) pt
       -- Insert a semicolon after the previous token
       -- unless we are the beginning of the file,
       -- or the previous token is a semicolon or open brace.
      -> (sToken (afterPrev pt) sep :)
    _ -> id

  closingToken :: [Token] -> Position -> Block -> Token
  closingToken ts pos = sToken pos . \case
    Implicit (LayoutDelimiters _ _ (Just sy)) _ _ -> sy
    _ -> layoutError ts "trying to close a top level block"

type Position = Posn
type Line     = Int
type Column   = Int

-- | Entry of the layout stack.
data Block
   = Implicit LayoutDelimiters Status Column
       -- ^ An implicit layout block with its start column.
   | Explicit

-- | Get current indentation.  0 if we are in an explicit block.
indentation :: Block -> Column
indentation = \case
  Implicit _ _ n -> n
  Explicit -> 0

-- | Check if s block is implicit.
isImplicit :: Block -> Bool
isImplicit = \case
  Implicit{} -> True
  Explicit{} -> False

data Status
  = Tentative   -- ^ A layout column that has not been confirmed by a line break
  | Definitive  -- ^ A layout column that has been confirmed by a line break.

-- | Add a new implicit layout block.
addImplicit
  :: LayoutDelimiters -- ^ Delimiters of the new block.
  -> Position         -- ^ Position of the layout keyword.
  -> Position         -- ^ Position of the token following the layout keword.
  -> [Block]
  -> [Block]
addImplicit delim (Pn _ l0 _) (Pn _ l1 c1) st
    -- Case: layout keyword was at the end of the line:
    -- New implicit block is definitive.
    | l1 > l0   = Implicit delim Definitive (col st') : st'
    -- Case: staying on the same line:
    -- New implicit block is tentative.
    | otherwise = Implicit delim Tentative (col st) : st
  where
  st' = confirm c1 st
  col bs = max c1 $ 1 + definiteIndentation bs
    -- The column of the next token determines the starting column
    -- of the implicit layout block.
    -- However, the next block needs to be strictly more indented
    -- than the previous block.

  -- | Get the current confirmed indentation level.
  definiteIndentation :: [Block] -> Int
  definiteIndentation bs =
    case dropWhile isTentative bs of
      Implicit _ Definitive n : _ -> n
      _ -> 0  -- 0 enables a first unindented block, see 194_layout/good05.in

  isTentative :: Block -> Bool
  isTentative = \case
    Implicit _ Tentative _ -> True
    _ -> False

-- | Confirm tentative blocks that are not more indented than @col@.
confirm :: Column -> [Block] -> [Block]
confirm c0 = loop
  where
  loop = \case
    Implicit delim Tentative c : bs
      | c <= c0 -> Implicit delim Definitive c : loop bs
    bs -> bs

-- | Get the position immediately to the right of the given token.
--   If no token is given, gets the first position in the file.
afterPrev :: Maybe Token -> Position
afterPrev = maybe (Pn 0 1 1) nextPos

-- | Get the position immediately to the right of the given token.
nextPos :: Token -> Position
nextPos t = Pn (g + s) l (c + s + 1)
  where
  Pn g l c = tokenPosn t
  s        = tokenLength t

-- | Get the number of characters in the token.
tokenLength :: Token -> Int
tokenLength = length . prToken

-- | Create a position symbol token.
sToken :: Position -> TokSymbol -> Token
sToken p t = PT p $ TK t

-- | Get the line number of a token.
line :: Token -> Line
line = fst . tokenLineCol

-- | Get the column number of a token.
column :: Token -> Column
column = snd . tokenLineCol

-- | Is the following token on a new line?
newLine :: Maybe Token -> Token -> Bool
newLine pt t0 = maybe True ((line t0 >) . line) pt

-- | Check if a word is a layout start token.
isLayout :: Token -> Maybe LayoutDelimiters
isLayout = \case
  PT _ (TK t) -> lookup t layoutWords
  _ -> Nothing

-- | Check if a token is one of the given symbols.
isTokenIn :: [TokSymbol] -> Token -> Bool
isTokenIn ts = \case
  PT _ (TK t) -> t `elem` ts
  _ -> False

-- | Check if a token is a layout stop token.
isStop :: Token -> Bool
isStop = isTokenIn layoutStopWords

-- | Check if a token is the layout open token.
isLayoutOpen :: Token -> Bool
isLayoutOpen = isTokenIn layoutOpen

-- | Check if a token is the layout separator token.
isLayoutSep :: Token -> Bool
isLayoutSep = isTokenIn layoutSep

-- | Check if a token is the layout close token.
isLayoutClose :: Token -> Bool
isLayoutClose = isTokenIn layoutClose

-- | Check if a token is an opening parenthesis.
isParenOpen :: Token -> Bool
isParenOpen = isTokenIn parenOpen

-- | Check if a token is a closing parenthesis.
isParenClose :: Token -> Bool
isParenClose = isTokenIn parenClose

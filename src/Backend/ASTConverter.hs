-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree 
--   *  introduce De Bruijn indices for bound variables

module Backend.ASTConverter where

import Backend.IAST ( Function( Func ), Program, mapFunction, reverseMapFunction )
import qualified Data.Map as Map
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Print ( printTree )
import qualified Frontend.LambdaQ.Abs as GenAbSyntax
import GHC.Err ( errorWithoutStackTrace )

parse :: String -> Program
parse str = case pProgram (myLexer str) of
   Left str -> errorWithoutStackTrace str
   Right p -> mapProgram p

mapProgram :: GenAbSyntax.Program -> Program
mapProgram (GenAbSyntax.ProgDef fs)  = map mapFunction fs

reverseMapProgram ::  Program -> GenAbSyntax.Program
reverseMapProgram = GenAbSyntax.ProgDef . map reverseMapFunction

parseAndPrintTreeFromString :: String -> String
parseAndPrintTreeFromString str = 
  case pProgram (myLexer str) of
    Left str -> errorWithoutStackTrace str
    Right prog -> printTree prog

parseAndPrintTreeFromFile :: FilePath -> IO String
parseAndPrintTreeFromFile path = parseAndPrintTreeFromString <$> readFile path


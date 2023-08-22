-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator.

module Backend.ASTConverter where

import Backend.IAST (Program, mapFunction, reverseMapFunction )
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Print ( printTree )
import qualified Frontend.LambdaQ.Abs as GenAbSyntax
import GHC.Err ( errorWithoutStackTrace )

parse :: String -> Program
parse str = case pProgram (myLexer str) of
   Left str -> errorWithoutStackTrace str
   Right p -> mapProgram p

mapProgram :: GenAbSyntax.Program -> Program
mapProgram (GenAbSyntax.ProgDef funs) = map mapFunction funs

reverseMapProgram :: Program -> GenAbSyntax.Program
reverseMapProgram = GenAbSyntax.ProgDef . map reverseMapFunction

parseAndPrintTreeFromString :: String -> String
parseAndPrintTreeFromString str = case pProgram (myLexer str) of
    Left str -> errorWithoutStackTrace str
    Right prog -> printTree prog

parseAndPrintTreeFromFile :: FilePath -> IO String
parseAndPrintTreeFromFile path = parseAndPrintTreeFromString <$> readFile path


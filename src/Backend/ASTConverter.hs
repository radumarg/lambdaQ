-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree 
--   *  introduce De Bruijn indices for bound variables

module Backend.ASTConverter where

import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Print ( printTree )


parse :: String -> Program
parse str = case pProgram (myLexer str) of
    Left str -> undefined
    Right str -> undefined

astToIast :: GeneratedAbstractSyntax.Program -> Program
astToIast = undefined

iastToAst ::  Program -> GeneratedAbstractSyntax.Program
iastToAst = undefined

parseAndPrintTreeFromString :: String -> String
parseAndPrintTreeFromString = printTree . iastToAst . parse

parseAndPrintTreeFromFile :: FilePath -> IO String
parseAndPrintTreeFromFile path = parseAndPrintTreeFromString <$> readFile path


module Backend.ASTConverter where

-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator.  

 -- TODO: 
 -- convert all functions into lambda abstractions
 -- convert BNFC generated AST terms into an intermediate abstract syntax tree 
 -- introduce De Bruijn indices for bound variables

import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Print ( printTree )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

data Type =
    TypeBit |
    TypeQbit |
    TypeUnit |
    TypeExp Type |
    TypeTensrs Type Integer |
    TypeTensr Type Type |
    TypeFunc Type Type
  deriving (Eq, Ord, Show, Read)

data Function = Func String Type Term
type Program = [Function]

translateType :: GeneratedAbstractSyntax.Type -> Type
translateType GeneratedAbstractSyntax.TypeBit   = TypeBit
translateType GeneratedAbstractSyntax.TypeQbit  = TypeQbit
translateType GeneratedAbstractSyntax.TypeUnit  = TypeUnit
-- todo: finish

reverseTranslateType :: Type -> GeneratedAbstractSyntax.Type
reverseTranslateType TypeBit  = GeneratedAbstractSyntax.TypeBit
reverseTranslateType TypeQbit = GeneratedAbstractSyntax.TypeQbit
reverseTranslateType TypeUnit = GeneratedAbstractSyntax.TypeUnit
-- todo: finish

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


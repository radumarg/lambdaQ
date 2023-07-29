-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the semantic analyser, type checker and the code generator:  
--   *  functions to be be converted to lambda abstractions
--   *  BNFC generated AST terms to be converted into an intermediate abstract syntax tree 
--   *  introduce De Bruijn indices for bound variables

module Backend.ASTConverter where

import Backend.IAST ( Function( Func ), Program, mapTerm, mapType )
import qualified Data.Map as Map
import Frontend.LambdaQ.Par ( myLexer, pProgram )
import Frontend.LambdaQ.Print ( printTree )
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import GHC.Err ( errorWithoutStackTrace )

parse :: String -> Program
parse str = case pProgram (myLexer str) of
    Left str -> errorWithoutStackTrace str
    Right p -> mapProgram p

mapFunction :: GeneratedAbstractSyntax.FunctionDeclaration -> Function
mapFunction (GeneratedAbstractSyntax.FunDecl funType funDef) = Func fname (mapType ftype) term
    where
        (GeneratedAbstractSyntax.FunType _ ftype) = funType
        (GeneratedAbstractSyntax.FunDef (GeneratedAbstractSyntax.Var fvar) fargs fbody) = funDef
        ((fline, fcol), fname) = fvar
        term = mapTerm Map.empty $ toLambda (debang ftype) fargs fbody

toLambda :: GeneratedAbstractSyntax.FunctionType -> [GeneratedAbstractSyntax.Var] ->  GeneratedAbstractSyntax.Term -> GeneratedAbstractSyntax.Term
toLambda = undefined

-- the linear type flag: '!' will be removed if present
debang :: GeneratedAbstractSyntax.Type -> GeneratedAbstractSyntax.Type
debang (GeneratedAbstractSyntax.TypeNonLin t) = debang t
debang t = t

--reverseMapFunction :: Function -> GeneratedAbstractSyntax.Function
--reverseMapFunction (FunDecl name typ term) = undefined

mapProgram :: GeneratedAbstractSyntax.Program -> Program
mapProgram (GeneratedAbstractSyntax.ProgDef fs)  = map mapFunction fs

--reverseMapProgram ::  Program -> GeneratedAbstractSyntax.Program
--reverseMapProgram = GeneratedAbstractSyntax.ProgDef . map reverseMapFunction

--parseAndPrintTreeFromString :: String -> String
--parseAndPrintTreeFromString = printTree . reverseMapProgram . parse

--parseAndPrintTreeFromFile :: FilePath -> IO String
--parseAndPrintTreeFromFile path = parseAndPrintTreeFromString <$> readFile path


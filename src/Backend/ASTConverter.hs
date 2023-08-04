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
import qualified Frontend.LambdaQ.Abs as GenAbSyntax
import GHC.Err ( errorWithoutStackTrace )

parse :: String -> Program
parse str = case pProgram (myLexer str) of
   Left str -> errorWithoutStackTrace str
   Right p -> mapProgram p

mapFunction :: GenAbSyntax.FunctionDeclaration -> Function
mapFunction (GenAbSyntax.FunDecl funType funDef) = Func fname (mapType ftype) term
   where
      (GenAbSyntax.FunType _ ftype) = funType
      (GenAbSyntax.FunDef (GenAbSyntax.Var fvar) fargs fbody) = funDef
      ((fline, fcol), fname) = fvar
      term = mapTerm Map.empty $ toLambda (trimExclMark ftype) fargs fbody

-- convert function to a lambda abstraction 
toLambda :: GenAbSyntax.Type -> [GenAbSyntax.Arg] ->  GenAbSyntax.Term -> GenAbSyntax.Term
toLambda ftype [] fbody = fbody
toLambda (GenAbSyntax.TypeFunc ltype rtype) (GenAbSyntax.FunArg (GenAbSyntax.Var var) : vars ) body = 
   GenAbSyntax.TLambda (GenAbSyntax.Lambda "\\") (GenAbSyntax.FunType (GenAbSyntax.Var var) ltype) (toLambda rtype vars body)
toLambda (GenAbSyntax.TypeNonLin (GenAbSyntax.TypeFunc ltype rtype)) (GenAbSyntax.FunArg (GenAbSyntax.Var var) : vars ) body =
   GenAbSyntax.TLambda (GenAbSyntax.Lambda "\\") (GenAbSyntax.FunType (GenAbSyntax.Var var) ltype) (toLambda rtype vars body)

-- the outer non-linear type flag(s) '!' will be removed if present
trimExclMark :: GenAbSyntax.Type -> GenAbSyntax.Type
trimExclMark (GenAbSyntax.TypeNonLin t) = trimExclMark t
trimExclMark t = t

mapProgram :: GenAbSyntax.Program -> Program
mapProgram (GenAbSyntax.ProgDef fs)  = map mapFunction fs

parseAndPrintTreeFromString :: String -> String
parseAndPrintTreeFromString str = 
  case pProgram (myLexer str) of
    Left str -> errorWithoutStackTrace str
    Right prog -> printTree prog

parseAndPrintTreeFromFile :: FilePath -> IO String
parseAndPrintTreeFromFile path = parseAndPrintTreeFromString <$> readFile path


module Backend.ASTConverter where

-- A representation of a lambdaQ program in the shape of an abstract syntax tree
-- will be generated here. Subsequently the abstract syntax tree is converted
-- to an intermediate abstract syntax tree with a simpler syntax to make it easier
-- to process by the type checker and the code generator.  

 -- TODO: 
 -- convert all functions into lambda abstractions
 -- convert BNFC generated AST terms into an intermediate abstract syntax tree 
 -- introduce De Bruijn indices for bound variables

import Frontend.LambdaQ.Par ( myLexer, pProgram )

parseCode str = case pProgram (myLexer str) of
    Left str -> -- todo
    Right str -> -- todo




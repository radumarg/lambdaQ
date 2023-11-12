{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import Backend.IAST (Program, reverseMapFunction, mapProgram)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast)
import Frontend.LambdaQ.Print ( printTree )

import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runIASTConversion :: FilePath -> Exec Program
runIASTConversion filePath = readTheFile filePath
                            >>= parseProgram
                            >>= semanticAnalysis
                            >>= convertAstToIast

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runIASTConversion filePath) >>= \case
          Left error -> putStrLn $ "Error: " ++ show error ++ "!"
          Right program -> putStrLn $ printTree (map reverseMapFunction program)
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck, generateCode)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax
import Backend.IAST (Program)

runCodeGenerator :: FilePath -> Exec String
runCodeGenerator filePath = readTheFile filePath
                          >>= parseProgram
                          >>= semanticAnalysis
                          >>= convertAstToIast
                          >>= typeCheck
                          >>= generateCode

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runCodeGenerator filePath) >>= \case
          Left error -> putStrLn $ "Error: " ++ show error ++ "!"
          Right code -> putStrLn code
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
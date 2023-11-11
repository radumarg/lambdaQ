{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import Backend.IAST (Program)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runTypeChecker :: FilePath -> Exec Program
runTypeChecker filePath = readTheFile filePath
                          >>= parseProgram
                          >>= semanticAnalysis
                          >>= convertAstToIast
                          >>= typeCheck

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runTypeChecker filePath) >>= \case
          Left error -> putStrLn $ "Error: " ++ show error ++ "!"
          Right _ -> putStrLn "All good."
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
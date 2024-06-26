{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import CompilationEngine (Exec, readFileContents, parseProgram, semanticAnalysis, convertAstToIast, typeCheck, generateCode)

runCodeGeneration :: FilePath -> Exec String
runCodeGeneration filePath = readFileContents filePath
                          >>= parseProgram
                          >>= semanticAnalysis
                          >>= convertAstToIast
                          >>= typeCheck
                          >>= generateCode

lastNElements :: Int -> [a] -> [a]
lastNElements n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runCodeGeneration filePath) >>= \case
          Left err -> putStrLn $ "Error: " ++ show err ++ "!"
          Right code -> putStrLn code
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastNElements 3
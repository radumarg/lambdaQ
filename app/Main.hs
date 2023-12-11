{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck, generateCode)
import System.IO

compile :: FilePath -> Exec String
compile filePath = readTheFile filePath
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
        runExceptT (compile filePath) >>= \case
          Left err -> putStrLn $ "Error: " ++ show err ++ "!"
          Right code -> 
            do
              handle <- openFile outputFile ReadMode
              putStr code
              hClose handle
            where
              outputFile = init (init filePath) ++ ".qasm"
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
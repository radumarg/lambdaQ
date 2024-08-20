{-# LANGUAGE LambdaCase #-}

import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import Frontend.ASTtoIASTConverter (Program)
import CompilationEngine (Exec, readFileContents, parseProgram, semanticAnalysis, convertAstToIast)

runIASTConversion :: FilePath -> Exec Program
runIASTConversion filePath = readFileContents filePath
                            >>= parseProgram
                            >>= semanticAnalysis
                            >>= convertAstToIast

lastNElements :: Int -> [a] -> [a]
lastNElements n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runIASTConversion filePath) >>= \case
          Left err -> putStrLn $ "Error: " ++ show err ++ "!"
          Right program -> mapM_ (\f -> print f >> putStrLn "") program
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastNElements 3
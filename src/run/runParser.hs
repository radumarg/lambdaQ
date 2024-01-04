import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import CompilationEngine (Exec, readTheFile, parseProgram)
import Frontend.LambdaQ.Print ( printTree )

import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runParser :: FilePath -> Exec GeneratedAbstractSyntax.Program
runParser filePath = readTheFile filePath
                    >>= parseProgram

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && last3 (head args) == ".lq"
    then
      do
        let filePath = head args
        runExceptT (runParser filePath) >>= \case
          Left err -> putStrLn $ "Error: " ++ show err ++ "!"
          Right program -> putStrLn $ printTree program
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
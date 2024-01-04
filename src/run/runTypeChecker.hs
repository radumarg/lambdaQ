import System.Environment ( getArgs )  
import Control.Monad.Except ( runExceptT)
import Backend.ASTtoIASTConverter (Program)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck)

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
          Left err -> putStrLn $ "Error: " ++ show err ++ "!"
          Right _ -> putStrLn "All good."
      else
        putStrLn "Please supply one argument: a path to a lambdaQ program file!"
  where
    last3 = lastN 3
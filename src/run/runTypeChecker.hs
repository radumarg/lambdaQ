import Backend.IAST (Program)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runTypeChecker :: FilePath -> Exec Program
runTypeChecker filePath = readTheFile filePath
                          >>= parseProgram
                          >>= semanticAnalysis
                          >>= convertAstToIast
                          >>= typeCheck

main :: IO ()
main = undefined
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast, typeCheck, generateCode)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runCodeGenerator :: FilePath -> Exec String
runCodeGenerator filePath = readTheFile filePath
                          >>= parseProgram
                          >>= semanticAnalysis
                          >>= convertAstToIast
                          >>= typeCheck
                          >>= generateCode

main :: IO ()
main = undefined
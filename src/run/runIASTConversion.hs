import Backend.IAST (Program)
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runIASTConversion :: FilePath -> Exec Program
runIASTConversion filePath = readTheFile filePath
                            >>= parseProgram
                            >>= semanticAnalysis
                            >>= convertAstToIast

main :: IO ()
main = undefined

import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runParser :: FilePath -> Exec GeneratedAbstractSyntax.Program
runParser filePath = readTheFile filePath
                    >>= parseProgram

main :: IO ()
main = undefined
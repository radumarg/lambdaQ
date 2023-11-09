
import CompilationEngine (Exec, readTheFile, parseProgram, semanticAnalysis, convertAstToIast)
import qualified Frontend.LambdaQ.Abs as GeneratedAbstractSyntax

runSemanticAnalysis :: FilePath -> Exec GeneratedAbstractSyntax.Program
runSemanticAnalysis filePath = readTheFile filePath
                              >>= parseProgram
                              >>= semanticAnalysis


main :: IO ()
main = undefined
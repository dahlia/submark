import Language.Haskell.HLint
import System.Exit

arguments :: [String]
arguments = ["app", "src", "test"]

main :: IO ()
main = do
    hlints <- hlint arguments
    case hlints of
        [] -> exitSuccess
        _ -> exitFailure

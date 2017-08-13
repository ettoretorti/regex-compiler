import Regex
import Parser (regex)
import Conversion (regexToDfa)
import DFACompile (compileToC)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Environment (getArgs, getProgName)
import Text.ParserCombinators.Parsec (parse, eof)
import qualified Data.Set as Set

-- just in case it takes more than 1 normalization
regexNormFixpoint rgx = it (normalize rgx) rgx
    where it cur prev
              | cur == prev = cur
              | otherwise = it (normalize cur) cur

restrictedAlphabet rgx = ri Set.empty rgx
    where ri set Null = set
          ri set Empty = set
          ri set (Term x) = set `Set.union` Set.singleton x
          ri set (Rep x) = ri set x
          ri set (Comp x) = ri set x
          ri set (Seq x y) = ri set x `Set.union` ri set y
          ri set (Alt x y) = ri set x `Set.union` ri set y
          ri set (Inter x y) = ri set x `Set.union` ri set y

parseRegex str = case parse (Parser.regex <* eof) "" str of
                   (Left err)  -> Left (show err)
                   (Right rgx) -> Right rgx

compileRegex fName parsed = do rgx <- parsed
                               let rgx' = regexNormFixpoint $ rmap fromEnum rgx
                               let dfa  = regexToDfa rgx' $ Set.toList $ restrictedAlphabet rgx'
                               compileToC dfa fName

printCompiled rgx name = do let res = compileRegex name (parseRegex rgx)
                            case res of
                              (Left err) -> hPutStrLn stderr $ "Error: " ++ err
                              (Right cmp) -> putStr cmp

insufficientArgs = do pname <- getProgName
                      hPutStrLn stderr $ "Error: " ++ pname ++ " expects args in the form: rgx [name]"

main = do args <- getArgs
          case args of
            [rgx]      -> printCompiled rgx "matches"
            [rgx,name] -> printCompiled rgx name
            _          -> insufficientArgs

module DFACompile (
    compileToC
) where
import DFA
import Data.List (intercalate)

type CompileError = String

cFuncStart name = "int " ++ name ++ "(const unsigned char *str, const unsigned len) {\n\
                  \    unsigned idx = 0;\n\
                  \    unsigned char cur = 0;\n\n"
cFuncEnd = "}\n"

cGetNext = "    cur = str[idx++];\n"

cEndSuccess = "    if(idx == len) return 1;\n"
cEndFail    = "    if(idx == len) return 0;\n"

cLabel n = "l" ++ show n ++ ":;\n"
cGoto c n = "    if(cur == " ++ show c ++ ") goto l" ++ show n ++ ";\n"
cCatchAll = "    return 0;\n"

compileToC :: DFA Int -> String -> Either CompileError String
compileToC (DFA size alpha trans accept) funcName
    | not $ null [x | x <- alpha, x < 0, x > 255] = Left "Alphabet is not in unsigned char range"
    | otherwise = Right $ cFuncStart funcName ++
                          intercalate "\n" [genCode x | x <- [0..(size-1)]] ++
                          cFuncEnd
        where genCode s = cLabel s ++ 
                          (if accept s then cEndSuccess else cEndFail) ++
                          cGetNext ++
                          concat [cGoto c (trans s c) | c <- alpha] ++ 
                          cCatchAll

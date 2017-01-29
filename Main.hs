import Safe
import Data.List
import Data.List.Split
import Text.Regex.Posix --only used for one thing so not as bad as it looks
import System.Environment

selfCloseTags :: [String]
selfCloseTags = ["<br", "<hr", "<col", "<area", "<base", "<embed", "<link", "<input", "<img", "<meta", "<param", "<source", "<track", "<!"]

nTabs :: Int -> String
nTabs n = take n $ repeat '\t'

--strips any previous whitespace
unindent :: String -> String
unindent line = do
  let index = elemIndex '<' line
  case index of
       Just n -> drop n line
       Nothing -> ""

removeMatches :: String -> [[String]] -> String
removeMatches line [] = line
removeMatches line matches = do
  let submatches = head matches
  if submatches == [] then removeMatches line $ tail matches
    else do
    let nline = intercalate "" $ splitOn (head submatches) line
    if submatches == [] then
      removeMatches nline $ tail matches
      else removeMatches nline ([(tail submatches)] ++ (tail matches))

regMatchLine :: String -> String -> [String]
regMatchLine r line = getAllTextMatches $ line =~ r :: [String]

removeSelfClose :: String -> String
removeSelfClose line = do
  let regstrs = map (\x -> x ++ "([^>]*)>") selfCloseTags
  let matches = map (\r -> regMatchLine r line) regstrs
  removeMatches line matches
  
getIndentLevel :: String -> Int
getIndentLevel line = do
  let nline = removeSelfClose line
  let starts = (length (splitOn "<" nline)) - 1
  let ends = (length (splitOn "</" nline)) - 1
  (starts - (2*ends))

handleOpens :: [String] -> Int -> String -> String
handleOpens [] lvl res = res
handleOpens opens lvl res = do
  let tok = head opens
  let pnres = res ++ "\n" ++ (nTabs (lvl-1)) ++ "<" ++ tok
  if (removeSelfClose tok) /= tok then handleOpens (tail opens) (lvl-1) pnres
    else do
    if tok /= "" then do
      let endflag = maybeNumToNum $ elemIndex '/' tok
      if (endflag == 0) then do
        let nres = res ++ "\n" ++ (nTabs (lvl-1)) ++ "<" ++ tok
        handleOpens (tail opens) (lvl-1) nres
        else do
             let nres = res ++ "\n" ++ (nTabs lvl) ++ "<" ++ tok
             handleOpens (tail opens) (lvl+1) nres
      else handleOpens (tail opens) lvl res
    
indentLine :: String -> Int -> String
indentLine line initialIndent = handleOpens (splitOn "<" line) initialIndent ""

indent :: [String] -> String -> Int -> String
indent [] ndat lvl = ndat
indent dat ndat lvl = do
  let line = head dat
  let nlvl = lvl + (getIndentLevel line)
  let nline = indentLine line lvl
  indent (tail dat) (ndat ++ nline) nlvl
  
mkPretty :: [String] -> String
mkPretty lines = do
  let nlines = map unindent lines
  indent nlines "" 0
  
main :: IO ()
main = do
  args <- getArgs
  if (length args) > 1 then do
    let path = maybeStrToStr $ safeNth args 0
    let outpath = maybeStrToStr $ safeNth args 1
    fileData <- readFile path
    let codeLines = lines fileData

    writeFile outpath $ mkPretty codeLines
    else putStrLn "./Main [infile.html] [outfile.html]"

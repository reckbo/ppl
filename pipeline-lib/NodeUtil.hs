module NodeUtil
  (getPath
  ,rplc
  ,showKey
  ,outdir
  ) where

import           Data.List       (intercalate, isInfixOf)
import           Data.List.Split (splitOn)
import           Paths           (given, outdir)
import           Text.Regex      (mkRegex, subRegex)

getPath key caseid = case lookup key given of
  Nothing -> error $ "Set '" ++ key ++ "' in Paths.hs"
  Just path -> rplc "{case}" caseid path

rplc :: String -> String -> String -> String
rplc param val s = if not (isInfixOf param s)
                          then error $ param ++ " must be in path " ++ s
                          else intercalate val . splitOn param $ s

showKey :: Show a => a -> String
showKey =  filter (/='"') . clean . rmCommaSpaces . rmFieldNames . show
  where
    rmFieldNames s = subRegex (mkRegex "[a-zA-Z12]+ = ") s ""
    rmCommaSpaces s = subRegex (mkRegex ", ") s ","
    clean = map rplc
    rplc ' ' = '-'
    rplc '[' = '('
    rplc ']' = ')'
    rplc '{' = '('
    rplc '}' = ')'
    rplc c = c

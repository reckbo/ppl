module Node.Util where

import           Data.List       (intercalate, isInfixOf)
import           Data.List.Split (splitOn)
import           Paths           (given)

getPath key caseid = case lookup key given of
  Nothing -> error "Set '" ++ key ++ "' in Paths.hs"
  Just path -> rplc "{case}" caseid path

rplc :: String -> String -> String -> String
rplc param val s = if not (isInfixOf param s)
                          then error $ param ++ " must be in path " ++ s
                          else intercalate val . splitOn param $ s

showKey :: Show a => a -> String
showKey = filter (/='"') . clean . show
  where
    clean = map rplc
    rplc ' ' = '_'
    rplc c = c

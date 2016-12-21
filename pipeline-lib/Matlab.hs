module Matlab
  (run)
where

import Shake.BuildNode

run :: [FilePath] -> String -> Action ()
run addpaths mcmd = command_ [] "matlab" ["-nodesktop", "-nosplash"
                                         , "-r", mcmd']
  where mcmd' = (concatMap (\p -> "addpath " ++ p ++ ";" ) addpaths)
                ++ mcmd
                ++ ";quit"

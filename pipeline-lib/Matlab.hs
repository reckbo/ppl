module Matlab
  (cmdarr)
where

import Shake.BuildNode

cmdarr :: [FilePath] -> String -> [String]
cmdarr addpaths mcmd = ["matlab","-nodesktop", "-nosplash"
                                         , "-r", mcmd']
  where mcmd' = (concatMap (\p -> "addpath " ++ p ++ ";" ) addpaths)
                ++ catchException(mcmd)
                ++ ";quit"
        catchException s = "try, " ++ s ++ ", catch ME, getReport(ME), end"

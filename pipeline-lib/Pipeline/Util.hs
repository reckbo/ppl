module Pipeline.Util
  (
    showKey
  ) where

showKey :: Show a => a -> String
showKey = filter (/='"') . filter (/='(') . filter (/=')') . clean . show
  where
    clean = map rplc
    rplc ' ' = '-'
    rplc ',' = '-'
    rplc c = c


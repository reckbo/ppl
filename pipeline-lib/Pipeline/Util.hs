module Pipeline.Util
  (
    showKey
  ) where

showKey :: Show a => a -> String
showKey = filter (/='"') . clean . show
  where
    clean = map rplc
    rplc ' ' = '_'
    rplc ',' = '-'
    rplc c = c


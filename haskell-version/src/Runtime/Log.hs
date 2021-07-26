module Runtime.Log where

class Monad m => Log m where
  log :: String -> m ()

instance Log IO where
  log _ = pure ()

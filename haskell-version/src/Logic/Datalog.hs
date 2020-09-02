{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Datalog where

data Table a = Table [a]

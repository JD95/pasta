{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Env where

import           Polysemy

data SymLookup key val m a where
  SymLookup :: key -> SymLookup key val m (Maybe val)

makeSem ''SymLookup

data SymAlter key val m a where
  SymAlter  :: key -> (Maybe val -> Maybe val) -> SymAlter key val m ()

makeSem ''SymAlter

data NameGen m a where
  NewName :: NameGen m String

makeSem ''NameGen

data Logging m a where
  Log :: String -> Logging m ()

makeSem ''Logging

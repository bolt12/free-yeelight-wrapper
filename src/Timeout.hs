{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Timeout where

import Control.Monad
import Data.Functor
import Polysemy
import Polysemy.Final
import qualified System.Timeout as T

data Timeout m a where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

makeSem ''Timeout

runTimeoutToIO :: Member (Final IO) r => Sem (Timeout ': r) a -> Sem r a
runTimeoutToIO =
  interpretFinal
    ( \case
        Timeout i ma -> do
          s <- getInitialStateS
          ma' <- T.timeout i <$> runS ma
          pure $ maybe (s $> Nothing) (Just <$>) <$> ma'
    )

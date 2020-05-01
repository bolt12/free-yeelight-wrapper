{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Config
import Control.Exception
import Data.Maybe
import Multicast
import qualified Network.Socket as NS
import Polysemy
import Polysemy.Final
import Polysemy.Resource
import Polysemy.Trace
import Socket
import Timeout
import Types

discover :: Members '[Socket, Multicast, Trace, Timeout] r => Sem r (Maybe NS.Socket)
discover = timeout (5 * 1000000) $ do
  ip <- multicast "239.255.255.250" 1982
  connect ip

program :: Members '[Config, Socket, Multicast, Trace, Timeout] r => Sem r ()
program = do
  commands <- listToMaybe <$> getArguments
  case commands of
    Nothing -> trace "Please provide a command"
    Just cmd -> do
      (host, port) <- getAddress
      ip <- resolve host port
      case ip of
        Nothing -> do
          trace $ "Couldn't resolve " ++ show ip
          trace "Discovering..."
          sock <- discover
          maybe
            (trace "Discovery timeout!")
            (unicast cmd)
            sock
        Just ip -> do
          trace "Resolved!"
          trace "Connecting..."
          sock <- connect ip
          unicast cmd sock

main :: IO ()
main =
  let r =
        runFinal
          . runTimeoutToIO
          . embedToFinal
          . runResource
          . traceToIO
          . interpretSocket
          . interpretMulticast
          . interpretConfig
          $ program
   in do
        r
          `catch` ( \(e :: SomeException) -> do
                      print e
                      updateAddress "Null"
                  )

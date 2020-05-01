{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Control.Exception
import qualified Network.Socket as NS
import Polysemy
import Polysemy.Trace
import Polysemy.Resource
import Polysemy.Final
import Data.Maybe
import Config
import Multicast
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
        Nothing  -> trace "Please provide a command"
        Just cmd -> do
            (host, port) <- getAddress
            ip           <- resolve host port
            case ip of
                Nothing -> do
                    trace $ "Couldn't resolve " ++ show ip
                    trace "Discovering..."
                    sock <- discover
                    maybe (trace "Discovery timeout!")
                          (unicast cmd)
                          sock
                Just ip -> do
                    trace "Resolved!"
                    trace "Connecting..."
                    sock <- connect ip
                    unicast cmd sock

main :: IO ()
main =
    let r = runFinal
              . runTimeoutToIO
              . embedToFinal
              . interpretConfig
              . runResource
              . interpretMulticast
              . interpretSocket
              . traceToIO
              $ program
     in do
       r `catch` (\(e :: SomeException) -> do
         print e
         updateAddress "Null"
                 )

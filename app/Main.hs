{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import qualified Network.Socket as NS
import Polysemy
import Polysemy.Trace
import Data.Maybe
import Config
import Multicast
import Socket
import Types

discover :: Members '[Socket, Multicast] r => Sem r (Maybe NS.Socket)
discover = do
    ip <- multicast "239.255.255.250" 1982
    connect ip

program :: Members '[Config, Socket, Multicast, Trace] r => Sem r ()
program = do
    commands <- listToMaybe <$> getArguments
    case commands of
        Nothing -> trace "Please provide a command"
        Just cmd -> do
            (host, port) <- getAddress
            ip <- resolve host port
            case ip of
                Nothing -> do
                    trace "Discovering..."
                    sock <- discover
                    maybe (trace "Discovery timeout!") (unicast cmd) sock
                _ -> do
                    trace "Connecting..."
                    sock <- connect ip
                    maybe (return ()) (unicast cmd) sock

main :: IO ()
main = runM
    . interpretConfig
    . interpretMulticast
    . interpretSocket
    . traceToIO
    $ program

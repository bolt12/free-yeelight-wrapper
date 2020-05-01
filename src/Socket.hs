{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Socket where

import Polysemy
import Polysemy.Resource
import Data.ByteString.Char8 hiding (head , putStrLn)
import qualified Network.Socket as NS
import Network.Socket.ByteString (sendAll)
import Network.Multicast
import Control.Exception hiding (bracketOnError, finally)
import Types

data Socket m a where
    Connect :: Ip -> Socket m NS.Socket
    Unicast :: Command -> NS.Socket -> Socket m ()
    Resolve :: NS.HostName -> NS.ServiceName -> Socket m (Maybe NS.AddrInfo)

makeSem ''Socket

interpretSocket :: Members '[Embed IO, Resource] r => Sem (Socket ': r) a -> Sem r a
interpretSocket = interpret
    (\case
        Connect ip ->
          bracketOnError (embed $ NS.socket (NS.addrFamily ip) (NS.addrSocketType ip) (NS.addrProtocol ip))
                  (embed . NS.close)
                  (embed . \sock -> do
                      NS.connect sock $ NS.addrAddress ip
                      return sock
                  )
        Unicast cmd sock ->
          finally (embed $ do
                    putStrLn $ "Sending command " ++ cmd
                    case cmd of
                        "off" ->
                            sendAll sock
                                . pack
                                $ "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"off\"]}\r\n"
                        "on" ->
                            sendAll sock
                                . pack
                                $ "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"on\"]}\r\n"
                        _ ->
                            sendAll sock
                                . pack
                                $ "{\"id\": 1, \"method\":\"toggle\", \"params\":[]}\r\n"
                  )
                  (embed $ NS.close sock)
        Resolve host port -> embed $ do
            r <- try $ do
                let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
                head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
            case r of
                Left (_ :: SomeException) -> return Nothing
                Right addr -> return (Just addr)
    )

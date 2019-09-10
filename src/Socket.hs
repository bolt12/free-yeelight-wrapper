{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Socket where

import Polysemy
import Polysemy.Trace
import Data.ByteString.Char8 hiding (head, putStrLn)
import qualified Network.Socket as NS
import Network.Socket.ByteString (sendAll)
import Network.Multicast
import Control.Exception
import Types

data Socket m a where
    Connect :: Maybe Ip -> Socket m (Maybe NS.Socket)
    Unicast :: Command -> NS.Socket -> Socket m ()
    Resolve :: NS.HostName -> NS.ServiceName -> Socket m (Maybe NS.AddrInfo)

makeSem ''Socket

interpretSocket :: Member (Embed IO) r => Sem (Socket ': r) a -> Sem r a
interpretSocket = interpret (\case
    Connect Nothing -> return Nothing
    Connect (Just ip) -> embed $ do
        r <- try $ do
            sock <- NS.socket (NS.addrFamily ip) (NS.addrSocketType ip) (NS.addrProtocol ip)
            NS.connect sock $ NS.addrAddress ip
            return sock
        case r of
          Left (_ :: SomeException) -> do
              putStrLn "Couldn't connect!"
              return Nothing
          Right sock -> return $ Just sock
    Unicast cmd sock -> embed $ do
        case cmd of
          "off" -> sendAll sock . pack $ "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"off\"]}\r\n"
          "on"  -> sendAll sock . pack $ "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"on\"]}\r\n"
          _       -> sendAll sock . pack $ "{\"id\": 1, \"method\":\"toggle\", \"params\":[]}\r\n"
        NS.close sock
    Resolve host port -> embed $ do
        r <- try $ do
            let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
            head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
        case r of
          Left (_ :: SomeException) -> do
              putStrLn "Could not resolve default hostname!"
              return Nothing
          Right addr -> do
              putStrLn "Resolved!"
              return $ Just addr
                            )

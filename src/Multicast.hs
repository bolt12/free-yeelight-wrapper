{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Multicast
  ( module Multicast,
  )
where

import Control.Exception hiding (bracket)
import Control.Monad
import Data.ByteString.Char8 hiding (head, putStrLn, writeFile)
import Network.Multicast
import Network.Socket hiding (recv, recvFrom, sendAll, sendTo)
import Network.Socket.ByteString (recv, recvFrom, sendAll, sendTo)
import Polysemy
import Polysemy.Resource
import System.Directory
import System.Timeout
import Types

data Multicast m a where
  Multicast :: HostName -> PortNumber -> Multicast m Ip

makeSem ''Multicast

discoveryProtocol :: ByteString
discoveryProtocol =
  pack
    ( "M-SEARCH * HTTP/1.1\r\n"
        ++ "HOST: 239.255.255.250:1982\r\n"
        ++ "MAN: \"ssdp:discover\"\r\n"
        ++ "ST: wifi_bulb"
    )

interpretMulticast :: Members '[Embed IO, Resource] r => Sem (Multicast ': r) a -> Sem r a
interpretMulticast =
  interpret
    ( \case
        Multicast hn pn ->
          bracket
            (embed $ multicastSender hn pn)
            (embed . close . fst)
            ( embed . \(sSock, sAddr) -> do
                sendTo sSock discoveryProtocol sAddr
                (msg, rAddr) <- recvFrom sSock 1024
                let (SockAddrInet _ host) = rAddr
                    (w1, w2, w3, w4) = hostAddressToTuple host
                    (i1, i2, i3, i4) = (fromIntegral w1, fromIntegral w2, fromIntegral w3, fromIntegral w4)
                    ip = show i1 ++ "." ++ show i2 ++ "." ++ show i3 ++ "." ++ show i4
                 in do
                      let hints = defaultHints {addrSocketType = Stream}
                      addr <- head <$> getAddrInfo (Just hints) (Just ip) (Just "55443")
                      updateAddress ip
                      return addr
            )
    )

updateAddress :: String -> IO ()
updateAddress ip = do
  homeDir <- getHomeDirectory
  let fileP = homeDir ++ "/.yeelight_wrapper.config"
  writeFile fileP $ ip ++ "\n55443"

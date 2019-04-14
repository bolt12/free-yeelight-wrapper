{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll)
import Network.Multicast
import System.Directory
import System.Environment

-- (1) Auxiliary functions -----------

getAddress :: IO (HostName, ServiceName)
getAddress = do
        homeDir <- getHomeDirectory
        let fileP = homeDir ++ "/.yeelight_wrapper.config"
        r <- doesFileExist fileP
        if r then do
             [host, port] <- lines <$> readFile fileP
             return (host, port)
             else do
             writeFile fileP "127.0.0.1\n55443"
             return ("127.0.0.1", "55443")

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr

open :: AddrInfo -> IO Socket
open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

talk :: Socket -> C.ByteString -> IO ()
talk sock cmd = do
        sendAll sock cmd
        msg <- recv sock 1024
        putStr "Received: "
        C.putStrLn msg

discoveryProtocol :: String
discoveryProtocol = "M-SEARCH * HTTP/1.1\r\n" ++
    "HOST: 239.255.255.250:1982\r\n" ++
    "MAN: \"ssdp:discover\"\r\n" ++
    "ST: wifi_bulb"

{-
discover :: IO HostName
discover = withSocketsDo $ do
        addr <- resolve "239.255.255.250" 1982
        connect sock $ "239.255.255.250"
    where
        multicastSocket = socket AF_INET Datagram 1982
        -}

-- (2) Main program -----------

main :: IO ()
main = withSocketsDo $ do
    cmd <- getArgs
    (host, port) <- getAddress
    addr <- resolve host port
    sock <- open addr 
    case cmd of
        ["off"] -> talk sock "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"off\"]}\r\n"
        ["on"]  -> talk sock "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"on\"]}\r\n"
        _       -> talk sock "{\"id\": 1, \"method\":\"toggle\", \"params\":[]}\r\n"
    close sock

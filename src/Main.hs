{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Control.Monad
import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString (recv, sendAll, sendTo, recvFrom)
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

updateAddress :: HostName -> IO ()
updateAddress ip = do
        homeDir <- getHomeDirectory
        let fileP = homeDir ++ "/.yeelight_wrapper.config"
        writeFile fileP $ ip ++ "\n55443"


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

discoveryProtocol :: C.ByteString
discoveryProtocol = C.pack ("M-SEARCH * HTTP/1.1\r\n" ++
    "HOST: 239.255.255.250:1982\r\n" ++
    "MAN: \"ssdp:discover\"\r\n" ++
    "ST: wifi_bulb")

-- (2) Discovery functions -----------

discover :: AddrInfo -> IO Socket
discover _ = withSocketsDo $ do
        (sSock, sAddr) <- multicastSender "239.255.255.250" 1982
        sendTo sSock discoveryProtocol sAddr
        (msg, rAddr)              <- recvFrom sSock 1024
        let (SockAddrInet _ host) = rAddr
            (w1, w2, w3, w4)      = hostAddressToTuple host
            (i1, i2, i3, i4)      = (fromIntegral w1, fromIntegral w2, fromIntegral w3,
                                     fromIntegral w4)
            ip                    = show i1 ++ "." ++ show i2 ++ "." ++ show i3 ++ "." ++ show i4
        addr                      <- resolve ip "55443"
        updateAddress ip
        open addr


-- (2) Main program -----------

main :: IO ()
main = withSocketsDo $ do
    cmd <- getArgs
    (host, port) <- getAddress
    E.bracketOnError (resolve host port) (loop cmd <=< discover) (loop cmd <=< open)
 where
     loop cmd sock = do
         case cmd of
             ["off"] -> talk sock "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"off\"]}\r\n"
             ["on"]  -> talk sock "{\"id\": 1, \"method\":\"set_power\", \"params\":[\"on\"]}\r\n"
             _       -> talk sock "{\"id\": 1, \"method\":\"toggle\", \"params\":[]}\r\n"
         close sock

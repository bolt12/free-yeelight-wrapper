module Types
        ( 
            Ip,
            Command
        )
    where

import Network.Socket

type Ip = AddrInfo
type Command = String

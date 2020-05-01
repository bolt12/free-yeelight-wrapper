module Types
    ( Ip
    , Command
    , ConnectionError (..)
    )
where

import Network.Socket

type Ip = AddrInfo
type Command = String

data ConnectionError = ConnectionError
  deriving (Eq, Ord, Show)


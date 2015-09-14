{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Lens
import           Data.Aeson      (FromJSON, ToJSON, decode, toJSON)
import           Data.Aeson.Lens (key)
import           Data.Maybe      (fromJust)
import           GHC.Generics
import           Network.Wreq

type Name  = String
type MsgId = Int
data Msg   = Msg { name  :: Name
                 , msgId :: MsgId
                 , txt   :: String
                 } deriving (Generic, Show)

instance ToJSON   Msg
instance FromJSON Msg

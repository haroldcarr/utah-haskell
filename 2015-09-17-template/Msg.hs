{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Msg where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

type Name  = String
type MsgId = Int
data Msg   = Msg { name :: Name, msgId :: MsgId, txt :: String } deriving (Generic, Show)

instance ToJSON   Msg
instance FromJSON Msg

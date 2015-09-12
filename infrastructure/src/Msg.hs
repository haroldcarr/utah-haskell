{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 12 (Sat) 11:39:39 by Harold Carr.
-}
module Msg where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

type Name  = String
type MsgId = Int
data Msg   = Msg { name :: Name, msgId :: MsgId, txt :: String } deriving (Generic, Show)

instance ToJSON   Msg
instance FromJSON Msg

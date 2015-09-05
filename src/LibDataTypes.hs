{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 10:40:31 by Harold Carr.
-}
module LibDataTypes where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

type Name  = String
type MsgId = Int
data Msg   = Msg { msgId :: MsgId, txt :: String } deriving (Generic, Show)
data In    = In  { name  :: Name , msg :: Msg    } deriving (Generic, Show)
data User  = User Name MsgId                       deriving (Show)

instance ToJSON   Msg
instance FromJSON Msg
instance ToJSON   In
instance FromJSON In

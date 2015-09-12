{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Sep 05 (Sat) 11:05:00 by Harold Carr.
Last Modified : 2015 Sep 12 (Sat) 11:46:56 by Harold Carr.
-}
module Client where

import           Data.Aeson   (toJSON)
import           Msg
import           Network.Wreq

epAddr = "http://127.0.0.1:3000"

mkMsg = Msg "H"

msgs  = [ mkMsg 0 "intro"
        , mkMsg 1 "foldA1"
        , mkMsg 2 "foldA2"
        ]

test = mapM (post epAddr . toJSON) msgs

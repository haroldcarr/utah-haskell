{-# LANGUAGE OverloadedStrings #-}

module LibThreepenny
( threepennyMain
)
where

import           Control.Concurrent     (forkIO)
import           Graphics.UI.Threepenny
import           Network
import           System.IO              (hClose)

threepennyMain :: Event String -> IO ()
threepennyMain event =
    startGUI defaultConfig $ \win -> do
        bAccept <- stepper "" event
        entree <- entry bAccept
        element entree # set (attr "size") "10" # set style [("width","200px")]
        getBody win #+ [element entree]
        return ()


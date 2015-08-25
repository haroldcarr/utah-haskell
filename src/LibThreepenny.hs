{-# LANGUAGE OverloadedStrings #-}

module LibThreepenny
( threepennyMain
)
where

import           Control.Concurrent     (forkIO)
import           Graphics.UI.Threepenny
import           Network
import           System.IO              (hClose)

threepennyMain :: IO ()
threepennyMain = do
    (eAccept, hAccept) <- newEvent
    forkIO (acceptLoop hAccept 6789)
    forkIO (acceptLoop hAccept 9876)
    startGUI defaultConfig $ \win -> do
        bAccept <- stepper "" eAccept
        entree <- entry bAccept
        element entree # set (attr "size") "10" # set style [("width","200px")]
        getBody win #+ [element entree]
        return ()

acceptLoop :: (String -> IO a) -> PortNumber -> IO b
acceptLoop hAccept bindAddr = do
    s <- listenOn $ PortNumber bindAddr
    loop s
  where
    loop s = do
        (h, hostname, portNumber) <- accept s
        hClose h
        hAccept $ show bindAddr ++ " " ++ hostname ++ " " ++ show portNumber
        loop s
